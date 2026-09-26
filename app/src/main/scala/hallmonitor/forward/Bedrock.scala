package hallmonitor.forward

import hallmonitor.domain.RouteError
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, Method, Request, Url}
import zio.json.*
import zio.json.ast.Json
import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** Bedrock bearer from an AWS SSO profile. The same token `dsh-bedrock-token` mints, without that script. */
object BedrockSession:
  private val PreemptMs    = 120_000L
  private val TokenSeconds = 12 * 60 * 60

  def bearer(profile: String, region: String, client: Client): IO[RouteError, String] =
    bearer(profile, region, client, Path.of(java.lang.System.getProperty("user.home"), ".aws"))

  def bearer(profile: String, region: String, client: Client, awsHome: Path): IO[RouteError, String] =
    ZIO.clock.flatMap { clock =>
      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
        for
          role <- roleCredentials(profile, client, awsHome, now)
          stamp = DateTimeFormatter
            .ofPattern("yyyyMMdd'T'HHmmss'Z'")
            .withZone(ZoneOffset.UTC)
            .format(Instant.ofEpochMilli(now))
        yield BedrockBearer.token(role.accessKey, role.secretKey, role.sessionToken, region, TokenSeconds, stamp)
      }
    }

  private def roleCredentials(
      profile: String,
      client: Client,
      awsHome: Path,
      now: Long,
  ): IO[RouteError, RoleCredentials] =
    for
      config <- ZIO
        .attempt(AwsConfig.parse(Files.readString(awsHome.resolve("config"))))
        .mapError(_ => RouteError.Upstream(502, "could not read ~/.aws/config"))
      profileConfig <- ZIO
        .fromOption(config.profile(profile))
        .orElseFail(RouteError.Upstream(502, s"AWS profile $profile is missing"))
      sessionName <- ZIO
        .fromOption(profileConfig.get("sso_session"))
        .orElseFail(
          RouteError.Upstream(502, s"AWS profile $profile has no sso_session")
        )
      session <- ZIO
        .fromOption(config.session(sessionName))
        .orElseFail(
          RouteError.Upstream(502, s"AWS sso-session $sessionName is missing")
        )
      account <- ZIO
        .fromOption(profileConfig.get("sso_account_id"))
        .orElseFail(
          RouteError.Upstream(502, s"AWS profile $profile has no sso_account_id")
        )
      role <- ZIO
        .fromOption(profileConfig.get("sso_role_name"))
        .orElseFail(
          RouteError.Upstream(502, s"AWS profile $profile has no sso_role_name")
        )
      ssoRegion <- ZIO
        .fromOption(session.get("sso_region"))
        .orElseFail(
          RouteError.Upstream(502, s"AWS sso-session $sessionName has no sso_region")
        )
      token <- accessToken(sessionName, ssoRegion, client, awsHome, now)
      creds <- getRole(ssoRegion, account, role, token, client)
    yield creds

  private def accessToken(
      sessionName: String,
      region: String,
      client: Client,
      awsHome: Path,
      now: Long,
  ): IO[RouteError, Json.Obj] =
    val path = awsHome.resolve("sso").resolve("cache").resolve(sha1(sessionName) + ".json")
    readJson(path, s"no AWS SSO login for $sessionName; run aws sso login").flatMap { token =>
      val expires = instant(token.get("expiresAt"))
      if expires - now > PreemptMs then ZIO.succeed(token)
      else refresh(token, region, client).flatMap(updated => writeJson(path, updated).as(updated))
    }
  end accessToken

  private def refresh(token: Json.Obj, region: String, client: Client): IO[RouteError, Json.Obj] =
    val missing = List("refreshToken", "clientId", "clientSecret").filter(key => token.get(key).isEmpty)
    if missing.nonEmpty then ZIO.fail(RouteError.Upstream(502, "AWS SSO login expired; run aws sso login"))
    else
      val body = Json.Obj(
        "clientId"     -> field(token, "clientId"),
        "clientSecret" -> field(token, "clientSecret"),
        "grantType"    -> Json.Str("refresh_token"),
        "refreshToken" -> field(token, "refreshToken"),
      )
      val request = Request(
        Method.POST,
        Url.parse(s"https://oidc.$region.amazonaws.com/token"),
        Headers.empty.add(HeaderName.ContentType, "application/json").add(HeaderName.Accept, "application/json"),
        Body.json(body.toJson),
      )
      client.batched(request).mapError(_ => RouteError.Upstream(502, "AWS SSO refresh failed")).flatMap { response =>
        if response.status.code != 200 then
          ZIO.fail(RouteError.Upstream(502, "AWS SSO login expired; run aws sso login"))
        else
          response.body.asString.fromJson[Json.Obj] match
            case Left(_)      => ZIO.fail(RouteError.Upstream(502, "AWS SSO refresh failed"))
            case Right(fresh) =>
              fresh.get("accessToken") match
                case Some(access: Json.Str) =>
                  ZIO.clock.flatMap(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)).map { now =>
                    val seconds =
                      fresh.get("expiresIn").collect { case Json.Num(value) => value.longValue }.getOrElse(3600L)
                    val refreshToken = fresh.get("refreshToken").getOrElse(field(token, "refreshToken"))
                    Json.Obj(
                      token.fields.map {
                        case ("accessToken", _)  => "accessToken"  -> access
                        case ("refreshToken", _) => "refreshToken" -> refreshToken
                        case ("expiresAt", _)    =>
                          "expiresAt" -> Json.Str(Instant.ofEpochMilli(now + seconds * 1000L).toString)
                        case field => field
                      }
                    )
                  }
                case _ => ZIO.fail(RouteError.Upstream(502, "AWS SSO refresh returned no access token"))
      }
    end if
  end refresh

  private def getRole(
      region: String,
      account: String,
      role: String,
      token: Json.Obj,
      client: Client,
  ): IO[RouteError, RoleCredentials] =
    val access = token.get("accessToken").collect { case Json.Str(value) => value }.getOrElse("")
    val url    =
      s"https://portal.sso.$region.amazonaws.com/federation/credentials?account_id=${encode(account)}&role_name=${encode(role)}"
    val request = Request(
      Method.GET,
      Url.parse(url),
      Headers.empty.add(HeaderName("x-amz-sso_bearer_token"), access).add(HeaderName.Accept, "application/json"),
      Body.empty,
    )
    client.batched(request).mapError(_ => RouteError.Upstream(502, "AWS SSO role credentials failed")).flatMap {
      response =>
        if response.status.code != 200 then ZIO.fail(RouteError.Upstream(502, "AWS SSO role credentials failed"))
        else
          response.body.asString.fromJson[Json.Obj] match
            case Left(_)     => ZIO.fail(RouteError.Upstream(502, "AWS SSO role credentials failed"))
            case Right(body) =>
              body.get("roleCredentials") match
                case Some(creds: Json.Obj) =>
                  (text(creds, "accessKeyId"), text(creds, "secretAccessKey"), text(creds, "sessionToken")) match
                    case (Some(key), Some(secret), Some(session)) => ZIO.succeed(RoleCredentials(key, secret, session))
                    case _ => ZIO.fail(RouteError.Upstream(502, "AWS SSO role credentials were incomplete"))
                case _ => ZIO.fail(RouteError.Upstream(502, "AWS SSO role credentials were incomplete"))
    }
  end getRole

  private def readJson(path: Path, missing: String): IO[RouteError, Json.Obj] =
    ZIO.attempt(Files.readString(path).fromJson[Json.Obj]).mapError(_ => RouteError.Upstream(502, missing)).flatMap {
      case Left(_)     => ZIO.fail(RouteError.Upstream(502, missing))
      case Right(json) => ZIO.succeed(json)
    }

  private def writeJson(path: Path, json: Json.Obj): IO[RouteError, Unit] =
    ZIO
      .attempt(Files.writeString(path, json.toJson + "\n"))
      .mapError(_ => RouteError.Upstream(502, "could not store the refreshed AWS SSO token"))
      .unit

  private def field(obj: Json.Obj, key: String): Json =
    obj.get(key).getOrElse(Json.Str(""))

  private def text(obj: Json.Obj, key: String): Option[String] =
    obj.get(key).collect { case Json.Str(value) if value.nonEmpty => value }

  private def instant(value: Option[Json]): Long =
    value match
      case Some(Json.Str(raw)) => scala.util.Try(Instant.parse(raw).toEpochMilli).getOrElse(0L)
      case _                   => 0L

  private def sha1(value: String): String =
    val digest = MessageDigest.getInstance("SHA-1").digest(value.getBytes(StandardCharsets.UTF_8))
    digest.map("%02x".format(_)).mkString

  private def encode(value: String): String =
    BedrockBearer.encode(value)

  private final case class RoleCredentials(accessKey: String, secretKey: String, sessionToken: String)
end BedrockSession

object BedrockBearer:
  private val Host = "bedrock.amazonaws.com"

  def token(
      accessKey: String,
      secretKey: String,
      sessionToken: String,
      region: String,
      expiresSeconds: Int,
      amzDate: String,
  ): String =
    val scope      = s"${amzDate.take(8)}/$region/bedrock/aws4_request"
    val credential = s"$accessKey/$scope"
    val names      = List(
      "Action"               -> "CallWithBearerToken",
      "X-Amz-Algorithm"      -> "AWS4-HMAC-SHA256",
      "X-Amz-Credential"     -> credential,
      "X-Amz-Date"           -> amzDate,
      "X-Amz-Expires"        -> expiresSeconds.toString,
      "X-Amz-Security-Token" -> sessionToken,
      "X-Amz-SignedHeaders"  -> "host",
    )
    val canonical = names.sortBy(_._1).map((key, value) => s"${encode(key)}=${encode(value)}").mkString("&")
    val request   =
      s"POST\n/\n$canonical\nhost:$Host\n\nhost\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    val toSign    = s"AWS4-HMAC-SHA256\n$amzDate\n$scope\n${hex(sha256(request))}"
    val signature = hex(hmac(signingKey(secretKey, amzDate.take(8), region), toSign))
    val shown     = List(
      "Action"               -> "CallWithBearerToken",
      "X-Amz-Algorithm"      -> "AWS4-HMAC-SHA256",
      "X-Amz-Credential"     -> credential,
      "X-Amz-Date"           -> amzDate,
      "X-Amz-Expires"        -> expiresSeconds.toString,
      "X-Amz-SignedHeaders"  -> "host",
      "X-Amz-Security-Token" -> sessionToken,
    ).map((key, value) => s"${encode(key)}=${encode(value)}").mkString("&")
    val presigned = s"$Host/?$shown&X-Amz-Signature=$signature&Version=1"
    "bedrock-api-key-" + java.util.Base64.getEncoder.encodeToString(presigned.getBytes(StandardCharsets.UTF_8))
  end token

  def encode(value: String): String =
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    val out   = new StringBuilder
    bytes.foreach { byte =>
      val c = byte & 0xff
      if unreserved(c) then out.append(c.toChar) else out.append(f"%%$c%02X")
    }
    out.result()

  private def unreserved(c: Int): Boolean =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~'

  private def sha256(value: String): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))

  private def hmac(key: Array[Byte], data: String): Array[Byte] =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data.getBytes(StandardCharsets.UTF_8))

  private def signingKey(secret: String, date: String, region: String): Array[Byte] =
    val dateKey    = hmac(("AWS4" + secret).getBytes(StandardCharsets.UTF_8), date)
    val regionKey  = hmac(dateKey, region)
    val serviceKey = hmac(regionKey, "bedrock")
    hmac(serviceKey, "aws4_request")

  private def hex(bytes: Array[Byte]): String =
    bytes.map("%02x".format(_)).mkString
end BedrockBearer

final class AwsConfig(sections: Map[String, Map[String, String]]):
  def profile(name: String): Option[Map[String, String]] =
    sections.get(s"profile $name").orElse(if name == "default" then sections.get("default") else None)

  def session(name: String): Option[Map[String, String]] =
    sections.get(s"sso-session $name")

object AwsConfig:
  def parse(text: String): AwsConfig =
    val sections = scala.collection.mutable.LinkedHashMap.empty[String, scala.collection.mutable.Map[String, String]]
    var current  = ""
    text.split('\n').foreach { raw =>
      val line = raw.trim
      if line.isEmpty || line.startsWith("#") || line.startsWith(";") then ()
      else if line.startsWith("[") && line.endsWith("]") then current = line.drop(1).dropRight(1).trim
      else
        val eq = line.indexOf('=')
        if eq > 0 && current.nonEmpty then
          val key     = line.take(eq).trim
          val value   = line.drop(eq + 1).trim
          val section = sections.getOrElseUpdate(current, scala.collection.mutable.LinkedHashMap.empty)
          section.update(key, value)
    }
    new AwsConfig(sections.view.mapValues(_.toMap).toMap)
  end parse
end AwsConfig
