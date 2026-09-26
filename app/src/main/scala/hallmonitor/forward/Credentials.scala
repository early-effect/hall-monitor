package hallmonitor.forward

import hallmonitor.domain.*
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, Method, Request, Url}
import zio.json.*
import zio.json.ast.Json
import zio.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

final class TokenCache(tokens: Ref[Map[String, Minted]], discovery: Ref[Option[String]], client: Client):
  def bearer(backend: Backend): IO[RouteError, String] =
    backend.auth match
      case BackendAuth.Key(secret)      => ZIO.succeed(secret.reveal)
      case grok: BackendAuth.Grok       => GrokSession.token(grok.home, client, discovery)
      case bedrock: BackendAuth.Bedrock =>
        cached(s"bedrock ${bedrock.profile} ${bedrock.region}", 12 * 60 * 60 - 60)(
          BedrockSession.bearer(bedrock.profile, bedrock.region, client).map(token => Minted(token, 0L))
        )

  private def cached(key: String, ttlSeconds: Int)(mint: IO[RouteError, Minted]): IO[RouteError, String] =
    ZIO.clock.flatMap { clock =>
      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
        tokens.get.flatMap { table =>
          table.get(key).filter(_.expiresAtMs - now > 60_000) match
            case Some(hit) => ZIO.succeed(hit.token)
            case None      =>
              mint.flatMap { minted =>
                val expires = if minted.expiresAtMs > 0 then minted.expiresAtMs else now + ttlSeconds.toLong * 1000L
                tokens.update(_ + (key -> minted.copy(expiresAtMs = expires))).as(minted.token)
              }
        }
      }
    }
end TokenCache

object TokenCache:
  def make(client: Client): UIO[TokenCache] =
    for
      tokens    <- Ref.make(Map.empty[String, Minted])
      discovery <- Ref.make(Option.empty[String])
    yield TokenCache(tokens, discovery, client)

final case class Minted(token: String, expiresAtMs: Long)

object Home:
  def form(fields: (String, String)*): String =
    fields
      .map((key, value) =>
        URLEncoder.encode(key, StandardCharsets.UTF_8) + "=" + URLEncoder.encode(value, StandardCharsets.UTF_8)
      )
      .mkString("&")

  def expandHome(path: String): String =
    val home = java.lang.System.getProperty("user.home")
    if path == "~" then home
    else if path.startsWith("~/") then home + path.drop(1)
    else path
end Home

object GrokSession:
  private val PreemptMs = 120_000L
  private val ClientId  = "b1a00492-073a-47ea-816f-4c329264a828"

  def token(home: String, client: Client, discovery: Ref[Option[String]]): IO[RouteError, String] =
    val path = Path.of(Home.expandHome(home), "auth.json")
    ZIO.clock.flatMap { clock =>
      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
        read(path).flatMap { session =>
          if session.expiresAt - now > PreemptMs then ZIO.succeed(session.access)
          else refresh(session, client, discovery).flatMap(updated => write(path, updated).as(updated.access))
        }
      }
    }
  end token

  private def read(path: Path): IO[RouteError, Session] =
    ZIO
      .attempt(Json.decoder.decodeJson(Files.readString(path)).left.map(identity))
      .mapError(_ => RouteError.Upstream(502, s"no Grok Build login at $path; run grok login"))
      .flatMap {
        case Left(_)     => ZIO.fail(RouteError.Upstream(502, s"no Grok Build login at $path; run grok login"))
        case Right(json) =>
          json match
            case obj: Json.Obj =>
              obj.fields.collectFirst {
                case (key, value: Json.Obj)
                    if key.contains("auth.x.ai") || value.get("oidc_issuer").contains(Json.Str("https://auth.x.ai")) =>
                  session(value)
              }.flatten match
                case Some(session) => ZIO.succeed(session)
                case None => ZIO.fail(RouteError.Upstream(502, s"$path has no auth.x.ai session; run grok login"))
            case _ => ZIO.fail(RouteError.Upstream(502, s"$path has no auth.x.ai session; run grok login"))
      }

  private def session(entry: Json.Obj): Option[Session] =
    for
      access  <- entry.get("key").collect { case Json.Str(value) if value.nonEmpty => value }
      refresh <- entry.get("refresh_token").collect { case Json.Str(value) if value.nonEmpty => value }
    yield Session(
      access,
      refresh,
      expires(entry.get("expires_at")),
      entry
        .get("oidc_issuer")
        .collect { case Json.Str(value) if value.nonEmpty => value }
        .getOrElse("https://auth.x.ai"),
      entry.get("oidc_client_id").collect { case Json.Str(value) if value.nonEmpty => value }.getOrElse(ClientId),
      entry,
    )

  private def expires(value: Option[Json]): Long =
    value match
      case Some(Json.Str(raw)) =>
        scala.util
          .Try(Instant.parse(raw).toEpochMilli)
          .toOption
          .orElse(raw.toLongOption.map(epoch => if epoch > 1000000000000L then epoch else epoch * 1000L))
          .getOrElse(0L)
      case Some(Json.Num(raw)) =>
        val epoch = raw.longValue
        if epoch > 1000000000000L then epoch else epoch * 1000L
      case _ => 0L

  private def refresh(session: Session, client: Client, discovery: Ref[Option[String]]): IO[RouteError, Session] =
    endpoint(session.issuer, client, discovery).flatMap { tokenEndpoint =>
      val body = Home.form(
        "grant_type"    -> "refresh_token",
        "client_id"     -> session.clientId,
        "refresh_token" -> session.refresh,
      )
      val request = Request(
        Method.POST,
        Url.parse(tokenEndpoint),
        Headers.empty
          .add(HeaderName.Accept, "application/json")
          .add(HeaderName.ContentType, "application/x-www-form-urlencoded"),
        Body.text(body),
      )
      client.batched(request).mapError(_ => RouteError.Upstream(502, "grok session refresh failed")).flatMap {
        response =>
          if response.status.code != 200 then ZIO.fail(RouteError.Upstream(502, "grok session refresh failed"))
          else
            val text = response.body.asString
            text.fromJson[Json.Obj] match
              case Right(obj) =>
                obj.get("access_token") match
                  case Some(Json.Str(access)) if access.nonEmpty =>
                    val refresh = obj
                      .get("refresh_token")
                      .collect { case Json.Str(value) if value.nonEmpty => value }
                      .getOrElse(session.refresh)
                    val seconds =
                      obj.get("expires_in").collect { case Json.Num(value) => value.longValue }.getOrElse(3600L)
                    ZIO.clock.flatMap { clock =>
                      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).map { now =>
                        session.copy(access = access, refresh = refresh, expiresAt = now + seconds * 1000L)
                      }
                    }
                  case _ => ZIO.fail(RouteError.Upstream(502, "grok session refresh returned no access_token"))
              case Left(_) => ZIO.fail(RouteError.Upstream(502, "grok session refresh failed"))
            end match
      }
    }

  private def endpoint(issuer: String, client: Client, discovery: Ref[Option[String]]): IO[RouteError, String] =
    discovery.get.flatMap {
      case Some(cached) => ZIO.succeed(cached)
      case None         =>
        val url     = issuer.stripSuffix("/") + "/.well-known/openid-configuration"
        val request =
          Request(Method.GET, Url.parse(url), Headers.empty.add(HeaderName.Accept, "application/json"), Body.empty)
        client.batched(request).mapError(_ => RouteError.Upstream(502, "grok session discovery failed")).flatMap {
          response =>
            response.body.asString.fromJson[Json.Obj] match
              case Left(_)    => ZIO.fail(RouteError.Upstream(502, "grok session discovery failed"))
              case Right(obj) =>
                obj.get("token_endpoint") match
                  case Some(Json.Str(found)) => discovery.set(Some(found)).as(found)
                  case _                     => ZIO.fail(RouteError.Upstream(502, "grok session discovery failed"))
        }
    }

  private def write(path: Path, session: Session): IO[RouteError, Unit] =
    ZIO
      .attempt {
        val original = Files.readString(path).fromJson[Json.Obj].getOrElse(Json.Obj())
        val updated  = Json.Obj(
          original.fields.map {
            case (key, value: Json.Obj) if key.contains("auth.x.ai") =>
              key -> Json.Obj(
                value.fields.map {
                  case ("key", _)           => "key"           -> Json.Str(session.access)
                  case ("refresh_token", _) => "refresh_token" -> Json.Str(session.refresh)
                  case ("expires_at", _) => "expires_at" -> Json.Str(Instant.ofEpochMilli(session.expiresAt).toString)
                  case field             => field
                }
              )
            case field => field
          }
        )
        Files.writeString(path, updated.toJson)
        ()
      }
      .mapError(_ => RouteError.Upstream(502, "could not store the refreshed Grok Build session"))

  private final case class Session(
      access: String,
      refresh: String,
      expiresAt: Long,
      issuer: String,
      clientId: String,
      entry: Json.Obj,
  )
end GrokSession
