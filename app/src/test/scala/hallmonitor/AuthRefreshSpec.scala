package hallmonitor

import hallmonitor.domain.*
import hallmonitor.forward.{BedrockSession, GrokSession}
import heddle.*
import heddle.client.Client
import zio.test.*
import zio.{Chunk, Ref, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object AuthRefreshSpec extends ZIOSpecDefault:
  def spec = suite("auth refresh")(
    test("an expired Grok Build session is refreshed and written back") {
      val routes = Routes(
        Method.GET / ".well-known" / "openid-configuration" ->
          Handler.fromFunctionZIO(_ =>
            ZIO.succeed(Response.json("""{"token_endpoint":"https://auth.x.ai/oauth/token"}"""))
          ),
        Method.POST / "oauth" / "token" ->
          Handler.fromFunctionZIO(_ =>
            ZIO.succeed(Response.json("""{"access_token":"fresh","refresh_token":"rt2","expires_in":4000}"""))
          ),
      )
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("grok-auth")))(dir => ZIO.attempt(delete(dir)).ignore)
        .flatMap { dir =>
          val auth = dir.resolve("auth.json")
          Files.writeString(
            auth,
            """{"https://auth.x.ai::client":{"key":"old","refresh_token":"rt","expires_at":"1970-01-01T00:00:00Z","oidc_issuer":"https://auth.x.ai","oidc_client_id":"client"}}""",
          )
          for
            discovery <- Ref.make(Option.empty[String])
            token     <- ZIO
              .serviceWithZIO[Client](client => GrokSession.token(dir.toString, client, discovery))
              .provide(Client.inMemory(routes))
            saved = Files.readString(auth)
          yield assertTrue(token == "fresh", saved.contains("fresh"), saved.contains("rt2"))
        }
    },
    test("an expired AWS SSO token is refreshed and used to mint a Bedrock bearer") {
      val routes = Routes(
        Method.POST / "token" -> Handler.fromFunctionZIO(_ =>
          ZIO.succeed(Response.json("""{"accessToken":"sso-fresh","expiresIn":28800,"refreshToken":"rt2"}"""))
        ),
        Method.GET / "federation" / "credentials" -> Handler.fromFunctionZIO(_ =>
          ZIO.succeed(
            Response.json(
              """{"roleCredentials":{"accessKeyId":"AKIDEXAMPLE","secretAccessKey":"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY","sessionToken":"AQoEXAMPLE","expiration":1}}"""
            )
          )
        ),
      )
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("aws-sso")))(dir => ZIO.attempt(delete(dir)).ignore)
        .flatMap { dir =>
          val cache = dir.resolve("sso").resolve("cache")
          Files.createDirectories(cache)
          Files.writeString(
            dir.resolve("config"),
            """[profile us-dev]
              |sso_session = us
              |sso_account_id = 111122223333
              |sso_role_name = Bedrock
              |[sso-session us]
              |sso_start_url = https://example.awsapps.com/start
              |sso_region = us-west-2
              |""".stripMargin,
          )
          val name = java.security.MessageDigest
            .getInstance("SHA-1")
            .digest("us".getBytes(java.nio.charset.StandardCharsets.UTF_8))
            .map("%02x".format(_))
            .mkString
          Files.writeString(
            cache.resolve(name + ".json"),
            """{"accessToken":"old","expiresAt":"1970-01-01T00:00:00Z","refreshToken":"rt","clientId":"cid","clientSecret":"sec","region":"us-west-2","startUrl":"https://example.awsapps.com/start"}""",
          )
          for
            token <- ZIO
              .serviceWithZIO[Client](client => BedrockSession.bearer("us-dev", "us-west-2", client, dir))
              .provide(Client.inMemory(routes))
            saved = Files.readString(cache.resolve(name + ".json"))
          yield assertTrue(token.startsWith("bedrock-api-key-"), saved.contains("sso-fresh"), saved.contains("rt2"))
        }
    },
  )

  private def delete(dir: Path): Unit =
    Files.walk(dir).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists)
end AuthRefreshSpec
