package hallmonitor

import hallmonitor.domain.*
import hallmonitor.forward.{GrokSession, TokenCache}
import heddle.*
import heddle.client.Client
import zio.test.*
import zio.{durationInt, Chunk, Ref, ZIO}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermission
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
      ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("grok-auth")))(dir => ZIO.attempt(delete(dir)).ignore)
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
    test("a command bearer is reused until it is inside a minute of expiry") {
      ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("auth-cmd")))(dir => ZIO.attempt(delete(dir)).ignore)
        .flatMap { dir =>
          val log    = dir.resolve("hits")
          val script = dir.resolve("mint.sh")
          Files.writeString(
            script,
            """#!/bin/sh
              |printf x >> "$1"
              |n=$(wc -c < "$1" | tr -d ' ')
              |printf '{"access_token":"tok-%s"}\n' "$n"
              |""".stripMargin,
          )
          Files.setPosixFilePermissions(
            script,
            java.util.Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_EXECUTE),
          )
          val backend = Backend(
            BackendId("bedrock"),
            ModelKind.Conversational,
            "https://bedrock.example/openai/v1",
            "us.xai.grok-4.6",
            BackendAuth.Command(script.toString, List(log.toString), ttlSeconds = 3600, timeoutSeconds = 30),
            Nil,
          )
          val idle = new Client:
            def batched(request: heddle.http.Request): zio.Task[Response] =
              ZIO.dieMessage("command auth does not use the http client")
          for
            cache  <- TokenCache.make(idle)
            first  <- cache.bearer(backend)
            second <- cache.bearer(backend)
            _      <- TestClock.adjust(3601.seconds)
            third  <- cache.bearer(backend)
          yield assertTrue(first == "tok-1", second == "tok-1", third == "tok-2")
        }
    },
  )

  private def delete(dir: Path): Unit =
    Files.walk(dir).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists)
end AuthRefreshSpec
