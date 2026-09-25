package hallmonitor

import hallmonitor.config.Load
import hallmonitor.domain.Loaded
import hallmonitor.http.{Api, Gate}
import heddle.client.Client
import heddle.{BytesLength, Server}
import hexis.config.HeddleSettings
import zio.*

object Main extends ZIOAppDefault:
  def run =
    Server.sbtInterruptExit *>
      program.catchAllCause { cause =>
        if cause.isInterruptedOnly then ZIO.unit else ZIO.refailCause(cause)
      }

  private val program =
    for
      args <- getArgs
      env  <- System.envs.orDie
      path = Load.resolvePath(args.toList, env.get)
      loaded <- Load.fromFile(path, env.get).catchAll { failure =>
        ZIO.logError(failure.render) *> exit(ExitCode.failure) *> ZIO.never
      }
      current   <- Ref.make(loaded)
      client    <- ZIO.service[Client].provide(ZLayer.succeed(clientConfig(loaded)) >>> Client.layer)
      transport <- ZIO.service[hexis.Transport].provide(ZLayer.succeed(bootstrap) >>> hexis.Transport.live)
      reload = System.envs.orDie.flatMap(next => Load.fromFile(path, next.get))
      _ <- ZIO.logInfo(s"hall-monitor listening on ${loaded.listen.host}:${loaded.listen.port}")
      _ <- Server.serve(Api.routes(Gate(current, transport, client, reload)), serverConfig(loaded))
    yield ()

  private def serverConfig(loaded: Loaded): Server.Config =
    Server.Config.default.copy(
      host = loaded.listen.host,
      port = loaded.listen.port,
      maxBodyBytes = BytesLength(32L * 1024L * 1024L),
      idleTimeout = Duration.fromSeconds(loaded.upstreamIdleSeconds.toLong),
    )

  private def clientConfig(loaded: Loaded): Client.Config =
    Client.Config.default.copy(
      idleTimeout = Duration.fromSeconds(loaded.upstreamIdleSeconds.toLong),
      maxBodyBytes = BytesLength(32L * 1024L * 1024L),
    )

  private val bootstrap: hexis.Config =
    hexis.Config(
      apiKey = hexis.ApiKey.unsafely("bootstrap"),
      timeout = Duration.fromSeconds(120),
      heddle = HeddleSettings.default.copy(
        idleTimeout = Duration.fromSeconds(120),
        maxBodyBytes = 32L * 1024L * 1024L,
      ),
    )
end Main
