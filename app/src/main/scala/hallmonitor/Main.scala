package hallmonitor

import hallmonitor.admit.{Checks, GrokQuota, Pool}
import hallmonitor.config.Load
import hallmonitor.domain.Loaded
import hallmonitor.forward.TokenCache
import hallmonitor.http.{Api, Gate}
import hallmonitor.journal.Journal
import heddle.client.Client
import heddle.{BytesLength, Server}
import hexis.config.HeddleSettings
import zio.*

object Main extends ZIOAppDefault:
  /** Bound so a stop cannot wait forever on the runtime's shutdown hook. */
  override def gracefulShutdownTimeout: Duration = 10.seconds

  def run =
    stopOnIntOrTerm *>
      program.catchAllCause { cause =>
        if cause.isInterruptedOnly then ZIO.unit else ZIO.refailCause(cause)
      }

  private val program =
    for
      args <- getArgs
      env  <- System.envs.orDie
      path = Load.resolvePath(args.toList, env.get)
      loaded    <- Load.fromFile(path, env.get).tapError(failure => ZIO.logError(failure.render))
      current   <- Ref.make(loaded)
      client    <- ZIO.service[Client].provide(ZLayer.succeed(clientConfig(loaded)) >>> Client.layer)
      transport <- ZIO.service[hexis.Transport].provide(ZLayer.succeed(bootstrap) >>> hexis.Transport.live)
      reload = System.envs.orDie.flatMap(next => Load.fromFile(path, next.get))
      tokens    <- TokenCache.make(client)
      discovery <- Ref.make(Option.empty[String])
      pool      <- Pool.make(quota => GrokQuota.fetch(quota, client, discovery))
      journal   <- Journal.make
      _ <- ZIO.logInfo(s"hall-monitor listening on ${loaded.listen.host}:${loaded.listen.port} (Ctrl-C to stop)")
      _ <- ZIO.scoped {
        Checks.supervise(current, pool, client).forkScoped *>
          Server.serve(
            Api.routes(Gate(current, transport, client, tokens, reload, journal, pool)),
            serverConfig(loaded),
          )
      }
    yield ()

  /** Ctrl-C and `kill` both stop the process. sbt treats a raw SIGINT status of 130 as a crash, so exit 0. */
  private val stopOnIntOrTerm: UIO[Unit] =
    ZIO.succeed {
      val stop: sun.misc.SignalHandler = _ => java.lang.System.exit(0)
      sun.misc.Signal.handle(sun.misc.Signal("INT"), stop)
      sun.misc.Signal.handle(sun.misc.Signal("TERM"), stop)
      ()
    }

  private def serverConfig(loaded: Loaded): Server.Config =
    Server.Config.default.copy(
      host = loaded.listen.host,
      port = loaded.listen.port,
      maxBodyBytes = BytesLength(32L * 1024L * 1024L),
      idleTimeout = Duration.fromSeconds(loaded.upstreamIdleSeconds.toLong),
    )

  private def clientConfig(loaded: Loaded): Client.Config =
    Client.Config.default.copy(
      connectTimeout = connectBudget(loaded),
      idleTimeout = Duration.fromSeconds(loaded.upstreamIdleSeconds.toLong),
      maxBodyBytes = BytesLength(32L * 1024L * 1024L),
    )

  /** Connect budget is the longest backend or liveness timeout. Generation stays on `idleTimeout`. */
  private def connectBudget(loaded: Loaded): Duration =
    val connects = loaded.policy.backends.map(_.connectTimeoutSeconds)
    val checks   = loaded.policy.backends.flatMap(backend => Checks.effective(backend).map(_.timeoutSeconds))
    Duration.fromSeconds((connects ++ checks).maxOption.getOrElse(3).toLong)

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
