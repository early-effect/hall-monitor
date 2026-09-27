package hallmonitor.admit

import hallmonitor.domain.*
import heddle.client.Client
import heddle.http.header.{HeaderName, Headers}
import heddle.http.{Body, Method, Request, Url}
import zio.*

import java.net.URI

object Checks:
  val DefaultEverySeconds: Int   = 5
  val DefaultTimeoutSeconds: Int = 1

  /** The check that actually runs. An explicit disabled table wins over the loopback default. */
  def effective(backend: Backend): Option[Liveness] =
    backend.liveness match
      case Some(value) if !value.enabled     => None
      case Some(value)                       => Some(value)
      case None if loopback(backend.baseUrl) =>
        Some(Liveness(DefaultEverySeconds, DefaultTimeoutSeconds, None, enabled = true))
      case None => None

  def loopback(baseUrl: String): Boolean =
    val host =
      try Option(URI(baseUrl).getHost).getOrElse("").toLowerCase
      catch case _: IllegalArgumentException => ""
    host == "localhost" || host == "127.0.0.1" || host == "::1" || host == "0:0:0:0:0:0:0:1"

  /** One pass: check every backend whose interval is due. The caller sleeps between passes. */
  def tick(current: Ref[Loaded], pool: Pool, client: Client, due: Ref[Map[BackendId, Long]]): UIO[Unit] =
    for
      loaded   <- current.get
      now      <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
      schedule <- due.get
      ready = loaded.policy.backends.filter { backend =>
        effective(backend).isDefined && schedule.getOrElse(backend.id, 0L) <= now
      }
      _ <- ZIO.foreachParDiscard(ready) { backend =>
        check(backend, client).flatMap { up =>
          val next = now + effective(backend).fold(DefaultEverySeconds)(_.everySeconds).toLong * 1000L
          due.update(_.updated(backend.id, next)) *>
            (if up then pool.noteUp(backend.id, now) else pool.noteDown(backend.id, now, None))
        }
      }
    yield ()

  def supervise(current: Ref[Loaded], pool: Pool, client: Client): UIO[Nothing] =
    Ref.make(Map.empty[BackendId, Long]).flatMap { due =>
      (tick(current, pool, client, due) *> ZIO.sleep(1.second)).forever
    }

  private def check(backend: Backend, client: Client): UIO[Boolean] =
    val spec    = effective(backend).getOrElse(Liveness(DefaultEverySeconds, DefaultTimeoutSeconds, None, true))
    val url     = spec.path.fold(backend.baseUrl)(path => join(backend.baseUrl, path))
    val request = Request(
      Method.GET,
      Url.parse(url),
      Headers.empty.add(HeaderName.Accept, "application/json"),
      Body.empty,
    )
    client
      .batched(request)
      .timeoutFail(new java.io.IOException("liveness timed out"))(Duration.fromSeconds(spec.timeoutSeconds.toLong))
      .either
      .map {
        case Left(_)                                            => false
        case Right(_) if spec.path.isEmpty                      => true
        case Right(response) if response.status.code / 100 == 2 => true
        case Right(_)                                           => false
      }
  end check

  private def join(base: String, path: String): String =
    val suffix = if path.startsWith("/") then path else "/" + path
    base.stripSuffix("/") + suffix
end Checks
