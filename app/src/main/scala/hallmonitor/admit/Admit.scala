package hallmonitor.admit

import hallmonitor.domain.*
import heddle.http.Response
import heddle.http.header.HeaderName
import zio.*

final case class Admission(
    response: Option[Response],
    attempts: List[Attempt],
    served: Option[Backend],
    failure: Option[RouteError],
)

object Admit:
  private val FallStatuses: Set[Int] = Set(429, 502, 503, 504)
  private val CooldownMs: Long       = 30_000L

  def walk(
      plan: RoutePlan,
      pool: Pool,
      quotas: Map[QuotaId, Quota],
      downForSeconds: Int,
      send: Backend => IO[RouteError, Response],
  ): UIO[Admission] =
    pool.prepare(plan.order, quotas) *>
      ZIO.clock.flatMap { clock =>
        clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
          go(plan.order, Nil, pool, downForSeconds, send, now)
        }
      }

  private def go(
      rest: List[Backend],
      acc: List[Attempt],
      pool: Pool,
      downForSeconds: Int,
      send: Backend => IO[RouteError, Response],
      nowMs: Long,
  ): UIO[Admission] =
    rest match
      case Nil =>
        ZIO.succeed(Admission(None, acc, None, Some(RouteError.NoneAvailable(describe(acc)))))
      case backend :: tail =>
        skip(backend, pool, nowMs).flatMap {
          case Some(reason) =>
            go(tail, acc :+ Attempt.Skipped(backend.id, reason), pool, downForSeconds, send, nowMs)
          case None =>
            attempt(backend, pool, downForSeconds, send, nowMs).flatMap {
              case Tried.Next(attempt) =>
                go(tail, acc :+ attempt, pool, downForSeconds, send, nowMs)
              case Tried.Stop(attempt, response, failure) =>
                val served = if failure.isEmpty then Some(backend) else None
                ZIO.succeed(Admission(response, acc :+ attempt, served, failure))
            }
        }

  private def skip(backend: Backend, pool: Pool, nowMs: Long): UIO[Option[Skip]] =
    pool.coolingUntil(backend.id, nowMs).flatMap {
      case Some(until) => ZIO.succeed(Some(Skip.CoolingDown(until)))
      case None        =>
        pool.downSince(backend.id, nowMs).flatMap {
          case Some(since) => ZIO.succeed(Some(Skip.Down(since)))
          case None        =>
            backend.quota match
              case None       => ZIO.succeed(None)
              case Some(name) =>
                pool.reading(name).map {
                  case Some(reading) =>
                    reading.used.filter(_ >= reading.stopAt).map { used =>
                      Skip.OverQuota(name.value, used, reading.stopAt, reading.resetsAt)
                    }
                  case None => None
                }
        }
    }

  private def attempt(
      backend: Backend,
      pool: Pool,
      downForSeconds: Int,
      send: Backend => IO[RouteError, Response],
      nowMs: Long,
  ): UIO[Tried] =
    pool.claim(backend.id, backend.maxInFlight).flatMap {
      case None =>
        pool.inFlight(backend.id).map { held =>
          val max = backend.maxInFlight.getOrElse(held)
          Tried.Next(Attempt.Skipped(backend.id, Skip.Saturated(held, max)))
        }
      case Some(_) =>
        timed(send(backend).either)
          .ensuring(pool.release(backend.id))
          .flatMap { (elapsed, result) =>
            settle(backend, pool, downForSeconds, nowMs, elapsed, result)
          }
    }

  private def settle(
      backend: Backend,
      pool: Pool,
      downForSeconds: Int,
      nowMs: Long,
      elapsed: Duration,
      result: Either[RouteError, Response],
  ): UIO[Tried] =
    result match
      case Left(RouteError.Unreachable(_)) =>
        val until = if Checks.effective(backend).isDefined then None else Some(nowMs + downForSeconds.toLong * 1000L)
        pool.noteDown(backend.id, nowMs, until).as(Tried.Next(called(backend, None, elapsed, Some(Fall.Unreachable))))
      case Left(other) =>
        ZIO.succeed(Tried.Stop(called(backend, None, elapsed, None), None, Some(other)))
      case Right(response) if FallStatuses.contains(response.status.code) =>
        val code = response.status.code
        val arm  =
          if code == 429 then pool.armCooldown(backend.id, retryUntil(response, nowMs))
          else ZIO.unit
        arm.as(Tried.Next(called(backend, Some(code), elapsed, Some(Fall.Status(code)))))
      case Right(response) =>
        pool
          .noteUp(backend.id, nowMs)
          .as(Tried.Stop(called(backend, Some(response.status.code), elapsed, None), Some(response), None))

  private def called(
      backend: Backend,
      status: Option[Int],
      elapsed: Duration,
      fellThrough: Option[Fall],
  ): Attempt =
    Attempt.Called(backend.id, status, elapsed, fellThrough)

  private def timed[A](effect: UIO[A]): UIO[(Duration, A)] =
    for
      start  <- Clock.nanoTime
      value  <- effect
      finish <- Clock.nanoTime
    yield (Duration.fromNanos(finish - start), value)

  private def retryUntil(response: Response, nowMs: Long): Long =
    response.header(HeaderName.RetryAfter).flatMap(_.toLongOption) match
      case Some(seconds) if seconds > 0 => nowMs + seconds * 1000L
      case _                            => nowMs + CooldownMs

  private def describe(attempts: List[Attempt]): String =
    if attempts.isEmpty then "no backend accepted the call"
    else
      val parts = attempts.map {
        case Attempt.Skipped(id, Skip.Saturated(held, max)) =>
          s"${id.value} saturated $held/$max"
        case Attempt.Skipped(id, Skip.OverQuota(quota, used, stopAt, _)) =>
          s"${id.value} over $quota ${"%.2f".format(used)}/${"%.2f".format(stopAt)}"
        case Attempt.Skipped(id, Skip.CoolingDown(_)) =>
          s"${id.value} cooling down"
        case Attempt.Skipped(id, Skip.Down(_)) =>
          s"${id.value} down"
        case Attempt.Called(id, _, _, Some(Fall.Unreachable)) =>
          s"${id.value} unreachable"
        case Attempt.Called(id, Some(code), _, Some(Fall.Status(_))) =>
          s"${id.value} status $code"
        case Attempt.Called(id, _, _, _) =>
          s"${id.value} failed"
      }
      "no backend accepted the call: " + parts.mkString(", ")

  private enum Tried:
    case Next(attempt: Attempt)
    case Stop(attempt: Attempt, response: Option[Response], failure: Option[RouteError])
end Admit
