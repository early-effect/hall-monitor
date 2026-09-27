package hallmonitor.admit

import hallmonitor.domain.*
import zio.*

final case class Mark(up: Boolean, sinceMs: Long, checkedMs: Long, untilMs: Option[Long])

final case class QuotaReading(
    used: Option[Double],
    stopAt: Double,
    resetsAt: Option[String],
    fetchedAtMs: Long,
)

final case class PoolEntry(id: BackendId, presence: String, sinceMs: Long, checkedMs: Long)

/** In-flight permits, presence, 429 cooldowns, and cached quota readings. */
final class Pool(
    inflight: Ref[Map[BackendId, Int]],
    presence: Ref[Map[BackendId, Mark]],
    cooldown: Ref[Map[BackendId, Long]],
    readings: Ref[Map[QuotaId, QuotaReading]],
    transitions: Ref[Chunk[String]],
    probe: Quota => IO[RouteError, QuotaWindow],
):
  def inFlight(id: BackendId): UIO[Int] =
    inflight.get.map(_.getOrElse(id, 0))

  def transitionsSoFar: UIO[Chunk[String]] =
    transitions.get

  /** Claims one permit. `None` when `max` is set and the count is already there. */
  def claim(id: BackendId, max: Option[Int]): UIO[Option[Int]] =
    inflight.modify { counts =>
      val held = counts.getOrElse(id, 0)
      max match
        case Some(limit) if held >= limit => (None, counts)
        case _                            => (Some(held + 1), counts.updated(id, held + 1))
    }

  def release(id: BackendId): UIO[Unit] =
    inflight.update { counts =>
      val held = counts.getOrElse(id, 0) - 1
      if held <= 0 then counts - id else counts.updated(id, held)
    }

  def noteUp(id: BackendId, nowMs: Long): UIO[Unit] =
    note(id, up = true, nowMs, None)

  def noteDown(id: BackendId, nowMs: Long, untilMs: Option[Long]): UIO[Unit] =
    note(id, up = false, nowMs, untilMs)

  def armCooldown(id: BackendId, untilMs: Long): UIO[Unit] =
    cooldown.update(_.updated(id, untilMs))

  def coolingUntil(id: BackendId, nowMs: Long): UIO[Option[Long]] =
    cooldown.get.map(_.get(id).filter(_ > nowMs))

  def downSince(id: BackendId, nowMs: Long): UIO[Option[Long]] =
    presence.get.map(_.get(id).filter(mark => Pool.isDown(mark, nowMs)).map(_.sinceMs))

  def reading(id: QuotaId): UIO[Option[QuotaReading]] =
    readings.get.map(_.get(id))

  def putReading(id: QuotaId, reading: QuotaReading): UIO[Unit] =
    readings.update(_.updated(id, reading))

  /** Refreshes each named quota whose cache is older than a minute. A failed probe keeps the previous reading. */
  def prepare(backends: List[Backend], quotas: Map[QuotaId, Quota]): UIO[Unit] =
    ZIO.foreachDiscard(backends.flatMap(_.quota).distinct) { name =>
      quotas.get(name) match
        case None        => ZIO.unit
        case Some(quota) => refresh(name, quota)
    }

  def snapshot(backends: List[Backend]): UIO[List[PoolEntry]] =
    ZIO.clock.flatMap { clock =>
      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
        presence.get.map { marks =>
          backends.map { backend =>
            marks.get(backend.id) match
              case Some(mark) if mark.up =>
                PoolEntry(backend.id, "up", mark.sinceMs, mark.checkedMs)
              case Some(mark) if Pool.isDown(mark, now) =>
                PoolEntry(backend.id, "down", mark.sinceMs, mark.checkedMs)
              case _ =>
                PoolEntry(backend.id, "unchecked", 0L, 0L)
          }
        }
      }
    }

  private def refresh(name: QuotaId, quota: Quota): UIO[Unit] =
    ZIO.clock.flatMap { clock =>
      clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).flatMap { now =>
        readings.get.flatMap { cached =>
          val fresh = cached.get(name).exists(reading => now - reading.fetchedAtMs < Pool.QuotaTtlMs)
          ZIO
            .unless(fresh) {
              probe(quota).either.flatMap {
                case Right(window) =>
                  readings.update(
                    _.updated(name, QuotaReading(Some(window.used), quota.stopAt, window.resetsAt, now))
                  )
                case Left(_) =>
                  val kept = cached.get(name) match
                    case Some(previous) => previous.copy(fetchedAtMs = now)
                    case None           => QuotaReading(None, quota.stopAt, None, now)
                  readings.update(_.updated(name, kept))
              }
            }
            .unit
        }
      }
    }

  private def note(id: BackendId, up: Boolean, nowMs: Long, untilMs: Option[Long]): UIO[Unit] =
    presence
      .modify { marks =>
        val previous = marks.get(id)
        val changed  = previous.forall(_.up != up)
        val since    = if changed then nowMs else previous.fold(nowMs)(_.sinceMs)
        val next     = Mark(up, since, nowMs, if up then None else untilMs)
        val line     = if changed then Some(s"${id.value} ${if up then "up" else "down"}") else None
        (line, marks.updated(id, next))
      }
      .flatMap {
        case None       => ZIO.unit
        case Some(line) => transitions.update(_ :+ line) *> ZIO.logInfo(line)
      }
end Pool

object Pool:
  val QuotaTtlMs: Long = 60_000L

  def isDown(mark: Mark, nowMs: Long): Boolean =
    !mark.up && mark.untilMs.forall(_ > nowMs)

  def make(probe: Quota => IO[RouteError, QuotaWindow]): UIO[Pool] =
    for
      inflight    <- Ref.make(Map.empty[BackendId, Int])
      presence    <- Ref.make(Map.empty[BackendId, Mark])
      cooldown    <- Ref.make(Map.empty[BackendId, Long])
      readings    <- Ref.make(Map.empty[QuotaId, QuotaReading])
      transitions <- Ref.make(Chunk.empty[String])
    yield Pool(inflight, presence, cooldown, readings, transitions, probe)
end Pool
