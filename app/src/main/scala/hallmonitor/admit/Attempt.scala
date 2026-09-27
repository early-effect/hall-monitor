package hallmonitor.admit

import hallmonitor.domain.*
import zio.Duration

enum Skip:
  case Saturated(inFlight: Int, max: Int)
  case OverQuota(quota: String, used: Double, stopAt: Double, resetsAt: Option[String])
  case CoolingDown(untilMs: Long)
  case Down(sinceMs: Long)

enum Fall:
  case Unreachable
  case Status(code: Int)

enum Attempt:
  case Skipped(backend: BackendId, reason: Skip)
  case Called(backend: BackendId, status: Option[Int], elapsed: Duration, fellThrough: Option[Fall])

final case class QuotaWindow(used: Double, resetsAt: Option[String])
