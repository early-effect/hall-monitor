package hallmonitor.domain

import hexis.Answer

enum Ruling:
  case Applies
  case DoesNotApply
  case Uncertain
  case NotOnThisFace

enum Step:
  case Constraint(
      id: RuleId,
      ruling: Ruling,
      narrowed: Boolean,
      allow: List[BackendId],
      remaining: List[BackendId],
  )
  case Preference(id: RuleId, ruling: Ruling, ranked: Boolean, prefer: List[BackendId])
  case Pinned(name: String, backend: BackendId)
  case Ranked(order: List[BackendId])
end Step

final case class Trace(
    requested: Option[String],
    answers: Map[CriterionId, Answer[?]],
    steps: List[Step],
    eligible: List[BackendId],
    truncated: Boolean,
    unclassified: Boolean,
)

/** `order` is who may still be tried, best first. `rejected` is set only when `order` is empty. */
final case class RoutePlan(order: List[Backend], trace: Trace, rejected: Option[RouteError])
