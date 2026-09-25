package hallmonitor.domain

import hexis.{Confidence, Probability}

enum ModelKind:
  case Conversational
  case Decision

object ModelKind:
  val both: Set[ModelKind] = Set(Conversational, Decision)

  def parse(raw: String): Option[ModelKind] =
    raw match
      case "conversational" => Some(Conversational)
      case "decision"       => Some(Decision)
      case _                => None

final case class Backend(
    id: BackendId,
    kind: ModelKind,
    baseUrl: String,
    upstreamModel: String,
    apiKey: Secret,
    aliases: List[String],
)

enum Criterion:
  def id: CriterionId
  def faces: Set[ModelKind]

  case Noul(
      id: CriterionId,
      instructions: String,
      yes: String,
      no: String,
      faces: Set[ModelKind],
  )
  case Choice(
      id: CriterionId,
      instructions: String,
      options: List[(String, String)],
      faces: Set[ModelKind],
  )
  case Score(
      id: CriterionId,
      instructions: String,
      levels: List[String],
      faces: Set[ModelKind],
  )
end Criterion

enum Predicate:
  def criterion: CriterionId

  case NoulYesAtLeast(criterion: CriterionId, atLeast: Probability)
  case ChoiceIs(criterion: CriterionId, options: Set[String], minConfidence: Confidence)
  case ScoreAtLeast(criterion: CriterionId, level: String, minConfidence: Confidence)
  case ScoreAtMost(criterion: CriterionId, level: String, minConfidence: Confidence)

enum Rule:
  def id: RuleId
  def when: List[Predicate]

  case Constraint(id: RuleId, when: List[Predicate], allow: Set[BackendId])
  case Preference(id: RuleId, when: List[Predicate], prefer: List[BackendId])

final case class Policy(
    backends: List[Backend],
    criteria: List[Criterion],
    rules: List[Rule],
    defaultPrefer: List[BackendId],
    maxStateChars: Int,
)

final case class Listen(host: String, port: Int)

final case class ClassifierEndpoint(
    baseUrl: String,
    model: String,
    timeoutSeconds: Int,
    apiKey: Secret,
)

final case class Loaded(
    policy: Policy,
    apiKey: Secret,
    classifier: Option[ClassifierEndpoint],
    listen: Listen,
    upstreamIdleSeconds: Int,
)
