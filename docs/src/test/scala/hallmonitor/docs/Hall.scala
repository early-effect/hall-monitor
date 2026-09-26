package hallmonitor.docs

import hallmonitor.domain.*
import hexis.{Confidence, Probability}

/** The hallway the guide keeps walking. */
object Hall:
  val localFast: Backend   = model("local-fast", ModelKind.Conversational)
  val publicFast: Backend  = model("public-fast", ModelKind.Conversational, List("grok-fast"))
  val localStrong: Backend = model("local-strong", ModelKind.Conversational)
  val publicHeavy: Backend = model("public-heavy", ModelKind.Conversational)
  val jevPublic: Backend   = model("jev-public", ModelKind.Decision)
  val jevVpc: Backend      = model("jev-vpc", ModelKind.Decision)

  val policy: Policy = Policy(
    backends = List(localFast, publicFast, localStrong, publicHeavy, jevPublic, jevVpc),
    criteria = List(
      Criterion.Noul(CriterionId("pii"), "pii", "yes", "no", ModelKind.both),
      Criterion.Score(
        CriterionId("weight"),
        "weight",
        List("trivial", "light", "moderate", "heavy"),
        Set(ModelKind.Conversational),
      ),
      Criterion.Choice(
        CriterionId("task"),
        "task",
        List("coding-light" -> "small", "coding-heavy" -> "large", "chat" -> "talk"),
        Set(ModelKind.Conversational),
      ),
    ),
    rules = List(
      Rule.Constraint(
        RuleId("pii-lock"),
        List(Predicate.NoulYesAtLeast(CriterionId("pii"), Probability.unsafely(0.40))),
        Set(BackendId("local-fast"), BackendId("local-strong"), BackendId("jev-vpc")),
      ),
      Rule.Preference(
        RuleId("light-code"),
        List(
          Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5)),
          Predicate.ScoreAtMost(CriterionId("weight"), "light", Confidence.unsafely(0.5)),
        ),
        List(BackendId("public-fast")),
      ),
      Rule.Preference(
        RuleId("heavy"),
        List(Predicate.ScoreAtLeast(CriterionId("weight"), "heavy", Confidence.unsafely(0.5))),
        List(BackendId("public-heavy")),
      ),
    ),
    defaultPrefer = List(BackendId("public-fast")),
    maxStateChars = 16000,
  )

  def chosen(
      face: ModelKind,
      answers: Map[CriterionId, hexis.Answer[?]],
      requested: Option[String] = None,
      truncated: Boolean = false,
      rules: Policy = policy,
  ): Either[RouteError, String] =
    Resolve(rules, face, answers, requested, truncated).map(_.id.value)

  def model(id: String, kind: ModelKind, aliases: List[String] = Nil): Backend =
    Backend(BackendId(id), kind, s"http://$id", id, BackendAuth.Key(Secret("secret")), aliases)
end Hall
