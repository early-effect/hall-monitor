package hallmonitor

import hallmonitor.domain.*
import hexis.{Answer, Confidence, Probability}
import zio.test.*

object ResolveSpec extends ZIOSpecDefault:
  private val localFast   = backend("local-fast", ModelKind.Conversational)
  private val publicFast  = backend("public-fast", ModelKind.Conversational, aliases = List("grok-fast"))
  private val localStrong = backend("local-strong", ModelKind.Conversational)
  private val publicHeavy = backend("public-heavy", ModelKind.Conversational)
  private val jevPublic   = backend("jev-public", ModelKind.Decision)
  private val jevVpc      = backend("jev-vpc", ModelKind.Decision)

  private lazy val policy = Policy(
    backends = List(localFast, publicFast, localStrong, publicHeavy, jevPublic, jevVpc),
    criteria = List(pii, weight, task),
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

  def spec = suite("Resolve")(
    test("light coding with no PII prefers the fast public model") {
      val answers = Map[CriterionId, Answer[?]](
        noul("pii", 0.1),
        choice("task", "coding-light", 0.9),
        score("weight", 1.0, 0.9),
      )
      assertTrue(chosen(policy, ModelKind.Conversational, answers, None) == Right("public-fast"))
    },
    test("PII keeps the call on the allowed models and a heavy preference does not widen that set") {
      val answers = Map[CriterionId, Answer[?]](
        noul("pii", 0.95),
        choice("task", "coding-heavy", 0.9),
        score("weight", 3.0, 0.9),
      )
      assertTrue(chosen(policy, ModelKind.Conversational, answers, None) == Right("local-fast"))
    },
    test("a named model outside the allowed set is rejected") {
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.95), score("weight", 3.0, 0.9))
      assertTrue:
        Resolve(policy, ModelKind.Conversational, answers, Some("public-heavy"), truncated = false) match
          case Left(RouteError.ModelNotAllowed("public-heavy", allowed)) =>
            !allowed.map(_.value).contains("public-heavy")
          case _ => false
    },
    test("a decision call with PII goes to the allowed decision model") {
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.95))
      assertTrue(chosen(policy, ModelKind.Decision, answers, None) == Right("jev-vpc"))
    },
    test("constraints intersect") {
      val narrow = policy.copy(
        rules = List(
          Rule.Constraint(RuleId("a"), Nil, Set(BackendId("local-fast"), BackendId("local-strong"))),
          Rule.Constraint(RuleId("b"), Nil, Set(BackendId("local-strong"), BackendId("public-heavy"))),
        )
      )
      assertTrue(chosen(narrow, ModelKind.Conversational, Map.empty, None) == Right("local-strong"))
    },
    test("an uncertain choice still applies a constraint") {
      val narrow = policy.copy(
        rules = List(
          Rule.Constraint(
            RuleId("maybe"),
            List(Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5))),
            Set(BackendId("local-strong")),
          )
        ),
        defaultPrefer = List(BackendId("public-fast")),
      )
      val answers = Map[CriterionId, Answer[?]](choice("task", "coding-light", 0.2))
      assertTrue(chosen(narrow, ModelKind.Conversational, answers, None) == Right("local-strong"))
    },
    test("an uncertain choice does not rank a preference") {
      val ranked = policy.copy(
        rules = List(
          Rule.Preference(
            RuleId("maybe"),
            List(Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5))),
            List(BackendId("public-fast")),
          )
        ),
        defaultPrefer = List(BackendId("local-strong")),
      )
      val answers = Map[CriterionId, Answer[?]](choice("task", "coding-light", 0.2))
      assertTrue(chosen(ranked, ModelKind.Conversational, answers, None) == Right("local-strong"))
    },
    test("a definitely false predicate cancels an uncertain sibling") {
      val mixed = policy.copy(
        rules = List(
          Rule.Constraint(
            RuleId("mixed"),
            List(
              Predicate.NoulYesAtLeast(CriterionId("pii"), Probability.unsafely(0.4)),
              Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5)),
            ),
            Set(BackendId("local-strong")),
          )
        )
      )
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.0), choice("task", "coding-light", 0.2))
      assertTrue(chosen(mixed, ModelKind.Conversational, answers, None) == Right("public-fast"))
    },
    test("truncation makes a Noul constraint apply") {
      val locked  = policy.copy(rules = List(policy.rules.head))
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.0))
      assertTrue(chosen(locked, ModelKind.Conversational, answers, None, truncated = true) == Right("local-fast"))
    },
    test("hall-monitor does not bypass a constraint") {
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.95))
      assertTrue(chosen(policy, ModelKind.Conversational, answers, Some("hall-monitor")) == Right("local-fast"))
    },
    test("a named model inside the allowed set wins over preference order") {
      val answers = Map[CriterionId, Answer[?]](noul("pii", 0.95))
      assertTrue(chosen(policy, ModelKind.Conversational, answers, Some("local-strong")) == Right("local-strong"))
    },
    test("decision backends are invisible on the chat face and the reverse") {
      val open = policy.copy(rules = Nil, defaultPrefer = List(BackendId("jev-vpc"), BackendId("public-fast")))
      assertTrue(
        chosen(open, ModelKind.Conversational, Map.empty, None) == Right("public-fast"),
        chosen(open, ModelKind.Decision, Map.empty, None) == Right("jev-vpc"),
      )
    },
    test("with no criteria the default preference is used") {
      val open = policy.copy(criteria = Nil, rules = Nil)
      assertTrue(chosen(open, ModelKind.Conversational, Map.empty, None) == Right("public-fast"))
    },
    test("an unknown name is rejected") {
      assertTrue(
        Resolve(policy, ModelKind.Conversational, Map.empty, Some("nope"), truncated = false) ==
          Left(RouteError.UnknownModel("nope"))
      )
    },
    test("an alias selects that backend") {
      val open = policy.copy(rules = Nil)
      assertTrue(chosen(open, ModelKind.Conversational, Map.empty, Some("grok-fast")) == Right("public-fast"))
    },
  )

  private def chosen(
      policy: Policy,
      face: ModelKind,
      answers: Map[CriterionId, Answer[?]],
      requested: Option[String],
      truncated: Boolean = false,
  ): Either[RouteError, String] =
    Resolve(policy, face, answers, requested, truncated).map(_.id.value)

  private def backend(id: String, kind: ModelKind, aliases: List[String] = Nil): Backend =
    Backend(BackendId(id), kind, s"http://$id", id, BackendAuth.Key(Secret("secret")), aliases)

  private val pii    = Criterion.Noul(CriterionId("pii"), "pii", "yes", "no", ModelKind.both)
  private val weight =
    Criterion.Score(
      CriterionId("weight"),
      "weight",
      List("trivial", "light", "moderate", "heavy"),
      Set(ModelKind.Conversational),
    )
  private val task = Criterion.Choice(
    CriterionId("task"),
    "task",
    List("coding-light" -> "small", "coding-heavy" -> "large", "chat" -> "talk"),
    Set(ModelKind.Conversational),
  )

  private def noul(id: String, probability: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Noul(Probability.unsafely(probability))

  private def choice(id: String, label: String, confidence: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Choice(label, Map.empty, Confidence.unsafely(confidence))

  private def score(id: String, value: Double, confidence: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Score(value, Map.empty, Map.empty, Confidence.unsafely(confidence), Seq(0, 1, 2, 3))
end ResolveSpec
