package hallmonitor.docs

import hallmonitor.domain.*
import hexis.{Answer, Confidence, Probability}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object HouseRules extends DocSpecSuite:
  def doc = page("House rules")(
    md"""
The questions page writes what the classifier is asked. A rule names one of
those questions and says what the answer may do.

A constraint is a lock. A preference is a strong opinion about who is pleasant
to work with. The lock wins the argument, including the ones the opinion feels
emotional about.

```toml
[[rules]]
id = "pii-lock"
type = "constraint"
allow = ["local-fast", "local-strong", "jev-vpc"]

[[rules.when]]
criterion = "pii"
yesAtLeast = 0.4

[[rules]]
id = "light-code"
type = "preference"
prefer = ["public-fast"]

[[rules.when]]
criterion = "task"
choice = ["coding-light"]
minConfidence = 0.5

[[rules.when]]
criterion = "weight"
atMost = "light"
minConfidence = 0.5
```

A lock clears the room down to the people on its list. Two locks leave the
people who appear on both. A preference then sorts whoever is still standing
there, clipboard order and all. "Light coding likes the fast public model" is a
lovely thought. A sensitive note stays with the models the lock named.

A choice or a score under `minConfidence` (the default is `0.5`) is a shrug. A
lock hears a shrug and stays shut. A preference hears a shrug and keeps quiet.
Yes or no is the probability itself: `yesAtLeast = 0.4` means "maybe" still
clicks the lock. Pick the number for the caution you actually want, then live
with it.

Send the model name `hall-monitor` when you want the monitor to choose. Send a
real name from the board when you want that model. You get them when the locks
still have them in the room.
""",
    exampleValue {
      Hall.chosen(
        ModelKind.Conversational,
        Map(
          noul("pii", 0.1),
          choice("task", "coding-light", 0.9),
          score("weight", 1.0, 0.9),
        ),
      )
    }.assert(name => assertTrue(name == Right("public-fast"))),
    exampleValue {
      Hall.chosen(
        ModelKind.Conversational,
        Map(
          noul("pii", 0.95),
          choice("task", "coding-heavy", 0.9),
          score("weight", 3.0, 0.9),
        ),
      )
    }.assert(name => assertTrue(name == Right("local-fast"))),
    exampleValue {
      Hall.chosen(ModelKind.Conversational, Map(noul("pii", 0.95)), Some("public-heavy"))
    }.assert {
      case Left(RouteError.ModelNotAllowed("public-heavy", _)) => assertTrue(true)
      case other                                               => assertTrue(other.toString.isEmpty)
    },
    exampleValue {
      Hall.chosen(ModelKind.Decision, Map(noul("pii", 0.95)))
    }.assert(name => assertTrue(name == Right("jev-vpc"))),
    exampleValue {
      val shrug  = Map[CriterionId, Answer[?]](choice("task", "coding-light", 0.2))
      val locked = Hall.policy.copy(
        rules = List(
          Rule.Constraint(
            RuleId("maybe"),
            List(Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5))),
            Set(BackendId("local-strong")),
          )
        )
      )
      Hall.chosen(ModelKind.Conversational, shrug, rules = locked)
    }.assert(name => assertTrue(name == Right("local-strong"))),
  )

  private def noul(id: String, p: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Noul(Probability.unsafely(p))

  private def choice(id: String, label: String, c: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Choice(label, Map.empty, Confidence.unsafely(c))

  private def score(id: String, value: Double, c: Double): (CriterionId, Answer[?]) =
    CriterionId(id) -> Answer.Score(value, Map.empty, Map.empty, Confidence.unsafely(c), Seq(0, 1, 2, 3))
end HouseRules
