package hallmonitor

import hallmonitor.admit.*
import hallmonitor.domain.*
import hallmonitor.journal.{CallRecord, Journal, Timings}
import hexis.{Answer, Probability}
import zio.json.ast.Json
import zio.json.*
import zio.test.*
import zio.Duration

object JournalSpec extends ZIOSpecDefault:
  def spec = suite("Journal")(
    test("a record renders the trace and the attempts and not the note") {
      val backend = Backend(
        BackendId("local-strong"),
        ModelKind.Conversational,
        "http://127.0.0.1:9",
        "qwen",
        BackendAuth.Key(Secret("secret")),
        Nil,
      )
      val plan = RoutePlan(
        List(backend),
        Trace(
          None,
          Map(CriterionId("pii") -> Answer.Noul(Probability.unsafely(0.95))),
          List(
            Step.Constraint(
              RuleId("pii-lock"),
              Ruling.Applies,
              narrowed = true,
              List(BackendId("local-strong")),
              List(BackendId("local-strong")),
            )
          ),
          List(BackendId("local-strong")),
          truncated = false,
          unclassified = false,
        ),
        None,
      )
      val record = CallRecord(
        "req-1",
        ModelKind.Conversational,
        Some("127.0.0.1:8091"),
        plan,
        List(Attempt.Skipped(backend.id, Skip.Down(sinceMs = 12L))),
        None,
        Timings(Duration.fromMillis(40), Duration.fromMillis(50)),
        Some(RouteError.NoneAvailable("local-strong down")),
      )
      val rendered = Journal.render(record)
      val parsed   = rendered.fromJson[Json]
      assertTrue(
        rendered.contains(""""steps""""),
        rendered.contains(""""attempts""""),
        rendered.contains(""""classifyMs""""),
        rendered.contains(""""sinceMs""""),
        !rendered.contains(""""text""""),
        !rendered.contains(""""messages""""),
        !rendered.contains(""""state""""),
        !rendered.contains(""""apiKey""""),
        !rendered.contains("secret"),
        parsed.isRight,
      )
    }
  )
end JournalSpec
