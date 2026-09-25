package hallmonitor

import hallmonitor.classify.{Classify, Subject}
import hallmonitor.domain.*
import hexis.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, ZIO}

object ClassifySpec extends ZIOSpecDefault:
  private val classifier = ClassifierEndpoint("http://classifier.test", "jev-latest", 10, Secret("local"))

  private val policy = Policy(
    backends = List(
      Backend(BackendId("public-fast"), ModelKind.Conversational, "http://up", "grok", Secret("k"), Nil)
    ),
    criteria = List(
      Criterion.Noul(CriterionId("pii"), "pii?", "yes", "no", ModelKind.both),
      Criterion.Score(
        CriterionId("weight"),
        "weight?",
        List("trivial", "light", "moderate", "heavy"),
        Set(ModelKind.Conversational),
      ),
      Criterion.Choice(
        CriterionId("task"),
        "task?",
        List("coding-light" -> "small", "coding-heavy" -> "large", "chat" -> "talk"),
        Set(ModelKind.Conversational),
      ),
      Criterion.Noul(CriterionId("stakes-only"), "stakes?", "yes", "no", Set(ModelKind.Decision)),
    ),
    rules = Nil,
    defaultPrefer = List(BackendId("public-fast")),
    maxStateChars = 16000,
  )

  def spec = suite("Classify")(
    test("a chat request asks the conversational criteria and not a decision-only one") {
      for
        live    <- TestTransport.make(TestTransport.Script(HttpResponse(200, Map.empty, answer)))
        reading <- Classify.run(
          policy,
          Some(classifier),
          ModelKind.Conversational,
          Subject(Some("hall-monitor"), "rename this function", List("read_file")),
          live,
        )
        seen <- ZIO.service[Chunk[HttpRequest]].provide(live.requests)
        body = seen.head.body.getOrElse("")
        json <- ZIO.fromEither(body.fromJson[Json]).mapError(new RuntimeException(_))
      yield
        val questions = json match
          case obj: Json.Obj =>
            obj.get("questions") match
              case Some(q: Json.Obj) => q.fields.map(_._1).toSet
              case _                 => Set.empty
          case _ => Set.empty
        assertTrue(
          reading.asked,
          questions == Set("pii", "weight", "task"),
          !questions.contains("stakes-only"),
          seen.head.url.contains("http://classifier.test/v1/systemone"),
          body.contains("jev-latest"),
          reading.answers
            .get(CriterionId("task"))
            .collect { case choice: Answer.Choice[?] =>
              choice.choice.toString
            }
            .contains("coding-light"),
        )
    },
    test("text past the cap is truncated and the flag is set") {
      val capped = policy.copy(maxStateChars = 20)
      val text   = "abcdefghijklmnopqrstuvwxyz"
      for
        live    <- TestTransport.make(TestTransport.Script(HttpResponse(200, Map.empty, answer)))
        reading <- Classify.run(capped, Some(classifier), ModelKind.Conversational, Subject(None, text, Nil), live)
        seen    <- ZIO.service[Chunk[HttpRequest]].provide(live.requests)
        json    <- ZIO.fromEither(seen.head.body.getOrElse("").fromJson[Json]).mapError(new RuntimeException(_))
      yield
        val sent = json match
          case obj: Json.Obj =>
            obj.get("state") match
              case Some(state: Json.Obj) =>
                state.get("text") match
                  case Some(Json.Str(value)) => value
                  case _                     => ""
              case _ => ""
          case _ => ""
        assertTrue(reading.truncated, sent.length <= 20, sent.nonEmpty)
      end for
    },
  )

  private val answer: String =
    """
      |{
      |  "model": "jev-latest",
      |  "answers": {
      |    "pii": { "type": "noul", "noul": 0.1 },
      |    "weight": { "type": "score", "score": 1.0, "confidence": 0.9, "probabilities": {}, "legend": {} },
      |    "task": {
      |      "type": "choice",
      |      "choice": "coding-light",
      |      "confidence": 0.9,
      |      "probabilities": { "coding-light": 0.9, "coding-heavy": 0.05, "chat": 0.05 }
      |    }
      |  },
      |  "usage": { "input_tokens": 1, "output_tokens": 1 }
      |}
      |""".stripMargin
end ClassifySpec
