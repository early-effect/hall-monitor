package hallmonitor

import hallmonitor.config.{Load, LoadError}
import hallmonitor.domain.*
import hallmonitor.forward.TokenCache
import hallmonitor.http.{Api, Gate}
import heddle.*
import heddle.client.Client
import hexis.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Ref, ZIO}

import java.nio.file.{Files, Path}

object ApiSpec extends ZIOSpecDefault:
  private val fast = Backend(
    BackendId("public-fast"),
    ModelKind.Conversational,
    "https://up.test/v1",
    "grok-4-fast",
    BackendAuth.Key(Secret("fast-key")),
    Nil,
  )
  private val heavy = Backend(
    BackendId("public-heavy"),
    ModelKind.Conversational,
    "https://up.test/v1",
    "grok-4",
    BackendAuth.Key(Secret("heavy-key")),
    Nil,
  )
  private val local = Backend(
    BackendId("local-strong"),
    ModelKind.Conversational,
    "http://127.0.0.1:11434/v1",
    "qwen",
    BackendAuth.Key(Secret("local-key")),
    Nil,
  )
  private val vpc = Backend(
    BackendId("jev-vpc"),
    ModelKind.Decision,
    "https://jev.internal.example",
    "jev-latest",
    BackendAuth.Key(Secret("vpc-key")),
    Nil,
  )

  private val loaded = Loaded(
    policy = Policy(
      backends = List(fast, heavy, local, vpc),
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
          List("coding-light" -> "s", "coding-heavy" -> "l", "chat" -> "c"),
          Set(ModelKind.Conversational),
        ),
      ),
      rules = List(
        Rule.Constraint(
          RuleId("pii-lock"),
          List(Predicate.NoulYesAtLeast(CriterionId("pii"), Probability.unsafely(0.4))),
          Set(BackendId("local-strong"), BackendId("jev-vpc")),
        ),
        Rule.Preference(
          RuleId("light-code"),
          List(
            Predicate.ChoiceIs(CriterionId("task"), Set("coding-light"), Confidence.unsafely(0.5)),
            Predicate.ScoreAtMost(CriterionId("weight"), "light", Confidence.unsafely(0.5)),
          ),
          List(BackendId("public-fast")),
        ),
      ),
      defaultPrefer = List(BackendId("public-fast")),
      maxStateChars = 16000,
    ),
    apiKey = Secret("hm-key"),
    classifier = Some(ClassifierEndpoint("http://classifier.test", "jev-latest", 10, Secret("local"))),
    listen = Listen("127.0.0.1", 8080),
    upstreamIdleSeconds = 30,
  )

  def spec = suite("Api")(
    test("a missing bearer is rejected") {
      for
        (gate, _) <- harness(answers(0.1), _ => Response.json("""{"ok":true}"""))
        response  <- Api.routes(gate)(
          Request.post("/v1/chat/completions", Body.json(chat("hall-monitor", stream = false)))
        )
      yield assertTrue(response.status == Status.Unauthorized)
    },
    test("a light chat is rewritten onto the fast backend and keeps extra fields") {
      for
        (gate, seen) <- harness(answers(0.1), _ => Response.json("""{"id":"c"}"""))
        response     <- Api.routes(gate)(
          authed(Request.post("/v1/chat/completions", Body.json(chat("hall-monitor", stream = false))))
        )
        forwarded <- seen.get
        raw       <- forwarded.head.body.utf8
        parsed = raw.fromJson[Json]
      yield
        val fields = parsed.toOption.collect { case obj: Json.Obj => obj }
        assertTrue(
          response.status == Status.Ok,
          forwarded.head.header(HeaderName.Authorization).contains("Bearer fast-key"),
          forwarded.head.url.render.contains("/v1/chat/completions"),
          fields.flatMap(_.get("model")).contains(Json.Str("grok-4-fast")),
          fields.flatMap(_.get("temperature")).isDefined,
        )
    },
    test("stream true returns the upstream event stream bytes") {
      val sse = "data: hi\n\n"
      for
        (gate, _) <- harness(answers(0.1), _ => Response(Status.Ok).withBody(Body.text(sse, MediaType.EventStream)))
        response  <- Api.routes(gate)(
          authed(Request.post("/v1/chat/completions", Body.json(chat("hall-monitor", stream = true))))
        )
        text <- response.body.utf8
      yield assertTrue(
        response.header(HeaderName.ContentType).exists(_.contains("text/event-stream")),
        text == sse,
      )
      end for
    },
    test("a PII script refuses a public heavy model and does not call upstream") {
      for
        (gate, seen) <- harness(answers(0.95), _ => Response.json("""{"ok":true}"""))
        response     <- Api.routes(gate)(
          authed(Request.post("/v1/chat/completions", Body.json(chat("public-heavy", stream = false))))
        )
        forwarded <- seen.get
      yield assertTrue(response.status == Status.Forbidden, forwarded.isEmpty)
    },
    test("a systemone call is forwarded to the decision backend with the upstream model") {
      for
        (gate, seen) <- harness(piiOnly(0.95), _ => Response.json("""{"ok":true}"""))
        response     <- Api.routes(gate)(authed(Request.post("/jev/v1/systemone", Body.json(systemOne))))
        forwarded    <- seen.get
        raw          <- forwarded.head.body.utf8
      yield assertTrue(
        response.status == Status.Ok,
        forwarded.head.url.render.contains("/v1/systemone"),
        forwarded.head.header(HeaderName.Authorization).contains("Bearer vpc-key"),
        raw.contains("jev-latest"),
        raw.contains("department"),
      )
    },
    test("the two model lists have different shapes and omit the classifier") {
      for
        (gate, _)    <- harness(answers(0.1), _ => Response.json("{}"))
        chat         <- Api.routes(gate)(authed(Request.get("/v1/models")))
        decision     <- Api.routes(gate)(authed(Request.get("/jev/v1/models")))
        chatBody     <- chat.body.utf8
        decisionBody <- decision.body.utf8
      yield assertTrue(
        chatBody.contains("hall-monitor"),
        chatBody.contains("public-fast"),
        !chatBody.contains("classifier.test"),
        decisionBody.contains("jev-vpc"),
        !decisionBody.contains("public-fast"),
        !decisionBody.contains("classifier.test"),
      )
    },
    test("a bad reload keeps the previous policy") {
      ZIO
        .acquireRelease(ZIO.attempt(Files.createTempDirectory("hall-monitor")))(dir => ZIO.attempt(delete(dir)).ignore)
        .flatMap { dir =>
          val path                          = dir.resolve("hall-monitor.toml")
          val env: String => Option[String] = Map("HALL_MONITOR_API_KEY" -> "hm-key", "XAI_API_KEY" -> "fast-key").get
          for
            _       <- ZIO.attempt(Files.writeString(path, reloadToml))
            initial <- Load.fromFile(path, env)
            current <- Ref.make(initial)
            live    <- TestTransport.make(TestTransport.Script(HttpResponse(200, Map.empty, answers(0.1))))
            seen    <- Ref.make(Chunk.empty[Request])
            client  <- recording(seen, _ => Response.json("""{"ok":true}"""))
            tokens  <- TokenCache.make(client)
            gate = Gate(current, live, client, tokens, Load.fromFile(path, env))
            _        <- ZIO.attempt(Files.writeString(path, "[["))
            denied   <- Api.routes(gate)(authed(Request.post("/admin/reload", Body.empty)))
            response <- Api.routes(gate)(
              authed(Request.post("/v1/chat/completions", Body.json(chat("hall-monitor", stream = false))))
            )
            forwarded <- seen.get
          yield assertTrue(
            denied.status == Status.UnprocessableContent,
            response.status == Status.Ok,
            forwarded.nonEmpty,
          )
          end for
        }
    },
  )

  private def harness(script: String, respond: Request => Response) =
    for
      current <- Ref.make(loaded)
      live    <- TestTransport.make(TestTransport.Script(HttpResponse(200, Map.empty, script)))
      seen    <- Ref.make(Chunk.empty[Request])
      client  <- recording(seen, respond)
      tokens  <- TokenCache.make(client)
    yield (Gate(current, live, client, tokens, ZIO.fail(LoadError.Parse("unused"))), seen)

  private def recording(seen: Ref[Chunk[Request]], respond: Request => Response) =
    val routes = Routes.fromHandler(Handler.fromFunctionZIO { request =>
      seen.update(_ :+ request).as(respond(request))
    })
    ZIO.service[Client].provide(Client.inMemory(routes))

  private def authed(request: Request): Request =
    request.addHeader(HeaderName.Authorization, "Bearer hm-key")

  private def chat(model: String, stream: Boolean): String =
    s"""{"model":"$model","temperature":0.2,"stream":$stream,"messages":[{"role":"user","content":"rename this function"}]}"""

  private val systemOne: String =
    """{"model":"hall-monitor","state":"classify this","questions":{"department":{"type":"noul","instructions":"billing?"}}}"""

  private def answers(pii: Double): String =
    s"""
       |{
       |  "model": "jev-latest",
       |  "answers": {
       |    "pii": { "type": "noul", "noul": $pii },
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

  private def piiOnly(pii: Double): String =
    s"""
       |{
       |  "model": "jev-latest",
       |  "answers": { "pii": { "type": "noul", "noul": $pii } },
       |  "usage": { "input_tokens": 1, "output_tokens": 1 }
       |}
       |""".stripMargin

  private val reloadToml: String =
    """
      |apiKeyEnv = "HALL_MONITOR_API_KEY"
      |maxStateChars = 16000
      |upstreamIdleSeconds = 30
      |defaultPrefer = ["public-fast"]
      |[listen]
      |host = "127.0.0.1"
      |port = 8080
      |[classifier]
      |baseUrl = "http://classifier.test"
      |model = "jev-latest"
      |timeoutSeconds = 10
      |apiKey = "local"
      |[[backends]]
      |id = "public-fast"
      |kind = "conversational"
      |baseUrl = "https://up.test/v1"
      |upstreamModel = "grok-4-fast"
      |apiKeyEnv = "XAI_API_KEY"
      |[[criteria]]
      |id = "pii"
      |kind = "noul"
      |instructions = "pii"
      |yes = "yes"
      |no = "no"
      |""".stripMargin

  private def delete(path: Path): Unit =
    if Files.isDirectory(path) then
      val children = Files.list(path)
      try children.forEach(child => delete(child))
      finally children.close()
    Files.deleteIfExists(path)
end ApiSpec
