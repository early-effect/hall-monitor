package hallmonitor

import hallmonitor.config.{ConfigError, Load}
import hallmonitor.domain.ModelKind
import zio.test.*
import zio.{ZIO, *}

import java.nio.file.{Files, Path}

object LoadSpec extends ZIOSpecDefault:
  private val sampleEnv: String => Option[String] = Map(
    "HALL_MONITOR_API_KEY" -> "hm-secret",
    "XAI_API_KEY"          -> "xai-secret",
    "JEV_API_KEY"          -> "jev-secret",
    "JEV_VPC_API_KEY"      -> "vpc-secret",
  ).get

  def spec = suite("Load")(
    test("the sample file loads and the classifier is not a backend") {
      for
        text   <- ZIO.attempt(exampleText)
        loaded <- Load.fromString(text, sampleEnv)
      yield assertTrue(
        loaded.classifier.map(_.baseUrl).contains("http://127.0.0.1:8091"),
        loaded.classifier.map(_.model).contains("jev-latest"),
        loaded.classifier.map(_.apiKey.reveal).contains("local"),
        loaded.policy.backends.find(_.id.value == "public-fast").map(_.apiKey.reveal).contains("xai-secret"),
        loaded.policy.backends.find(_.id.value == "local-strong").map(_.apiKey.reveal).contains("ollama"),
        !loaded.policy.backends.exists(_.baseUrl == "http://127.0.0.1:8091"),
        loaded.policy.criteria.map(_.id.value) == List("pii", "weight", "task"),
        loaded.policy.criteria.find(_.id.value == "pii").exists(_.faces == ModelKind.both),
        loaded.policy.criteria.find(_.id.value == "task").exists(_.faces == Set(ModelKind.Conversational)),
        loaded.policy.defaultPrefer.map(_.value) == List("public-fast"),
        loaded.policy.backends
          .find(_.id.value == "grok-4.7")
          .exists(_.auth == hallmonitor.domain.BackendAuth.Grok("~/.grok")),
        loaded.policy.backends
          .find(_.id.value == "grok-4.6-bedrock")
          .exists(_.auth == hallmonitor.domain.BackendAuth.Bedrock("us-dev", "us-west-2")),
        loaded.policy.backends.find(_.id.value == "opus-5.5-bedrock").exists { case backend =>
          backend.wire match
            case hallmonitor.domain.UpstreamWire.Messages(headers) =>
              headers.get("anthropic-version").contains("2023-06-01")
            case _ => false
        },
        loaded.listen.port == 8080,
        loaded.apiKey.reveal == "hm-secret",
      )
    },
    test("a literal apiKey and an apiKeyEnv are the two auth forms") {
      val text = minimal(
        """
          |apiKey = "hm"
          |[[backends]]
          |id = "literal"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKey = "ollama"
          |[[backends]]
          |id = "from-env"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKeyEnv = "HALL_MONITOR_TEST_KEY"
          |""".stripMargin
      )
      for loaded <- Load.fromString(text, Map("HALL_MONITOR_TEST_KEY" -> "from-env-secret").get)
      yield assertTrue(
        loaded.policy.backends.map(b => b.id.value -> b.apiKey.reveal) ==
          List("literal" -> "ollama", "from-env" -> "from-env-secret")
      )
    },
    test("both apiKey and apiKeyEnv is rejected") {
      fails(
        minimal(
          """
            |apiKey = "hm"
            |[[backends]]
            |id = "both"
            |kind = "conversational"
            |baseUrl = "http://127.0.0.1:9"
            |upstreamModel = "m"
            |apiKey = "a"
            |apiKeyEnv = "OTHER"
            |""".stripMargin
        ),
        ConfigError.SecretBoth("both"),
      )
    },
    test("neither apiKey nor apiKeyEnv is rejected") {
      fails(
        minimal(
          """
            |apiKey = "hm"
            |[[backends]]
            |id = "none"
            |kind = "conversational"
            |baseUrl = "http://127.0.0.1:9"
            |upstreamModel = "m"
            |""".stripMargin
        ),
        ConfigError.SecretNeither("none"),
      )
    },
    test("an unset env var fails without echoing another backend token") {
      val text = minimal(
        """
          |apiKey = "hm"
          |[[backends]]
          |id = "local-strong"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKey = "ollama"
          |[[backends]]
          |id = "public-fast"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKeyEnv = "HALL_MONITOR_TEST_UNSET"
          |""".stripMargin
      )
      for error <- Load.fromString(text, _ => None).flip
      yield assertTrue(
        error.messages.exists(_.contains("HALL_MONITOR_TEST_UNSET")),
        !error.render.contains("ollama"),
        !error.render.contains("local"),
      )
    },
    test("criteria without a classifier fail") {
      fails(
        minimal(
          """
            |apiKey = "hm"
            |[[backends]]
            |id = "only"
            |kind = "conversational"
            |baseUrl = "http://127.0.0.1:9"
            |upstreamModel = "m"
            |apiKey = "z"
            |[[criteria]]
            |id = "pii"
            |kind = "noul"
            |instructions = "pii"
            |yes = "yes"
            |no = "no"
            |""".stripMargin
        ),
        ConfigError.CriteriaWithoutClassifier,
      )
    },
    test("no criteria and no classifier loads") {
      val text = minimal(
        """
          |apiKey = "hm"
          |[[backends]]
          |id = "only"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKey = "z"
          |defaultPrefer = ["only"]
          |""".stripMargin
      )
      for loaded <- Load.fromString(text, _ => None)
      yield assertTrue(loaded.classifier.isEmpty, loaded.policy.criteria.isEmpty)
    },
    test("a choice with no options fails") {
      fails(criterion("kind = \"choice\"\ninstructions = \"work\""), ConfigError.ChoiceSize("task", 0))
    },
    test("a score with one level fails") {
      fails(
        criterion("kind = \"score\"\ninstructions = \"work\"\nlevels = [\"only\"]"),
        ConfigError.ScoreSize("task", 1),
      )
    },
    test("an unknown allow id fails") {
      fails(rule("allow = [\"nope\"]"), ConfigError.UnknownBackend("rule pii-lock", "nope"))
    },
    test("a constraint whose allow list misses a face it applies to fails") {
      fails(rule("allow = [\"public-fast\"]"), ConfigError.AllowMissesFace("rule pii-lock", "decision"))
    },
    test("liveness and a grok quota load") {
      val text = minimal(
        """
          |apiKey = "hm"
          |downForSeconds = 20
          |[quotas.grok-build]
          |kind = "grok-weekly"
          |home = "~/.grok"
          |stopAt = 0.9
          |[[backends]]
          |id = "local"
          |kind = "conversational"
          |baseUrl = "http://127.0.0.1:9"
          |upstreamModel = "m"
          |apiKey = "ollama"
          |maxInFlight = 1
          |[backends.liveness]
          |everySeconds = 5
          |timeoutSeconds = 1
          |[[backends]]
          |id = "grok"
          |kind = "conversational"
          |baseUrl = "https://api.x.ai/v1"
          |upstreamModel = "grok-4.7"
          |auth = "grok"
          |quota = "grok-build"
          |""".stripMargin
      )
      for loaded <- Load.fromString(text, sampleEnv)
      yield assertTrue(
        loaded.downForSeconds == 20,
        loaded.policy.backends.find(_.id.value == "local").exists { backend =>
          backend.maxInFlight.contains(1) &&
          backend.liveness.exists(check => check.everySeconds == 5 && check.timeoutSeconds == 1 && check.path.isEmpty)
        },
        loaded.quotas.get(hallmonitor.domain.QuotaId("grok-build")).exists(_.stopAt == 0.9),
        loaded.policy.backends.find(_.id.value == "grok").exists(_.quota.map(_.value).contains("grok-build")),
      )
    },
    test("a grok quota on key auth is rejected") {
      fails(
        minimal(
          """
            |apiKey = "hm"
            |[quotas.grok-build]
            |kind = "grok-weekly"
            |home = "~/.grok"
            |stopAt = 0.9
            |[[backends]]
            |id = "keyed"
            |kind = "conversational"
            |baseUrl = "https://api.x.ai/v1"
            |upstreamModel = "m"
            |apiKey = "x"
            |quota = "grok-build"
            |""".stripMargin
        ),
        ConfigError.QuotaAuth("keyed"),
      )
    },
    test("resolvePath prefers the argument, then the env var, then hall-monitor.toml") {
      assertTrue(
        Load.resolvePath(List("from-arg.toml"), _ => None) == Path.of("from-arg.toml"),
        Load.resolvePath(Nil, Map("HALL_MONITOR_CONFIG" -> "from-env.toml").get) == Path.of("from-env.toml"),
        Load.resolvePath(Nil, _ => None) == Path.of("hall-monitor.toml"),
      )
    },
  )

  private def fails(text: String, expected: ConfigError) =
    for error <- Load.fromString(text, sampleEnv).flip
    yield assertTrue(error.messages.contains(expected.message))

  private def minimal(body: String): String =
    s"""
       |$body
       |[listen]
       |host = "127.0.0.1"
       |port = 8080
       |""".stripMargin

  private def criterion(fields: String): String =
    minimal(
      s"""
         |apiKey = "hm"
         |[classifier]
         |baseUrl = "http://127.0.0.1:8091"
         |model = "jev-latest"
         |timeoutSeconds = 10
         |apiKey = "local"
         |[[backends]]
         |id = "public-fast"
         |kind = "conversational"
         |baseUrl = "http://127.0.0.1:9"
         |upstreamModel = "m"
         |apiKey = "z"
         |[[criteria]]
         |id = "task"
         |$fields
         |""".stripMargin
    )

  private def rule(allow: String): String =
    minimal(
      s"""
         |apiKey = "hm"
         |[classifier]
         |baseUrl = "http://127.0.0.1:8091"
         |model = "jev-latest"
         |timeoutSeconds = 10
         |apiKey = "local"
         |[[backends]]
         |id = "public-fast"
         |kind = "conversational"
         |baseUrl = "http://127.0.0.1:9"
         |upstreamModel = "m"
         |apiKey = "z"
         |[[backends]]
         |id = "jev-vpc"
         |kind = "decision"
         |baseUrl = "http://127.0.0.1:10"
         |upstreamModel = "jev-latest"
         |apiKey = "z"
         |[[criteria]]
         |id = "pii"
         |kind = "noul"
         |instructions = "pii"
         |yes = "yes"
         |no = "no"
         |[[rules]]
         |id = "pii-lock"
         |type = "constraint"
         |$allow
         |[[rules.when]]
         |criterion = "pii"
         |yesAtLeast = 0.4
         |""".stripMargin
    )

  private def exampleText: String =
    Iterator
      .iterate(Path.of("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("examples/hall-monitor.toml"))
      .find(Files.isRegularFile(_))
      .map(Files.readString(_))
      .getOrElse(throw new IllegalStateException("examples/hall-monitor.toml not found"))
end LoadSpec
