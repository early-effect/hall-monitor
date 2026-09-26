package hallmonitor.config

import zio.Config

final case class ListenRaw(host: String, port: Int)

final case class SecretRaw(apiKey: Option[String], apiKeyEnv: Option[String])

final case class ClassifierRaw(
    baseUrl: String,
    model: String,
    timeoutSeconds: Int,
    apiKey: Option[String],
    apiKeyEnv: Option[String],
)

final case class BackendRaw(
    id: String,
    kind: String,
    baseUrl: String,
    upstreamModel: String,
    apiKey: Option[String],
    apiKeyEnv: Option[String],
    aliases: List[String],
    protocol: Option[String],
    auth: Option[String],
    grokHome: Option[String],
    authCommand: Option[String],
    authArgs: Option[List[String]],
    authTtlSeconds: Option[Int],
    authTimeoutSeconds: Option[Int],
    developerRole: Option[Boolean],
    maxTokensField: Option[String],
    headers: Option[Map[String, String]],
)

final case class PredicateRaw(
    criterion: String,
    yesAtLeast: Option[Double],
    choice: Option[List[String]],
    atLeast: Option[String],
    atMost: Option[String],
    minConfidence: Option[Double],
)

final case class RuleRaw(
    id: String,
    typeName: String,
    when: List[PredicateRaw],
    allow: Option[List[String]],
    prefer: Option[List[String]],
)

final case class CriterionRaw(
    id: String,
    kind: String,
    instructions: String,
    yes: Option[String],
    no: Option[String],
    options: Option[Map[String, String]],
    levels: Option[List[String]],
    faces: Option[List[String]],
)

final case class AppRaw(
    listen: ListenRaw,
    apiKey: Option[String],
    apiKeyEnv: Option[String],
    classifier: Option[ClassifierRaw],
    maxStateChars: Int,
    upstreamIdleSeconds: Int,
    backends: List[BackendRaw],
    criteria: List[CriterionRaw],
    rules: List[RuleRaw],
    defaultPrefer: List[String],
)

object AppRaw:
  def descriptor: Config[AppRaw] =
    map2(listen, secret("apiKey", "apiKeyEnv"))((listen, secret) => (listen, secret))
      .pipe(classifier)((acc, classifier) => (acc._1, acc._2, classifier))
      .pipe(Config.int("maxStateChars").withDefault(16000))((acc, maxStateChars) =>
        (acc._1, acc._2, acc._3, maxStateChars)
      )
      .pipe(Config.int("upstreamIdleSeconds").withDefault(300))((acc, idle) => (acc._1, acc._2, acc._3, acc._4, idle))
      .pipe(Config.listOf("backends", backend))((acc, backends) => (acc._1, acc._2, acc._3, acc._4, acc._5, backends))
      .pipe(Config.listOf("criteria", criterion).withDefault(Nil))((acc, criteria) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, criteria)
      )
      .pipe(Config.listOf("rules", rule).withDefault(Nil))((acc, rules) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, rules)
      )
      .pipe(Config.listOf("defaultPrefer", Config.string).withDefault(Nil)) { (acc, prefer) =>
        val (listen, secret, classifier, maxStateChars, idle, backends, criteria, rules) = acc
        AppRaw(
          listen = listen,
          apiKey = secret.apiKey,
          apiKeyEnv = secret.apiKeyEnv,
          classifier = classifier,
          maxStateChars = maxStateChars,
          upstreamIdleSeconds = idle,
          backends = backends,
          criteria = criteria,
          rules = rules,
          defaultPrefer = prefer,
        )
      }

  private val listen: Config[ListenRaw] =
    map2(Config.string("host"), Config.int("port"))(ListenRaw(_, _)).nested("listen")

  private val classifier: Config[Option[ClassifierRaw]] =
    map2(Config.string("baseUrl"), Config.string("model"))((baseUrl, model) => (baseUrl, model))
      .pipe(Config.int("timeoutSeconds"))((acc, timeout) => (acc._1, acc._2, timeout))
      .pipe(secret("apiKey", "apiKeyEnv"))((acc, secret) =>
        ClassifierRaw(acc._1, acc._2, acc._3, secret.apiKey, secret.apiKeyEnv)
      )
      .nested("classifier")
      .optional

  private val backend: Config[BackendRaw] =
    map2(Config.string("id"), Config.string("kind"))((id, kind) => (id, kind))
      .pipe(Config.string("baseUrl"))((acc, baseUrl) => (acc._1, acc._2, baseUrl))
      .pipe(Config.string("upstreamModel"))((acc, model) => (acc._1, acc._2, acc._3, model))
      .pipe(secret("apiKey", "apiKeyEnv"))((acc, secret) => (acc._1, acc._2, acc._3, acc._4, secret))
      .pipe(Config.listOf("aliases", Config.string).withDefault(Nil))((acc, aliases) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, aliases)
      )
      .pipe(Config.string("protocol").optional)((acc, protocol) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, protocol)
      )
      .pipe(Config.string("auth").optional)((acc, auth) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, auth)
      )
      .pipe(Config.string("grokHome").optional)((acc, grokHome) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8, grokHome)
      )
      .pipe(Config.string("authCommand").optional)((acc, authCommand) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8, acc._9, authCommand)
      )
      .pipe(Config.listOf("authArgs", Config.string).optional)((acc, authArgs) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8, acc._9, acc._10, authArgs)
      )
      .pipe(Config.int("authTtlSeconds").optional)((acc, ttl) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8, acc._9, acc._10, acc._11, ttl)
      )
      .pipe(Config.int("authTimeoutSeconds").optional)((acc, timeout) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8, acc._9, acc._10, acc._11, acc._12, timeout)
      )
      .pipe(Config.boolean("developerRole").optional)((acc, developerRole) =>
        (
          acc._1,
          acc._2,
          acc._3,
          acc._4,
          acc._5,
          acc._6,
          acc._7,
          acc._8,
          acc._9,
          acc._10,
          acc._11,
          acc._12,
          acc._13,
          developerRole,
        )
      )
      .pipe(Config.string("maxTokensField").optional)((acc, maxTokensField) =>
        (
          acc._1,
          acc._2,
          acc._3,
          acc._4,
          acc._5,
          acc._6,
          acc._7,
          acc._8,
          acc._9,
          acc._10,
          acc._11,
          acc._12,
          acc._13,
          acc._14,
          maxTokensField,
        )
      )
      .pipe(Config.table("headers", Config.string).optional) { (acc, headers) =>
        val (
          id,
          kind,
          baseUrl,
          model,
          secret,
          aliases,
          protocol,
          auth,
          grokHome,
          authCommand,
          authArgs,
          ttl,
          timeout,
          developerRole,
          maxTokensField,
        ) = acc
        BackendRaw(
          id,
          kind,
          baseUrl,
          model,
          secret.apiKey,
          secret.apiKeyEnv,
          aliases,
          protocol,
          auth,
          grokHome,
          authCommand,
          authArgs,
          ttl,
          timeout,
          developerRole,
          maxTokensField,
          headers.filter(_.nonEmpty),
        )
      }

  private val predicate: Config[PredicateRaw] =
    Config
      .string("criterion")
      .pipe(Config.double("yesAtLeast").optional)((criterion, yesAtLeast) => (criterion, yesAtLeast))
      .pipe(Config.listOf("choice", Config.string).optional)((acc, choice) => (acc._1, acc._2, choice))
      .pipe(Config.string("atLeast").optional)((acc, atLeast) => (acc._1, acc._2, acc._3, atLeast))
      .pipe(Config.string("atMost").optional)((acc, atMost) => (acc._1, acc._2, acc._3, acc._4, atMost))
      .pipe(Config.double("minConfidence").optional) { (acc, minConfidence) =>
        val (criterion, yesAtLeast, choice, atLeast, atMost) = acc
        PredicateRaw(criterion, yesAtLeast, choice, atLeast, atMost, minConfidence)
      }

  private val rule: Config[RuleRaw] =
    map2(Config.string("id"), Config.string("type"))((id, typeName) => (id, typeName))
      .pipe(Config.listOf("when", predicate).withDefault(Nil))((acc, when) => (acc._1, acc._2, when))
      .pipe(Config.listOf("allow", Config.string).optional)((acc, allow) => (acc._1, acc._2, acc._3, allow))
      .pipe(Config.listOf("prefer", Config.string).optional) { (acc, prefer) =>
        val (id, typeName, when, allow) = acc
        RuleRaw(id, typeName, when, allow, prefer)
      }

  private val criterion: Config[CriterionRaw] =
    map2(Config.string("id"), Config.string("kind"))((id, kind) => (id, kind))
      .pipe(Config.string("instructions"))((acc, instructions) => (acc._1, acc._2, instructions))
      .pipe(Config.string("yes").optional)((acc, yes) => (acc._1, acc._2, acc._3, yes))
      .pipe(Config.string("no").optional)((acc, no) => (acc._1, acc._2, acc._3, acc._4, no))
      .pipe(Config.table("options", Config.string).optional)((acc, options) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, options)
      )
      .pipe(Config.listOf("levels", Config.string).optional)((acc, levels) =>
        (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, levels)
      )
      .pipe(Config.listOf("faces", Config.string).optional) { (acc, faces) =>
        val (id, kind, instructions, yes, no, options, levels) = acc
        CriterionRaw(id, kind, instructions, yes, no, options, levels, faces)
      }

  private def secret(key: String, env: String): Config[SecretRaw] =
    map2(Config.string(key).optional, Config.string(env).optional)(SecretRaw(_, _))

  private def map2[A, B, C](left: Config[A], right: Config[B])(f: (A, B) => C): Config[C] =
    (left ++ right).map { case (a, b) => f(a, b) }

  extension [A](self: Config[A])
    private def pipe[B, C](right: Config[B])(f: (A, B) => C): Config[C] =
      map2(self, right)(f)
end AppRaw
