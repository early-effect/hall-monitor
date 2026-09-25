package hallmonitor.classify

import hallmonitor.domain.*
import hexis.*
import hexis.ToState.given
import hexis.wire.{Decode, WireAnswer, WireQuestion}
import zio.json.ast.Json
import zio.{Duration, IO, ZIO}

import scala.collection.immutable.ListMap

final case class Subject(requested: Option[String], text: String, toolNames: List[String])

final case class Reading(
    answers: Map[CriterionId, Answer[?]],
    truncated: Boolean,
    asked: Boolean,
)

final case class Batch(items: List[(String, Question[?])])

object Batch:
  given Ask.Aux[Batch, Map[String, Answer[?]]] =
    new Ask[Batch]:
      type Out = Map[String, Answer[?]]

      def encode(batch: Batch): Map[String, WireQuestion] =
        batch.items.map((key, question) => key -> WireQuestion.from(question)).toMap

      def decode(batch: Batch, raw: Map[String, WireAnswer]): Either[JevError, Out] =
        batch.items.foldLeft[Either[JevError, Out]](Right(Map.empty)) { case (acc, (key, question)) =>
          acc.flatMap { answers =>
            raw.get(key).toRight(JevError.Decode(key, "missing answer")).flatMap { answer =>
              decodeOne(question, answer, key).map(value => answers.updated(key, value))
            }
          }
        }

  private def decodeOne(question: Question[?], raw: WireAnswer, path: String): Either[JevError, Answer[?]] =
    question match
      case _: Question.Noul[?]        => Decode.noul(raw, path)
      case choice: Question.Choice[?] => Decode.choice(raw, choice.criteria, path)
      case score: Question.Score[?]   => Decode.score(raw, score.levels, path)
end Batch

object Classify:
  def run(
      policy: Policy,
      classifier: Option[ClassifierEndpoint],
      face: ModelKind,
      subject: Subject,
      transport: Transport,
  ): IO[RouteError, Reading] =
    val rendered  = render(subject)
    val truncated = rendered.length > policy.maxStateChars
    val text      = if truncated then rendered.take(policy.maxStateChars) else rendered
    val selected  = policy.criteria.filter(_.faces.contains(face))
    if selected.isEmpty then ZIO.succeed(Reading(Map.empty, truncated, asked = false))
    else
      classifier match
        case None           => ZIO.fail(RouteError.Classifier("classifier is not configured"))
        case Some(endpoint) =>
          ZIO
            .attempt(Batch(selected.map(criterion => criterion.id.value -> question(criterion))))
            .mapError(cause => RouteError.Classifier(Option(cause.getMessage).getOrElse(cause.toString)))
            .flatMap { batch =>
              val config = hexis.Config(
                apiKey = ApiKey.unsafely(endpoint.apiKey.reveal),
                baseUrl = endpoint.baseUrl,
                model = Model.parse(endpoint.model),
                timeout = Duration.fromSeconds(endpoint.timeoutSeconds.toLong),
              )
              val state = Json.Obj(
                "face"           -> Json.Str(faceName(face)),
                "requestedModel" -> subject.requested.fold(Json.Null)(Json.Str(_)),
                "text"           -> Json.Str(text),
                "truncated"      -> Json.Bool(truncated),
              )
              new SystemOne.Live(transport, config)
                .evaluate(state, batch)
                .mapBoth(
                  error => RouteError.Classifier(error.message),
                  answers =>
                    Reading(
                      answers.flatMap { (key, value) =>
                        selected.find(_.id.value == key).map(_.id -> value)
                      },
                      truncated,
                      asked = true,
                    ),
                )
            }
    end if
  end run

  def summary(answers: Map[CriterionId, Answer[?]]): String =
    answers.toList
      .sortBy(_._1.value)
      .map { (id, answer) =>
        s"${id.value}=${describe(answer)}"
      }
      .mkString(" ")

  def authority(baseUrl: String): String =
    val uri  = java.net.URI(baseUrl)
    val host = Option(uri.getHost).getOrElse(baseUrl)
    if uri.getPort > 0 then s"$host:${uri.getPort}" else host

  private def render(subject: Subject): String =
    val tools =
      if subject.toolNames.isEmpty then ""
      else s"tools: ${subject.toolNames.mkString(", ")}\n"
    tools + subject.text

  private def question(criterion: Criterion): Question[?] =
    criterion match
      case noul: Criterion.Noul =>
        Noul(noul.instructions, noul.yes, noul.no)
      case choice: Criterion.Choice =>
        Choice.dynamic(choice.instructions, ListMap.from(choice.options))
      case score: Criterion.Score =>
        Score.dynamic(score.instructions, score.levels)

  private def faceName(face: ModelKind): String =
    face match
      case ModelKind.Conversational => "conversational"
      case ModelKind.Decision       => "decision"

  private def describe(answer: Answer[?]): String =
    answer match
      case Answer.Noul(probability) => "%.2f".format(asDouble(probability))
      case choice: Answer.Choice[?] =>
        s"${label(choice.choice)}@${"%.2f".format(asDouble(choice.confidence))}"
      case score: Answer.Score[?] =>
        s"${"%.2f".format(score.score)}@${"%.2f".format(asDouble(score.confidence))}"

  private def label(value: Any): String =
    value match
      case text: String => text
      case other        => String.valueOf(other)

  private def asDouble(value: Double): Double = value
end Classify
