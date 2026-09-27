package hallmonitor.journal

import hallmonitor.admit.{Attempt, Fall, Skip}
import hallmonitor.domain.*
import hexis.Answer
import zio.json.ast.Json
import zio.json.*
import zio.*

final case class Timings(classify: Duration, total: Duration)

final case class CallRecord(
    id: String,
    face: ModelKind,
    classifier: Option[String],
    plan: RoutePlan,
    attempts: List[Attempt],
    served: Option[BackendId],
    timings: Timings,
    failure: Option[RouteError],
)

final class Journal(records: Ref[Chunk[CallRecord]]):
  def publish(record: CallRecord): UIO[Unit] =
    records.update { chunk =>
      val next = chunk :+ record
      if next.length > Journal.Capacity then next.drop(next.length - Journal.Capacity) else next
    } *> ZIO.logInfo(Journal.render(record))

  def recent: UIO[Chunk[CallRecord]] =
    records.get
end Journal

object Journal:
  val Capacity: Int = 200

  def make: UIO[Journal] =
    Ref.make(Chunk.empty[CallRecord]).map(Journal(_))

  def render(record: CallRecord): String =
    view(record).toJson

  def view(record: CallRecord): Json =
    val served = record.served.map(_.value)
    val first  = record.plan.order.headOption.map(_.id.value)
    val fields = List(
      Some("id"   -> Json.Str(record.id)),
      Some("face" -> Json.Str(faceName(record.face))),
      record.plan.trace.requested.map(name => "requested" -> Json.Str(name)),
      record.classifier.map(host => "classifier" -> Json.Str(host)),
      Some("answers"  -> Json.Arr(Chunk.fromIterable(answers(record.plan.trace.answers)))),
      Some("steps"    -> Json.Arr(Chunk.fromIterable(record.plan.trace.steps.map(step)))),
      Some("eligible" -> strings(record.plan.trace.eligible.map(_.value))),
      Some("attempts" -> Json.Arr(Chunk.fromIterable(record.attempts.map(attempt)))),
      served.map(id => "served" -> Json.Str(id)),
      first.filter(name => served.forall(_ != name)).map(name => "fallbackFrom" -> Json.Str(name)),
      Some("classifyMs"   -> Json.Num(record.timings.classify.toMillis)),
      Some("totalMs"      -> Json.Num(record.timings.total.toMillis)),
      Some("truncated"    -> Json.Bool(record.plan.trace.truncated)),
      Some("unclassified" -> Json.Bool(record.plan.trace.unclassified)),
      record.failure.map(error => "failure" -> Json.Str(error.message)),
    ).flatten
    Json.Obj(Chunk.fromIterable(fields))
  end view

  private def answers(values: Map[CriterionId, Answer[?]]): List[Json] =
    values.toList.sortBy(_._1.value).map { (id, answer) =>
      val common = "criterion" -> Json.Str(id.value)
      answer match
        case Answer.Noul(probability) =>
          Json.Obj(common, "kind" -> Json.Str("noul"), "probability" -> Json.Num(probability))
        case choice: Answer.Choice[?] =>
          Json.Obj(
            common,
            "kind"       -> Json.Str("choice"),
            "label"      -> Json.Str(label(choice.choice)),
            "confidence" -> Json.Num(choice.confidence),
          )
        case score: Answer.Score[?] =>
          Json.Obj(
            common,
            "kind"       -> Json.Str("score"),
            "score"      -> Json.Num(score.score),
            "confidence" -> Json.Num(score.confidence),
          )
      end match
    }

  private def step(value: Step): Json =
    value match
      case constraint: Step.Constraint =>
        Json.Obj(
          "kind"      -> Json.Str("constraint"),
          "id"        -> Json.Str(constraint.id.value),
          "ruling"    -> Json.Str(ruling(constraint.ruling)),
          "narrowed"  -> Json.Bool(constraint.narrowed),
          "allow"     -> strings(constraint.allow.map(_.value)),
          "remaining" -> strings(constraint.remaining.map(_.value)),
        )
      case preference: Step.Preference =>
        Json.Obj(
          "kind"   -> Json.Str("preference"),
          "id"     -> Json.Str(preference.id.value),
          "ruling" -> Json.Str(ruling(preference.ruling)),
          "ranked" -> Json.Bool(preference.ranked),
          "prefer" -> strings(preference.prefer.map(_.value)),
        )
      case Step.Pinned(name, backend) =>
        Json.Obj(
          "kind"    -> Json.Str("pinned"),
          "name"    -> Json.Str(name),
          "backend" -> Json.Str(backend.value),
        )
      case Step.Ranked(order) =>
        Json.Obj("kind" -> Json.Str("ranked"), "order" -> strings(order.map(_.value)))

  private def attempt(value: Attempt): Json =
    value match
      case Attempt.Skipped(id, reason) =>
        val extra = reason match
          case Skip.Saturated(held, max) =>
            List("inFlight" -> Json.Num(held), "maxInFlight" -> Json.Num(max))
          case Skip.OverQuota(_, used, stopAt, resetsAt) =>
            List(
              Some("used"   -> Json.Num(used)),
              Some("stopAt" -> Json.Num(stopAt)),
              resetsAt.map(value => "resetsAt" -> Json.Str(value)),
            ).flatten
          case Skip.CoolingDown(untilMs) =>
            List("untilMs" -> Json.Num(untilMs))
          case Skip.Down(sinceMs) =>
            List("sinceMs" -> Json.Num(sinceMs))
        Json.Obj(
          Chunk.fromIterable(List("backend" -> Json.Str(id.value), "result" -> Json.Str(skipName(reason))) ++ extra)
        )
      case Attempt.Called(id, status, elapsed, fellThrough) =>
        val result = fellThrough match
          case Some(Fall.Unreachable)   => "unreachable"
          case Some(Fall.Status(_))     => "status"
          case None if status.isDefined => "ok"
          case None                     => "failed"
        val fields = List(
          Some("backend" -> Json.Str(id.value)),
          Some("result"  -> Json.Str(result)),
          status.map(code => "status" -> Json.Num(code)),
          Some("ms" -> Json.Num(elapsed.toMillis)),
        ).flatten
        Json.Obj(Chunk.fromIterable(fields))

  private def skipName(reason: Skip): String =
    reason match
      case _: Skip.Saturated   => "saturated"
      case _: Skip.OverQuota   => "over-quota"
      case _: Skip.CoolingDown => "cooling-down"
      case _: Skip.Down        => "down"

  private def ruling(value: Ruling): String =
    value match
      case Ruling.Applies       => "applies"
      case Ruling.DoesNotApply  => "does-not-apply"
      case Ruling.Uncertain     => "uncertain"
      case Ruling.NotOnThisFace => "not-on-this-face"

  private def faceName(face: ModelKind): String =
    face match
      case ModelKind.Conversational => "conversational"
      case ModelKind.Decision       => "decision"

  private def strings(values: List[String]): Json =
    Json.Arr(Chunk.fromIterable(values.map(Json.Str(_))))

  private def label(value: Any): String =
    value match
      case text: String => text
      case other        => String.valueOf(other)
end Journal
