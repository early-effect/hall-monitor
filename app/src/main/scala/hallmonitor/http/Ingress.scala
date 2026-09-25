package hallmonitor.http

import hallmonitor.domain.{ModelKind, RouteError}
import zio.json.*
import zio.json.ast.Json

final case class Incoming(
    requested: Option[String],
    text: String,
    toolNames: List[String],
    stream: Boolean,
    body: Json.Obj,
)

object Ingress:
  def parse(face: ModelKind, raw: String): Either[RouteError, Incoming] =
    raw.fromJson[Json] match
      case Left(detail)         => Left(RouteError.Malformed(detail))
      case Right(obj: Json.Obj) =>
        face match
          case ModelKind.Conversational => chat(obj)
          case ModelKind.Decision       => systemOne(obj)
      case Right(_) => Left(RouteError.Malformed("body must be a JSON object"))

  private def chat(obj: Json.Obj): Either[RouteError, Incoming] =
    for
      requested <- stringField(obj, "model")
      stream    <- boolField(obj, "stream")
      messages  <- obj.get("messages") match
        case Some(Json.Arr(values)) => Right(values.toList)
        case Some(_)                => Left(RouteError.Malformed("messages must be an array"))
        case None                   => Left(RouteError.Malformed("messages is required"))
      lines <- messages.zipWithIndex.foldLeft[Either[RouteError, List[String]]](Right(Nil)) {
        case (acc, (message, index)) => acc.flatMap(lines => line(message, index).map(lines :+ _))
      }
    yield Incoming(requested, lines.mkString("\n"), toolNames(obj, messages), stream.getOrElse(false), obj)

  private def systemOne(obj: Json.Obj): Either[RouteError, Incoming] =
    for
      requested <- stringField(obj, "model")
      state     <- obj.get("state").toRight(RouteError.Malformed("state is required"))
    yield
      val keys = obj.get("questions") match
        case Some(questions: Json.Obj) => questions.fields.map(_._1).toList
        case _                         => Nil
      val rendered = state match
        case Json.Str(value) => value
        case other           => other.toJson
      val text =
        if keys.isEmpty then rendered
        else s"questions: ${keys.mkString(", ")}\n$rendered"
      Incoming(requested, text, Nil, stream = false, obj)

  private def stringField(obj: Json.Obj, name: String): Either[RouteError, Option[String]] =
    obj.get(name) match
      case None | Some(Json.Null) => Right(None)
      case Some(Json.Str(value))  => Right(Some(value))
      case Some(_)                => Left(RouteError.Malformed(s"$name must be a string"))

  private def boolField(obj: Json.Obj, name: String): Either[RouteError, Option[Boolean]] =
    obj.get(name) match
      case None | Some(Json.Null) => Right(None)
      case Some(Json.Bool(value)) => Right(Some(value))
      case Some(_)                => Left(RouteError.Malformed(s"$name must be a boolean"))

  private def line(message: Json, index: Int): Either[RouteError, String] =
    message match
      case obj: Json.Obj =>
        val role = obj.get("role").collect { case Json.Str(value) => value }.getOrElse("user")
        content(obj.get("content"), index).map { text =>
          val calls  = toolCalls(obj)
          val suffix = if calls.isEmpty then "" else s"\ntool_calls: ${calls.mkString(", ")}"
          s"[$role] $text$suffix"
        }
      case _ => Left(RouteError.Malformed(s"messages[$index] must be an object"))

  private def content(value: Option[Json], index: Int): Either[RouteError, String] =
    value match
      case None | Some(Json.Null) => Right("")
      case Some(Json.Str(text))   => Right(text)
      case Some(Json.Arr(parts))  =>
        parts.zipWithIndex.foldLeft[Either[RouteError, String]](Right("")) { case (acc, (part, partIndex)) =>
          acc.flatMap { text =>
            partText(part, index, partIndex).map(next => if text.isEmpty then next else s"$text$next")
          }
        }
      case Some(_) => Left(RouteError.Malformed(s"messages[$index].content must be a string or array"))

  private def partText(part: Json, message: Int, index: Int): Either[RouteError, String] =
    part match
      case Json.Str(text) => Right(text)
      case obj: Json.Obj  =>
        obj.get("type").collect { case Json.Str(value) => value } match
          case Some("image_url") | Some("image") | Some("input_image") => Right("[image]")
          case _                                                       =>
            Right(
              obj
                .get("text")
                .collect { case Json.Str(value) => value }
                .orElse(obj.get("content").collect { case Json.Str(value) => value })
                .getOrElse("")
            )
      case _ => Left(RouteError.Malformed(s"messages[$message].content[$index] must be a string or object"))

  private def toolNames(root: Json.Obj, messages: List[Json]): List[String] =
    val declared = root.get("tools") match
      case Some(Json.Arr(values)) => values.toList.flatMap(toolName)
      case _                      => Nil
    val called = messages.collect { case obj: Json.Obj => toolCalls(obj) }.flatten
    (declared ++ called).distinct

  private def toolCalls(message: Json.Obj): List[String] =
    message.get("tool_calls") match
      case Some(Json.Arr(values)) => values.toList.flatMap(toolName)
      case _                      => Nil

  private def toolName(value: Json): Option[String] =
    value match
      case obj: Json.Obj =>
        obj
          .get("function")
          .collect { case function: Json.Obj =>
            function.get("name").collect { case Json.Str(name) => name }
          }
          .flatten
          .orElse(obj.get("name").collect { case Json.Str(name) => name })
      case _ => None
end Ingress
