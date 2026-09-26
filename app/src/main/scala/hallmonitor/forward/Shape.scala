package hallmonitor.forward

import zio.json.*
import zio.json.ast.Json

object Shape:
  def chat(payload: Json.Obj, compat: hallmonitor.domain.ChatCompat): Json.Obj =
    val renamed = compat.maxTokensField match
      case Some("max_tokens")            => rename(payload, "max_completion_tokens", "max_tokens")
      case Some("max_completion_tokens") => rename(payload, "max_tokens", "max_completion_tokens")
      case _                             => payload
    if compat.rewriteDeveloper then rewriteRoles(renamed) else renamed

  def responses(payload: Json.Obj, model: String): Json.Obj =
    val messages = arr(payload, "messages")
    val systems  = messages.flatMap(textOf("system"))
    val input    = messages.flatMap(responseItem)
    val tools    = arr(payload, "tools").flatMap(responseTool)
    var fields   = List(
      "model"  -> Json.Str(model),
      "input"  -> Json.Arr(zio.Chunk.fromIterable(input)),
      "store"  -> Json.Bool(false),
      "stream" -> Json.Bool(false),
    )
    if systems.nonEmpty then fields = fields :+ ("instructions" -> Json.Str(systems.mkString("\n\n")))
    if tools.nonEmpty then
      fields = fields :+ ("tools" -> Json.Arr(zio.Chunk.fromIterable(tools))) :+ ("tool_choice" -> Json.Str("auto"))
    Json.Obj(zio.Chunk.fromIterable(fields))
  end responses

  def fromResponses(model: String, body: Json.Obj): Json.Obj =
    val output  = arr(body, "output")
    val text    = output.flatMap(outputText).mkString
    val calls   = output.flatMap(functionCall)
    val message = if calls.isEmpty then Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Str(text))
    else
      Json.Obj(
        "role"       -> Json.Str("assistant"),
        "content"    -> Json.Str(text),
        "tool_calls" -> Json.Arr(zio.Chunk.fromIterable(calls)),
      )
    completion(model, message, if calls.isEmpty then "stop" else "tool_calls", body)
  end fromResponses

  def messages(payload: Json.Obj, model: String): Json.Obj =
    val messages = arr(payload, "messages")
    val systems  = messages.flatMap(textOf("system")) ++ messages.flatMap(textOf("developer"))
    val dialog   = messages.flatMap(anthropicMessage)
    val tools    = arr(payload, "tools").flatMap(anthropicTool)
    var fields   = List(
      "model"      -> Json.Str(model),
      "messages"   -> Json.Arr(zio.Chunk.fromIterable(dialog)),
      "max_tokens" -> payload.get("max_tokens").orElse(payload.get("max_completion_tokens")).getOrElse(Json.Num(4096)),
    )
    if systems.nonEmpty then fields = fields :+ ("system" -> Json.Str(systems.mkString("\n\n")))
    if tools.nonEmpty then fields = fields :+ ("tools" -> Json.Arr(zio.Chunk.fromIterable(tools)))
    Json.Obj(zio.Chunk.fromIterable(fields))
  end messages

  def fromMessages(model: String, body: Json.Obj): Json.Obj =
    val blocks  = arr(body, "content")
    val text    = blocks.flatMap(blockText).mkString
    val calls   = blocks.flatMap(toolUse)
    val message =
      if calls.isEmpty then Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Str(text))
      else
        Json.Obj(
          "role"       -> Json.Str("assistant"),
          "content"    -> Json.Str(text),
          "tool_calls" -> Json.Arr(zio.Chunk.fromIterable(calls)),
        )
    val reason = body.get("stop_reason") match
      case Some(Json.Str("tool_use")) => "tool_calls"
      case _                          => "stop"
    completion(model, message, reason, body)
  end fromMessages

  def asEventStream(completion: Json.Obj): String =
    val message = completion.get("choices").collect { case Json.Arr(values) => values.headOption }.flatten match
      case Some(Json.Obj(fields)) =>
        fields.collectFirst { case ("message", message: Json.Obj) => message }.getOrElse(Json.Obj())
      case _ => Json.Obj()
    val chunk = Json.Obj(
      "object"  -> Json.Str("chat.completion.chunk"),
      "choices" -> Json.Arr(
        Json.Obj(
          "index"         -> Json.Num(0),
          "delta"         -> message,
          "finish_reason" -> Json.Str("stop"),
        )
      ),
    )
    s"data: ${chunk.toJson}\n\ndata: [DONE]\n\n"
  end asEventStream

  private def completion(model: String, message: Json, reason: String, body: Json.Obj): Json.Obj =
    val usage = body.get("usage").collect { case usage: Json.Obj =>
      val in  = number(usage, "input_tokens").orElse(number(usage, "prompt_tokens")).getOrElse(0)
      val out = number(usage, "output_tokens").orElse(number(usage, "completion_tokens")).getOrElse(0)
      Json.Obj(
        "prompt_tokens"     -> Json.Num(in),
        "completion_tokens" -> Json.Num(out),
        "total_tokens"      -> Json.Num(in + out),
      )
    }
    Json.Obj(
      List(
        Some("id"     -> body.get("id").getOrElse(Json.Str("hall-monitor"))),
        Some("object" -> Json.Str("chat.completion")),
        Some("model"  -> Json.Str(model)),
        Some(
          "choices" -> Json.Arr(
            Json.Obj("index" -> Json.Num(0), "message" -> message, "finish_reason" -> Json.Str(reason))
          )
        ),
        usage.map("usage" -> _),
      ).flatten*
    )
  end completion

  private def rename(payload: Json.Obj, from: String, to: String): Json.Obj =
    if payload.get(to).isDefined || payload.get(from).isEmpty then payload
    else
      Json.Obj(
        payload.fields.collect {
          case (key, value) if key == from => to -> value
          case field                       => field
        }
      )

  private def rewriteRoles(payload: Json.Obj): Json.Obj =
    Json.Obj(
      payload.fields.map {
        case ("messages", Json.Arr(values)) =>
          "messages" -> Json.Arr(
            values.map {
              case message: Json.Obj if message.get("role").contains(Json.Str("developer")) =>
                Json.Obj(message.fields.map {
                  case ("role", _) => "role" -> Json.Str("system")
                  case field       => field
                })
              case other => other
            }
          )
        case field => field
      }
    )

  private def arr(payload: Json.Obj, key: String): List[Json.Obj] =
    payload.get(key) match
      case Some(Json.Arr(values)) => values.collect { case obj: Json.Obj => obj }.toList
      case _                      => Nil

  private def textOf(role: String)(message: Json.Obj): Option[String] =
    if message.get("role").contains(Json.Str(role)) then Some(text(message.get("content"))).filter(_.nonEmpty)
    else None

  private def text(content: Option[Json]): String =
    content match
      case Some(Json.Str(value))  => value
      case Some(Json.Arr(values)) =>
        values
          .collect {
            case obj: Json.Obj
                if obj
                  .get("type")
                  .forall(kind =>
                    kind == Json.Str("text") || kind == Json.Str("input_text") || kind == Json.Str("output_text")
                  ) =>
              obj.get("text").collect { case Json.Str(value) => value }
          }
          .flatten
          .mkString
      case _ => ""

  private def responseItem(message: Json.Obj): List[Json] =
    message.get("role") match
      case Some(Json.Str("system")) | Some(Json.Str("developer")) => Nil
      case Some(Json.Str("tool"))                                 =>
        List(
          Json.Obj(
            "type"    -> Json.Str("function_call_output"),
            "call_id" -> message.get("tool_call_id").getOrElse(Json.Str("")),
            "output"  -> Json.Str(text(message.get("content"))),
          )
        )
      case Some(Json.Str(role)) =>
        val spoken = text(message.get("content"))
        val said   =
          if spoken.isEmpty then Nil
          else
            val part = if role == "assistant" then "output_text" else "input_text"
            List(
              Json.Obj(
                "type"    -> Json.Str("message"),
                "role"    -> Json.Str(role),
                "content" -> Json.Arr(Json.Obj("type" -> Json.Str(part), "text" -> Json.Str(spoken))),
              )
            )
        val calls = message.get("tool_calls") match
          case Some(Json.Arr(values)) =>
            values.toList.collect { case call: Json.Obj =>
              val function = call.get("function").collect { case obj: Json.Obj => obj }.getOrElse(Json.Obj())
              Json.Obj(
                "type"      -> Json.Str("function_call"),
                "call_id"   -> call.get("id").getOrElse(Json.Str("")),
                "name"      -> function.get("name").getOrElse(Json.Str("")),
                "arguments" -> function.get("arguments").getOrElse(Json.Str("{}")),
              )
            }
          case _ => Nil
        said ++ calls
      case _ => Nil

  private def responseTool(tool: Json.Obj): Option[Json] =
    val function = tool.get("function").collect { case obj: Json.Obj => obj }.getOrElse(tool)
    function.get("name").collect { case Json.Str(name) =>
      Json.Obj(
        "type"        -> Json.Str("function"),
        "name"        -> Json.Str(name),
        "description" -> function.get("description").getOrElse(Json.Str("")),
        "parameters"  -> function.get("parameters").getOrElse(Json.Obj()),
      )
    }
  end responseTool

  private def outputText(item: Json.Obj): Option[String] =
    item.get("type") match
      case Some(Json.Str("message")) => Some(text(item.get("content"))).filter(_.nonEmpty)
      case _                         => None

  private def functionCall(item: Json.Obj): Option[Json] =
    if item.get("type").contains(Json.Str("function_call")) then
      Some(
        Json.Obj(
          "id"       -> item.get("call_id").getOrElse(Json.Str("")),
          "type"     -> Json.Str("function"),
          "function" -> Json.Obj(
            "name"      -> item.get("name").getOrElse(Json.Str("")),
            "arguments" -> item.get("arguments").getOrElse(Json.Str("{}")),
          ),
        )
      )
    else None

  private def anthropicMessage(message: Json.Obj): List[Json] =
    message.get("role") match
      case Some(Json.Str("system")) | Some(Json.Str("developer")) => Nil
      case Some(Json.Str("tool"))                                 =>
        List(
          Json.Obj(
            "role"    -> Json.Str("user"),
            "content" -> Json.Arr(
              Json.Obj(
                "type"        -> Json.Str("tool_result"),
                "tool_use_id" -> message.get("tool_call_id").getOrElse(Json.Str("")),
                "content"     -> Json.Str(text(message.get("content"))),
              )
            ),
          )
        )
      case Some(Json.Str(role)) =>
        val spoken = text(message.get("content"))
        val blocks = List(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(spoken))).filter(_ => spoken.nonEmpty)
        val calls  = message.get("tool_calls") match
          case Some(Json.Arr(values)) =>
            values.toList.collect { case call: Json.Obj =>
              val function = call.get("function").collect { case obj: Json.Obj => obj }.getOrElse(Json.Obj())
              val input    = function
                .get("arguments")
                .collect { case Json.Str(raw) =>
                  Json.decoder.decodeJson(raw).getOrElse(Json.Obj())
                }
                .getOrElse(Json.Obj())
              Json.Obj(
                "type"  -> Json.Str("tool_use"),
                "id"    -> call.get("id").getOrElse(Json.Str("")),
                "name"  -> function.get("name").getOrElse(Json.Str("")),
                "input" -> input,
              )
            }
          case _ => Nil
        val content = blocks ++ calls
        if content.isEmpty then Nil
        else
          List(
            Json.Obj(
              "role"    -> Json.Str(if role == "assistant" then "assistant" else "user"),
              "content" -> Json.Arr(zio.Chunk.fromIterable(content)),
            )
          )
      case _ => Nil

  private def anthropicTool(tool: Json.Obj): Option[Json] =
    val function = tool.get("function").collect { case obj: Json.Obj => obj }.getOrElse(tool)
    function.get("name").collect { case Json.Str(name) =>
      Json.Obj(
        "name"         -> Json.Str(name),
        "description"  -> function.get("description").getOrElse(Json.Str("")),
        "input_schema" -> function.get("parameters").getOrElse(Json.Obj("type" -> Json.Str("object"))),
      )
    }

  private def blockText(block: Json.Obj): Option[String] =
    if block.get("type").contains(Json.Str("text")) then block.get("text").collect { case Json.Str(value) => value }
    else None

  private def toolUse(block: Json.Obj): Option[Json] =
    if block.get("type").contains(Json.Str("tool_use")) then
      val input = block.get("input").getOrElse(Json.Obj())
      Some(
        Json.Obj(
          "id"       -> block.get("id").getOrElse(Json.Str("")),
          "type"     -> Json.Str("function"),
          "function" -> Json.Obj(
            "name"      -> block.get("name").getOrElse(Json.Str("")),
            "arguments" -> Json.Str(input.toJson),
          ),
        )
      )
    else None

  private def number(obj: Json.Obj, key: String): Option[Int] =
    obj.get(key).collect { case Json.Num(value) => value.intValue }
end Shape
