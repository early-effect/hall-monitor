package hallmonitor

import hallmonitor.domain.ChatCompat
import hallmonitor.forward.{CommandAuth, Shape}
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object WireSpec extends ZIOSpecDefault:
  def spec = suite("upstream wires")(
    test("bedrock chat rewrites the developer role and the token field") {
      val raw = Json.Obj(
        "messages"              -> Json.Arr(Json.Obj("role" -> Json.Str("developer"), "content" -> Json.Str("rules"))),
        "max_completion_tokens" -> Json.Num(12),
      )
      val out = Shape.chat(raw, ChatCompat(rewriteDeveloper = true, maxTokensField = Some("max_tokens")))
      assertTrue(
        out.get("max_tokens").contains(Json.Num(12)),
        out.get("max_completion_tokens").isEmpty,
        out
          .get("messages")
          .contains(
            Json.Arr(Json.Obj("role" -> Json.Str("system"), "content" -> Json.Str("rules")))
          ),
      )
    },
    test("responses keeps the system text as instructions") {
      val raw = Json.Obj(
        "messages" -> Json.Arr(
          Json.Obj("role" -> Json.Str("system"), "content" -> Json.Str("be brief")),
          Json.Obj("role" -> Json.Str("user"), "content"   -> Json.Str("hi")),
        )
      )
      val out = Shape.responses(raw, "grok-4.7")
      assertTrue(
        out.get("model").contains(Json.Str("grok-4.7")),
        out.get("instructions").contains(Json.Str("be brief")),
        out.get("stream").contains(Json.Bool(false)),
      )
    },
    test("a responses message comes back as a chat completion") {
      val raw = Json.Obj(
        "id"     -> Json.Str("resp_1"),
        "output" -> Json.Arr(
          Json.Obj(
            "type"    -> Json.Str("message"),
            "content" -> Json.Arr(Json.Obj("type" -> Json.Str("output_text"), "text" -> Json.Str("hello"))),
          )
        ),
      )
      val out = Shape.fromResponses("grok-4.7", raw)
      assertTrue(out.toJson.contains("hello"), out.toJson.contains("chat.completion"))
    },
    test("anthropic messages keep the system text out of the dialog") {
      val raw = Json.Obj(
        "messages" -> Json.Arr(
          Json.Obj("role" -> Json.Str("system"), "content" -> Json.Str("be brief")),
          Json.Obj("role" -> Json.Str("user"), "content"   -> Json.Str("hi")),
        )
      )
      val out = Shape.messages(raw, "us.anthropic.claude-opus-5-5")
      assertTrue(
        out.get("system").contains(Json.Str("be brief")),
        out.get("model").contains(Json.Str("us.anthropic.claude-opus-5-5")),
      )
    },
    test("an auth command's json is a bearer and an expiry") {
      val parsed = CommandAuth.parse("""{"access_token":"bedrock-token","expires_in":3600}""")
      assertTrue(parsed.map(_.token).contains("bedrock-token"), parsed.exists(_.expiresAtMs > 0L))
    },
  )
end WireSpec
