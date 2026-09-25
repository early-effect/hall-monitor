package hallmonitor.docs

import hallmonitor.domain.*
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object TwoDoors extends DocSpecSuite:
  def doc = page("Two doors")(
    md"""
Two doors. Two languages. A visitor who tries to order a decision from the chat
window gets a 404 and a story to tell their other harnesses.

| Knock | Who answers |
| --- | --- |
| `POST /v1/chat/completions` | A conversational model |
| `GET /v1/models` | The conversational board, plus `hall-monitor` |
| `POST /jev/v1/systemone` | A decision model |
| `GET /jev/v1/models` | The decision board |

OpenAI clients use base URL `http://127.0.0.1:8080/v1`. Hexis uses
`http://127.0.0.1:8080/jev`, because hexis likes to append `/v1/systemone` and
`/v1/models` on its own way down the hall. One process, two lists, and nobody
has to argue about the shape of the JSON.

A chat completion wants words. A systemone call wants a choice, a score, or a
yes. `public-fast` lives behind the chat door. `jev-vpc` lives behind the
decision door. Ask the chat door for `jev-vpc` and the monitor checks the
conversational board, fails to find the name, and says so out loud.
""",
    exampleValue {
      Hall.chosen(ModelKind.Conversational, Map.empty, rules = Hall.policy.copy(rules = Nil))
    }.assert(name => assertTrue(name == Right("public-fast"))),
    exampleValue {
      Hall.chosen(
        ModelKind.Decision,
        Map.empty,
        rules = Hall.policy.copy(rules = Nil, defaultPrefer = List(BackendId("jev-vpc"))),
      )
    }.assert(name => assertTrue(name == Right("jev-vpc"))),
    exampleValue {
      Hall.chosen(ModelKind.Conversational, Map.empty, Some("jev-vpc"), rules = Hall.policy.copy(rules = Nil))
    }.assert {
      case Left(RouteError.UnknownModel("jev-vpc")) => assertTrue(true)
      case other => assertTrue(false) && assertTrue(other.toString == "expected unknown model")
    },
  )
end TwoDoors
