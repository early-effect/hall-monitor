package hallmonitor.docs

import hallmonitor.domain.{ModelKind, Names, RouteError}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object AHarness extends DocSpecSuite:
  def doc = page("A harness")(
    md"""
The harness keeps its own habits. You change two settings and a model name,
then go back to whatever you were building.

| Setting | Value |
| --- | --- |
| OpenAI base URL | `http://127.0.0.1:8080/v1` |
| OpenAI model | `hall-monitor`, or a name on the conversational board |
| Hexis `baseUrl` | `http://127.0.0.1:8080/jev` |
| API key | The Hall Monitor key |

`hall-monitor` means the monitor chooses. A backend id, or an alias such as
`grok-fast`, means "this one", and the locks still get a vote.

Hexis arrives carrying a model name from home, usually `jev-latest`. Leave that
nickname off the decision board and the answer is 404. The monitor will not
translate a provider's pet name into "surprise me." Set the hexis model to
`hall-monitor`, or to a decision backend you listed on purpose.

`GET /health` is the one door with no key, so a supervisor can ask whether
anyone is on duty. Every other knock wants `Authorization: Bearer` and the
front-door key.

`POST /admin/reload` rings the bell. A good file is the rules for the next
visitor. A file that fails to parse leaves the current rules on the wall and
comes back with the reason, which is the polite version of "try again."
""",
    exampleValue(Names.Auto).assert(name => assertTrue(name == "hall-monitor")),
    exampleValue {
      Hall.chosen(ModelKind.Decision, Map.empty, Some("jev-latest"), rules = Hall.policy.copy(rules = Nil))
    }.assert {
      case Left(RouteError.UnknownModel("jev-latest")) => assertTrue(true)
      case other                                       => assertTrue(other.toString.isEmpty)
    },
  )
end AHarness
