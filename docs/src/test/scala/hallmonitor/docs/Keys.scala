package hallmonitor.docs

import hallmonitor.config.{ConfigError, Load}
import specular.*
import specular.ziotest.{DocSpecSuite, DocTestInterpreter}
import zio.test.*

object Keys extends DocSpecSuite:
  def doc = page("Keys")(
    md"""
Every secret is one of two sticky notes.

| Field | What you write |
| --- | --- |
| `apiKey` | The token, written out. Fine for a laptop that never leaves the house. |
| `apiKeyEnv` | The name of the environment variable that holds the token. |

One of them. The front door, the classifier, and every model use the same pair.
The harness shows the front-door key. Upstream keys stay in the process. When
the file is wrong, the error names the drawer. It leaves the token out of the
message, which is a mercy for anyone reading the log later.

TOML remembers which room you walked into. Keys for the whole building go
before the first table.

```toml
apiKeyEnv = "HALL_MONITOR_API_KEY"
maxStateChars = 16000
upstreamIdleSeconds = 300
defaultPrefer = ["public-fast"]

[listen]
host = "127.0.0.1"
port = 8080
```

Write `apiKeyEnv` under `[listen]` and you have labeled the listen block. The
front door still has an empty hook. The monitor mentions that before anyone
gets in.
"""
  )

  override def spec =
    val story  = DocTestInterpreter.specOf(this).provideLayer(ExampleRunner.live)
    val checks = suite("Keys")(
      test("the front door key is a root key, written before any table") {
        Load.fromString(frontDoor, Map("HALL_MONITOR_API_KEY" -> "hm").get).map { loaded =>
          assertTrue(loaded.apiKey.reveal == "hm")
        }
      },
      test("a key tucked under listen is not the front door") {
        Load.fromString(tuckedUnderListen, _ => None).flip.map { error =>
          assertTrue(error.messages.contains(ConfigError.SecretNeither("hall-monitor").message))
        }
      },
    )
    story + checks
  end spec

  private val frontDoor: String =
    """
      |apiKeyEnv = "HALL_MONITOR_API_KEY"
      |defaultPrefer = ["local"]
      |[listen]
      |host = "127.0.0.1"
      |port = 8080
      |[[backends]]
      |id = "local"
      |kind = "conversational"
      |baseUrl = "http://127.0.0.1:9"
      |upstreamModel = "qwen"
      |apiKey = "ollama"
      |""".stripMargin

  private val tuckedUnderListen: String =
    """
      |[listen]
      |host = "127.0.0.1"
      |port = 8080
      |apiKey = "this-labels-listen"
      |[[backends]]
      |id = "local"
      |kind = "conversational"
      |baseUrl = "http://127.0.0.1:9"
      |upstreamModel = "qwen"
      |apiKey = "ollama"
      |""".stripMargin
end Keys
