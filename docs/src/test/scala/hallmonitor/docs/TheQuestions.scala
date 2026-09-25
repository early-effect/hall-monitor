package hallmonitor.docs

import hallmonitor.config.Load
import hallmonitor.domain.*
import hexis.{Answer, Probability}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object TheQuestions extends DocSpecSuite:
  def doc = page("The questions")(
    md"""
You edit the TOML file. There is no prompt screen.

Each `[[criteria]]` table is one question the monitor asks the classifier. The
`instructions` line is the question. The other fields are the answers that
question is allowed to give.

The harness sends the note: the chat messages, or the systemone state, with
tool names written on the front. The file supplies the questions. One knock
becomes one System One batch, and the batch holds only the questions whose
`faces` include that door.

The file is the one the process started with. The first argument wins, then
`HALL_MONITOR_CONFIG`, then `./hall-monitor.toml`. `POST /admin/reload` with
the Hall Monitor bearer re-reads that same file. A file that fails to parse
stays off the wall, and the questions already loaded keep running.

The first question means the file also has a `[classifier]` block. No
questions, and that block can stay out. The shell around the tables below is a
front-door key, a listen block, one conversational backend, and a classifier.
""",
    section("Yes or no")(
      md"""
`kind = "noul"` asks for a probability. `yes` and `no` are the two stories the
classifier is choosing between. Leave `faces` out and both doors hear it.

```toml
[[criteria]]
id = "pii"
kind = "noul"
instructions = "The request contains personal data, secrets, or regulated identifiers."
yes = "Names, emails, account numbers, health data, credentials, or private customer data appear."
no = "No personal or secret data."
```
""",
      exampleZIO {
        Load
          .fromString(
            file("""
          [[criteria]]
          id = "pii"
          kind = "noul"
          instructions = "The request contains personal data, secrets, or regulated identifiers."
          yes = "Names, emails, account numbers, health data, credentials, or private customer data appear."
          no = "No personal or secret data."
        """),
            _ => None,
          )
          .map(summarize)
      }.assert(line =>
        assertTrue(
          line == "pii noul doors=Conversational,Decision | The request contains personal data, secrets, or regulated identifiers. | yes: Names, emails, account numbers, health data, credentials, or private customer data appear. | no: No personal or secret data."
        )
      ),
    ),
    section("A scale")(
      md"""
`kind = "score"` asks where the note sits on a rubric. `levels` run from light
to heavy. Two to ten of them, with no repeats. `faces` keeps this one at the
conversation door.

```toml
[[criteria]]
id = "weight"
kind = "score"
instructions = "How heavy is the reasoning this request needs?"
levels = ["trivial", "light", "moderate", "heavy"]
faces = ["conversational"]
```
""",
      exampleZIO {
        Load
          .fromString(
            file("""
          [[criteria]]
          id = "weight"
          kind = "score"
          instructions = "How heavy is the reasoning this request needs?"
          levels = ["trivial", "light", "moderate", "heavy"]
          faces = ["conversational"]
        """),
            _ => None,
          )
          .map(summarize)
      }.assert(line =>
        assertTrue(
          line == "weight score doors=Conversational | How heavy is the reasoning this request needs? | trivial < light < moderate < heavy"
        )
      ),
    ),
    section("A labeled choice")(
      md"""
`kind = "choice"` asks the classifier to pick a label. The descriptions live in
`[criteria.options]`, and that table belongs to the question directly above it.
A later `[[criteria]]` starts a new question, and an options table after that
one labels the new question.

```toml
[[criteria]]
id = "task"
kind = "choice"
instructions = "What kind of work is this?"
faces = ["conversational"]

[criteria.options]
coding-light = "A small edit, rename, format, or short snippet."
coding-heavy = "Architecture, a large refactor, or multi-file design."
chat = "Conversation, explanation, or writing without a code change."
```
""",
      exampleZIO {
        Load
          .fromString(
            file("""
          [[criteria]]
          id = "task"
          kind = "choice"
          instructions = "What kind of work is this?"
          faces = ["conversational"]

          [criteria.options]
          coding-light = "A small edit, rename, format, or short snippet."
          coding-heavy = "Architecture, a large refactor, or multi-file design."
          chat = "Conversation, explanation, or writing without a code change."
        """),
            _ => None,
          )
          .map(summarize)
      }.assert(line =>
        assertTrue(
          line == "task choice doors=Conversational | What kind of work is this? | chat=Conversation, explanation, or writing without a code change., coding-heavy=Architecture, a large refactor, or multi-file design., coding-light=A small edit, rename, format, or short snippet."
        )
      ),
    ),
    section("The options table remembers the room")(
      md"""
Put the choice first and a yes/no after it, then write `[criteria.options]`.
The table labels the yes/no. The choice is left with an empty menu, and a
yes/no refuses options. Move the table back up so it sits on the choice.
""",
      exampleError {
        Load.fromString(
          file("""
          [[criteria]]
          id = "task"
          kind = "choice"
          instructions = "What kind of work is this?"
          faces = ["conversational"]

          [[criteria]]
          id = "pii"
          kind = "noul"
          instructions = "The request contains personal data."
          yes = "A name appears."
          no = "No personal data."

          [criteria.options]
          coding-light = "A small edit."
        """),
          _ => None,
        )
      }.assert { error =>
        val messages = error.messages
        assertTrue(
          messages.contains("criterion task choice options must be 1..255, got 0"),
          messages.contains("criterion pii cannot set options"),
        )
      },
    ),
    section("A question nobody's rule names")(
      md"""
A `[[rules]]` table names a question and says what the answer may do:
`yesAtLeast`, `choice`, `atLeast`, `atMost`. Until a rule names the question,
the monitor can ask it, write down a loud yes, and still hand the note to the
default model.
""",
      exampleValue {
        Hall.chosen(
          ModelKind.Conversational,
          Map(CriterionId("pii") -> Answer.Noul(Probability.unsafely(0.99))),
          rules = Hall.policy.copy(rules = Nil),
        )
      }.assert(name => assertTrue(name == Right("public-fast"))),
    ),
    section("Which file")(
      md"""
The first argument is the file. With no argument, `HALL_MONITOR_CONFIG` is the
file. With neither, the process reads `./hall-monitor.toml`.
""",
      exampleValue(Load.resolvePath(List("desk.toml"), _ => None).toString)
        .assert(path => assertTrue(path == "desk.toml")),
      exampleValue(Load.resolvePath(Nil, Map("HALL_MONITOR_CONFIG" -> "wall.toml").get).toString)
        .assert(path => assertTrue(path == "wall.toml")),
      exampleValue(Load.resolvePath(Nil, _ => None).toString)
        .assert(path => assertTrue(path == "hall-monitor.toml")),
    ),
  )

  private def file(criteria: String): String =
    s"""apiKey = "hm"
       |defaultPrefer = ["local"]
       |
       |[listen]
       |host = "127.0.0.1"
       |port = 8080
       |
       |[classifier]
       |baseUrl = "http://127.0.0.1:8091"
       |model = "local"
       |timeoutSeconds = 10
       |apiKey = "local"
       |
       |[[backends]]
       |id = "local"
       |kind = "conversational"
       |baseUrl = "http://127.0.0.1:9"
       |upstreamModel = "qwen"
       |apiKey = "ollama"
       |
       |${criteria.trim}
       |""".stripMargin

  private def summarize(loaded: hallmonitor.domain.Loaded): String =
    loaded.policy.criteria
      .map {
        case noul: Criterion.Noul =>
          s"${noul.id.value} noul doors=${doors(noul.faces)} | ${noul.instructions} | yes: ${noul.yes} | no: ${noul.no}"
        case choice: Criterion.Choice =>
          val menu = choice.options.sortBy(_._1).map((key, text) => s"$key=$text").mkString(", ")
          s"${choice.id.value} choice doors=${doors(choice.faces)} | ${choice.instructions} | $menu"
        case score: Criterion.Score =>
          s"${score.id.value} score doors=${doors(score.faces)} | ${score.instructions} | ${score.levels.mkString(" < ")}"
      }
      .mkString("\n")

  private def doors(faces: Set[ModelKind]): String =
    faces.toList.map(_.toString).sorted.mkString(",")
end TheQuestions
