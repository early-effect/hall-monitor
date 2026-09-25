package hallmonitor.docs

import hallmonitor.domain.*
import hexis.{Answer, Probability}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

object TheNote extends DocSpecSuite:
  def doc = page("The note")(
    md"""
There is no way to know a note has a phone number on it without reading the
note. Sorry. That part is physics.

The classifier is the person with that job. You hand them the questions you
wrote, one `[[criteria]]` table each: the instructions, and the answers that
question is allowed to give. The questions page is the recipe. They hand back
probabilities, then sit down. Writing the reply is someone else's afternoon,
and the classifier's name stays off both boards.

```toml
[classifier]
baseUrl = "http://127.0.0.1:8091"
model = "jev-latest"
timeoutSeconds = 10
apiKey = "local"
```

That block is a System One endpoint. Hexis posts it to `/v1/systemone`. Keep a
small one on the same machine when the note should stay in the building. Point
the block at `https://api.typesafe.ai` when sending the note out is a choice
you are making with your eyes open. The address is written where you can see
it. It is a lousy secret if it hides inside a backend id.

Leave the block out while the wall has no questions. The moment you add a
criterion, name who answers it. The monitor will refuse to guess.

A note longer than `maxStateChars` arrives as its opening pages. Every yes/no
lock stays shut. A half-read note is a lousy clean bill of health, and the
monitor has met that trick before.
""",
    exampleValue {
      val answers = Map[CriterionId, Answer[?]](
        CriterionId("pii") -> Answer.Noul(Probability.unsafely(0.0))
      )
      Hall.chosen(
        ModelKind.Conversational,
        answers,
        truncated = true,
        rules = Hall.policy.copy(rules = List(Hall.policy.rules.head)),
      )
    }.assert(name => assertTrue(name == Right("local-fast"))),
  )
end TheNote
