package hallmonitor.docs

import specular.*
import specular.ziotest.DocSpecSuite

object TheLog extends DocSpecSuite:
  def doc = page("The log")(
    md"""
Every knock that gets past the door writes one line. The line is a JSON
object. It names the locks that shut, the models that were full or down or
over a stop line, who actually answered, and how long the classifier and each
attempt took.

The note is not in the line. Neither are keys. The answers are. `pii` at
`0.95` can tell you what kind of note it was. That is the decision.

```json
{
  "id": "7f3a",
  "face": "conversational",
  "answers": [{ "criterion": "pii", "kind": "noul", "probability": 0.95 }],
  "steps": [
    {
      "kind": "constraint",
      "id": "pii-lock",
      "ruling": "applies",
      "narrowed": true,
      "remaining": ["local-strong"]
    }
  ],
  "attempts": [
    { "backend": "local-strong", "result": "unreachable", "ms": 1004 }
  ],
  "served": null,
  "classifyMs": 40,
  "totalMs": 1050
}
```

A successful attempt's `ms` is the whole buffered completion. The tape page
says why that is not time to first token.

`GET /admin/calls` with the front-door key returns the same objects, oldest
first, from the process. The terminal is the copy that outlives a scroll.
`GET /admin/pool` says which models last looked up or down. A down model stays
on the board. The next check puts it back.
"""
  )
end TheLog
