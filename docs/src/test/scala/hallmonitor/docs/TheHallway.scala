package hallmonitor.docs

import specular.*
import specular.ziotest.DocSpecSuite

object TheHallway extends DocSpecSuite:
  def doc = page("The hallway")(
    md"""
You hired someone to stand in the hallway.

They have a clipboard, a bell, and a firm lack of interest in your essay. A
harness knocks the way it knocks on any provider. Hall Monitor reads the note,
checks the rules on the wall, and points at a door.

One door is conversation: Grok, and anyone else who speaks OpenAI chat
completions. The other is decisions: Jev, and anyone else who speaks TypeSafe
System One. You pick the door with the URL. The monitor picks who is standing
behind it, and they will send a fast model back to its room when the rules say
so. Charm is not a routing criterion.

The questions on the wall are yours, and you write them in the file. Each one
is a `[[criteria]]` table. PII. How heavy the thinking is. What kind of work
this is. The one you invent on a Thursday. A System One model answers them.
Your rules decide what those answers are allowed to do.

```bash
sbt "app/run examples/hall-monitor.toml"
```

Point a chat harness at `http://127.0.0.1:8080/v1` with the model `hall-monitor`,
or point hexis at `http://127.0.0.1:8080/jev`. The key on the clipboard is yours.
Upstream keys stay in the office.
"""
  )
end TheHallway
