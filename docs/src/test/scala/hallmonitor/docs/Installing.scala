package hallmonitor.docs

import specular.*
import specular.ziotest.DocSpecSuite

object Installing extends DocSpecSuite:
  def doc = page("Installing")(
    md"""
The released program is a jar on Maven Central. Coursier fetches that jar and
the libraries in its POM, then runs `hallmonitor.Main`. The version in the
coordinate is the git tag: `v0.1.0` publishes `0.1.0`.

You still bring the TOML file. Coursier does not ship `examples/hall-monitor.toml`.
With no argument the process reads `HALL_MONITOR_CONFIG`, then `./hall-monitor.toml`.
""",
    section("Launch")(
      md"""
One shot, in the foreground:

```bash
cs launch rocks.earlyeffect::hall-monitor:0.1.0 -- hall-monitor.toml
```

`::` is the Scala 3 artifact, `hall-monitor_3`. Anything after `--` is the
program's own arguments. The first one is the config file.
"""
    ),
    section("A launcher you keep")(
      md"""
`cs bootstrap` writes a small program that downloads the same jars on first
run and then starts them:

```bash
cs bootstrap rocks.earlyeffect::hall-monitor:0.1.0 -o hall-monitor -f
./hall-monitor hall-monitor.toml
```

`-f` replaces an older copy at that path. Put `./hall-monitor` somewhere on
your `PATH` if you want the name by itself.
"""
    ),
    section("The short name")(
      md"""
`cs install scalafmt` works because `scalafmt` is a name in [Coursier's default
channel](https://github.com/coursier/apps). `cs install hall-monitor` looks at
that same list. Hall Monitor is not on it, so the bare command does not find
a jar.

`cs launch` and `cs bootstrap` skip the list and take the Maven coordinate.

A short name is a small JSON descriptor. This repo keeps one at
`coursier/apps.json`:

```json
{
  "hall-monitor": {
    "repositories": ["central"],
    "dependencies": ["rocks.earlyeffect::hall-monitor:latest.release"],
    "mainClass": "hallmonitor.Main"
  }
}
```

`latest.release` follows the newest version on Central. Once that file is on
`main`, the channel is its raw URL:

```bash
cs install --channel https://raw.githubusercontent.com/early-effect/hall-monitor/main/coursier/apps.json hall-monitor
hall-monitor hall-monitor.toml
```

The bare `cs install hall-monitor`, with no `--channel`, is a pull request to
[coursier/apps](https://github.com/coursier/apps). The contrib list is the
usual place. Until that pull request is merged, pass the channel.
"""
    ),
    section("From a checkout")(
      md"""
Working on this repository does not need Central:

```bash
sbt "app/run examples/hall-monitor.toml"
```

That forks the same `hallmonitor.Main`. Stopping it does not stop sbt.
"""
    ),
    section("Stopping")(
      md"""
The process stays in the terminal where you started it. The log line says
`Ctrl-C to stop`. Ctrl-C stops it. So does `kill` on that process. The listen
port comes back. A config file that does not load exits on its own, with a
message, and status 1.
"""
    ),
  )
end Installing
