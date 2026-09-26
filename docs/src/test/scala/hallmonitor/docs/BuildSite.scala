package hallmonitor.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.Path

object BuildSite extends DocsSite:
  def pages =
    Vector(
      TheHallway.doc,
      Installing.doc,
      TwoDoors.doc,
      TheNote.doc,
      TheQuestions.doc,
      HouseRules.doc,
      Keys.doc,
      AHarness.doc,
      TheTape.doc,
    )

  override def site: SiteModel =
    val branded = EarlyEffectTheme.brand(super.site)
    branded.copy(
      summaryMarkdown = Some(
        """**Hall Monitor** stands in the hallway with a clipboard. A harness knocks
like it would on any provider. Jev reads the note, your rules open the door,
and the model behind it does the talking.
"""
      ),
      brand = Some(
        Brand(
          name = "Hall Monitor",
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/hall-monitor")),
        )
      ),
      installSnippets = Vector(
        CodeSnippet(
          "Launch",
          "cs launch rocks.earlyeffect::hall-monitor:0.1.0 -- hall-monitor.toml",
        ),
        CodeSnippet(
          "Launcher",
          """cs bootstrap rocks.earlyeffect::hall-monitor:0.1.0 -o hall-monitor -f
            |./hall-monitor hall-monitor.toml""".stripMargin,
        ),
        CodeSnippet(
          "Harness",
          """OpenAI base URL  http://127.0.0.1:8080/v1
            |model            hall-monitor
            |Hexis baseUrl    http://127.0.0.1:8080/jev""".stripMargin,
        ),
      ),
    )
  end site

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    HallTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): Task[Unit] =
    val _ = result
    EarlyEffectTheme.writeLogo(out)
end BuildSite
