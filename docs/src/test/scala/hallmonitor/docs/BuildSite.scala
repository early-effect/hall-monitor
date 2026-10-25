package hallmonitor.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*
import zio.*

import java.nio.file.{Files, Path, StandardCopyOption}

object BuildSite extends DocsSite:

  /** The guide. The front page is not in this list, so it is not a second sidebar item. */
  private val guide: Vector[specular.DocPage] =
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
      TheLog.doc,
    )

  /** Not a nav item. The sidebar is the guide; this page is the front. */
  private val frontPage: specular.DocPage = Front.doc

  def pages: Vector[specular.DocPage] = frontPage +: guide

  override def site(settings: DocsSettings): SiteModel =
    val branded = EarlyEffectTheme.brand(super.site(settings))
    branded.copy(
      nav = Some(NavModel.flat(guide)),
      pages = pages,
      summaryMarkdown = None,
      installSnippets = Vector(CodeSnippet("Launch", Front.coordinate)),
      brand = Some(
        Brand(
          name = "Hall Monitor",
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/hall-monitor")),
        )
      ),
    )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    HallTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    // Specular always writes index.html as a summary plus a second copy of the nav.
    // The front DocPage is the index. Copy it over that file. Asset paths stay site-relative.
    val front   = out.resolve(s"${frontPage.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out)
end BuildSite
