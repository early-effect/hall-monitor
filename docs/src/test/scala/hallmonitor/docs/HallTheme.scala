package hallmonitor.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.{DocsSite, SiteBuilder, Theme, ThemeTokens}
import zio.*

/** Early Effect tokens, plus a gutter so the copy button does not sit on the code. */
object HallTheme:
  private val codeGutter: String =
    """
      |.specular-site-Theme-Content .specular-code > pre.specular-source {
      |  white-space: pre-wrap;
      |  overflow-wrap: break-word;
      |  padding-right: 3.25rem;
      |}
      |.specular-site-Theme-Content .specular-result pre {
      |  white-space: pre-wrap;
      |  overflow-wrap: break-word;
      |}
      |""".stripMargin

  val tokens: ThemeTokens =
    val base = EarlyEffectTheme.tokens
    base.copy(extraCss = base.extraCss + codeGutter)

  val layers: ZLayer[Any, Nothing, SiteBuilder] =
    Theme.fromTokens(tokens) >>> DocsSite.themedStack
end HallTheme
