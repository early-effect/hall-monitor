package hallmonitor.docs

import hallmonitor.domain.ModelKind
import mermoid.ascent.MermoidAscent
import mermoid.css.ThemeName
import mermoid.{ContainerFit, Diagram, LayoutConfig, Mermaid, RenderConfig, ResponsiveConfig, StateStatement}
import specular.*
import specular.ziotest.DocSpecSuite
import zio.test.*

/** The site front. Specular still emits an Index plus a second copy of the nav. [[BuildSite]] copies this page over it.
  *
  * Not a sidebar item.
  */
object Front extends DocSpecSuite:

  /** The two [[ModelKind]] doors. The URL is the transition. There is no third door. */
  private val doors =
    Mermaid("""stateDiagram-v2
              |  direction TB
              |  classDef door fill:#2a2b2e,stroke:#c46a52,color:#e8e6dc
              |  [*] --> Conversational: /v1/chat/completions
              |  [*] --> Decision: /jev/v1/systemone
              |  class Conversational,Decision door
              |""".stripMargin)

  /** Phone column is about 390px, with 2rem of content padding on each side. Fit may scale down to 0.5. */
  private val fitFloor = 0.5

  private val shown =
    RenderConfig(
      layout = LayoutConfig(
        padding = 12,
        fontSize = 14,
        edgeLabelFontSize = 12,
        lineHeight = 16,
        hSpacing = 28,
        vSpacing = 40,
        minNodeWidth = 64,
        nodeHeight = 36,
        nodePaddingH = 14,
      ),
      theme = ThemeName.Dark,
      responsive = ResponsiveConfig(fit = ContainerFit.ToWidth(fitFloor)),
    )

  def coordinate: String =
    s"cs launch rocks.earlyeffect::hall-monitor:${DocsVersion.version} -- hall-monitor.toml"

  def doc = page("Someone has to stand in the hallway.")(
    illustration(MermoidAscent.diagram(doors, shown)).assert { ui =>
      val text = ui.toString
      assertTrue(
        text.contains("mermoid-root"),
        text.contains("mermoid-fit"),
        text.contains("Conversational"),
        text.contains("Decision"),
        text.contains("/v1/chat/completions"),
        text.contains("/jev/v1/systemone"),
      )
    },
    section("Install")(
      md"""
```bash
$coordinate
```
"""
    ),
    section("The doors")(
      md"""
The URL picks the door. `ModelKind` is the two of them.
""",
      cite[ModelKind].definition,
    ),
  )

  override def spec =
    suite("Front")(
      super.spec,
      test("install coordinate is 0.1.0") {
        val version = DocsVersion.version
        assertTrue(
          version == DocsVersion.published,
          coordinate.contains(s"hall-monitor:$version"),
          coordinate.contains("rocks.earlyeffect::hall-monitor:"),
          !coordinate.contains("0.1.1"),
          !coordinate.contains("-ci"),
          !coordinate.contains("SNAPSHOT"),
          !coordinate.contains("+"),
          !coordinate.contains("<version>"),
          !coordinate.contains("addSbtPlugin"),
          DocsVersion.advertise("0.1.1") == DocsVersion.published,
          DocsVersion.advertise("0.1.1-ci") == DocsVersion.published,
          DocsVersion.advertise("0.1.1-SNAPSHOT") == DocsVersion.published,
          DocsVersion.advertise("0.1.0+4-abcdef") == DocsVersion.published,
          DocsVersion.advertise("0.1.0") == "0.1.0",
          DocsVersion.advertise("0.2.0") == DocsVersion.published,
        )
      },
      test("the figure is the two doors, and fit keeps it inside a phone column") {
        val targets = doorTargets(doors.diagram)
        val kinds   = ModelKind.values.map(_.toString).toList
        val width   = sceneWidth(MermoidAscent.diagram(doors, shown))
        // 390px viewport, 2rem padding each side. 1280px viewport keeps a 240px sidebar and the same padding.
        val phone   = width.exists(w => w > 0 && w * fitFloor <= 320)
        val desktop = width.exists(w => w > 0 && w * fitFloor <= 960)
        assertTrue(
          kinds == List("Conversational", "Decision"),
          targets == kinds,
          doors.source.contains("stateDiagram-v2"),
          phone,
          desktop,
        ).label(s"scene width $width")
      },
    )

  private def doorTargets(diagram: Diagram): List[String] =
    diagram match
      case Diagram.StateDiagram(_, statements) =>
        statements.collect { case StateStatement.TransitionSt(transition) => transition.to.value }
      case _ => Nil

  private def sceneWidth(ui: ascent.ast.UI[Any]): Option[Double] =
    ui match
      case element: ascent.ast.UI.Element[?] =>
        val here = element.attrs.collectFirst {
          case ascent.ast.Attr.StaticAttr("data-mermoid-width", ascent.domtypes.AttrValue.Str(raw)) =>
            raw.toDoubleOption
        }.flatten
        here.orElse(element.children.iterator.map(sceneWidth).collectFirst { case Some(w) => w })
      case ascent.ast.UI.Fragment(children) =>
        children.iterator.map(sceneWidth).collectFirst { case Some(w) => w }
      case _ => None
end Front
