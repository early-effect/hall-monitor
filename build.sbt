import zipx.plugin.ZipxPlugin.autoImport.*

MyVersions.settings

ThisBuild / organization         := "rocks.earlyeffect"
ThisBuild / organizationName     := "Early Effect"
ThisBuild / organizationHomepage := Some(uri("https://www.earlyeffect.rocks"))
ThisBuild / homepage             := Some(uri("https://github.com/early-effect/hall-monitor"))
ThisBuild / licenses             := List("Apache-2.0" -> uri("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / versionScheme        := Some("early-semver")
ThisBuild / scmInfo              := Some(
  ScmInfo(
    uri("https://github.com/early-effect/hall-monitor"),
    "scm:git@github.com:early-effect/hall-monitor.git",
  )
)
ThisBuild / developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = uri("https://github.com/russwyte"),
  )
)

lazy val app = project
  .in(file("app"))
  .settings(MyVersions.appLib, MyVersions.appTest)
  .settings(
    name := "hall-monitor",
    description := "A hallway monitor for model calls. Jev reads the note, your rules open the door, and the harness sees an ordinary provider.",
    Compile / mainClass  := Some("hallmonitor.Main"),
    Compile / run / fork := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all"),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    publishMavenStyle    := true,
    pomIncludeRepository := { _ => false },
    publishTo            := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
      else localStaging.value
    },
    // CI-only signing. PGP_KEY_HEX is the org secret in the generated release job.
    // MISSING_KEY_HEX keeps a local load working and makes a laptop publish fail loudly.
    usePgpKeyHex(sys.env.getOrElse("PGP_KEY_HEX", "MISSING_KEY_HEX")),
  )

lazy val docs = project
  .in(file("docs"))
  .dependsOn(app)
  .enablePlugins(SpecularPlugin)
  .settings(
    name                                            := "hall-monitor-docs",
    publish / skip                                  := true,
    publishArtifact                                 := false,
    zipxPublish                                     := Some(false),
    libraryDependencySchemes += "rocks.earlyeffect" %% "heddle" % VersionScheme.Always,
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all", "-language:implicitConversions"),
    MyVersions.docsTest,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Test / mainClass       := Some("specular.site.DocsServe"),
    Test / run / mainClass := (Test / mainClass).value,
    specularBuildMain      := "hallmonitor.docs.BuildSite",
    specularMetaProject    := Some(LocalProject("app")),
    specularArtifactKind   := "library",
    specularSiteDirectory  := (ThisBuild / baseDirectory).value / "target" / "site",
    specularDisplayVersion := stripCi,
  )

lazy val root = project
  .in(file("."))
  .aggregate(app, docs)
  .settings(
    name                 := "hall-monitor-root",
    publish / skip       := true,
    zipxPublish          := Some(false),
    zipxJavaVersion      := JdkVersion("25"),
    zipxWorkflowDispatch := true,
    zipxCapabilities ++= Seq(
      ZipxCentral.release.withCondition(JobCondition.repositoryIs("early-effect/hall-monitor")),
      ZipxDocs.pages().andCondition(JobCondition.repositoryIs("early-effect/hall-monitor")),
    ),
  )

addCommandAlias("docsPreview", "~docs/specularPreview")
