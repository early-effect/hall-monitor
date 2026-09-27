import sbt.librarymanagement.syntax.*
import zipx.*

/** Typed catalog. `zipxDepUpdate` rewrites constructors here. sbt-zipx is not a row. */
object MyVersions extends ZipxVersions:
  val sbt: SbtVersion     = SbtVersion("2.1.0-M2")
  val scala: ScalaVersion = ScalaVersion("3.9.0")

  val zio           = Lib("dev.zio", "zio", "2.1.26")
  val zioStreams    = zio.mod("zio-streams")
  val zioTest       = zio.mod("zio-test").test
  val zioTestSbt    = zio.mod("zio-test-sbt").test
  val zioJson       = Lib("dev.zio", "zio-json", "1.1.0")
  val zioConfig     = Lib("dev.zio", "zio-config", "4.1.0")
  val zioConfigToml = zioConfig.mod("zio-config-toml")

  val heddle = Lib("rocks.earlyeffect", "heddle", "0.4.1")
  val hexis  = Lib("rocks.earlyeffect", "hexis", "0.1.0")

  val specular        = Lib("rocks.earlyeffect", "specular-core", "0.17.0")
  val specularZioTest = specular.mod("specular-zio-test").test
  val specularSite    = specular.mod("specular-site").test
  val specularTheme   = specular.mod("early-effect-docs-theme").test

  val scalafmt       = Plugin("org.scalameta", "sbt-scalafmt", "2.6.2")
  val dynver         = Plugin("com.github.sbt", "sbt-dynver", "5.1.1")
  val specularPlugin = Plugin("rocks.earlyeffect", "sbt-specular", "0.17.0")

  def appLib   = library(zio, zioStreams, zioJson, zioConfig, zioConfigToml, heddle, hexis)
  def appTest  = library(zioTest, zioTestSbt)
  def docsTest = library(zioTest, zioTestSbt, specularZioTest, specularSite, specularTheme)
end MyVersions
