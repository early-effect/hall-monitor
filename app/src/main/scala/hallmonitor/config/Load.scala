package hallmonitor.config

import hallmonitor.domain.Loaded
import zio.config.*
import zio.config.toml.*
import zio.{ConfigProvider, IO, ZIO}

import java.nio.file.{Files, Path}

enum LoadError:
  case Parse(detail: String)
  case Config(detail: String)
  case Invalid(errors: List[ConfigError])

  def messages: List[String] =
    this match
      case Parse(detail)   => List(detail)
      case Config(detail)  => List(detail)
      case Invalid(errors) => errors.map(_.message)

  def render: String = messages.mkString("\n")
end LoadError

object Load:
  def resolvePath(args: List[String], env: String => Option[String]): Path =
    val chosen = args.headOption
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(env(Env).map(_.trim).filter(_.nonEmpty))
      .getOrElse(DefaultName)
    Path.of(chosen)

  def fromFile(path: Path, env: String => Option[String]): IO[LoadError, Loaded] =
    if !Files.isRegularFile(path) then ZIO.fail(LoadError.Parse(s"config not found: $path"))
    else
      ZIO
        .attempt(ConfigProvider.fromTomlPath(path))
        .mapError(parseFailure)
        .flatMap(load(_, env))

  def fromString(text: String, env: String => Option[String]): IO[LoadError, Loaded] =
    ZIO
      .attempt(ConfigProvider.fromTomlString(text))
      .mapError(parseFailure)
      .flatMap(load(_, env))

  private def load(provider: ConfigProvider, env: String => Option[String]): IO[LoadError, Loaded] =
    provider
      .load(AppRaw.descriptor)
      .mapError(error => LoadError.Config(error.prettyPrint()))
      .flatMap { raw =>
        ZIO.fromEither(Validate(raw, env)).mapError(LoadError.Invalid(_))
      }

  private def parseFailure(cause: Throwable): LoadError =
    LoadError.Parse(Option(cause.getMessage).getOrElse(cause.toString))

  private val Env: String         = "HALL_MONITOR_CONFIG"
  private val DefaultName: String = "hall-monitor.toml"
end Load
