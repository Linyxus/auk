package auk.config

import auk.platform.{Platform, PathOps}
import scala.util.control.NonFatal

/** The `[model]` section of the config: which provider and model to use. Both
  * fields are optional; absent ones fall back to environment overrides and then
  * the built-in catalog default (see `auk.llm.provider.ModelSelection`).
  */
case class ModelConfig(
    provider: Option[String],
    id: Option[String]
) derives ConfigSchema

/** Auk's on-disk configuration, parsed from `.auk/config` in the working
  * directory. Every section is optional, so a missing or empty file yields
  * [[AppConfig.empty]] and the app runs entirely on defaults.
  */
case class AppConfig(
    model: Option[ModelConfig]
) derives ConfigSchema

object AppConfig:
  /** Location of the config file, relative to the working directory. */
  val RelativePath = ".auk/config"

  /** The all-defaults configuration (no file present). */
  val empty: AppConfig = AppConfig(None)

  /** Load and parse `.auk/config` under `dir` (the working directory by default).
    * An absent file is [[empty]] (not an error); a present-but-unreadable file or
    * a malformed one yields the accumulated [[ConfigError]]s.
    */
  def load(dir: String = Platform.cwd()): Either[List[ConfigError], AppConfig] =
    val path = PathOps.join(dir, RelativePath)
    if !Platform.fs.exists(path) then Right(empty)
    else
      try Config.parse[AppConfig](Platform.fs.readString(path))
      catch
        case NonFatal(e) =>
          Left(List(ConfigError(s"could not read $path: ${e.getMessage}")))

  /** Serialize a config back to the `.auk/config` text format. */
  def render(config: AppConfig): String =
    val sb = new StringBuilder
    config.model.foreach { m =>
      sb.append("[model]\n")
      m.provider.foreach(p => sb.append(s"provider = $p\n"))
      m.id.foreach(id => sb.append(s"id = $id\n"))
    }
    sb.toString

  /** Write `config` to `.auk/config` under `dir`, creating the directory. */
  def save(config: AppConfig, dir: String = Platform.cwd()): Either[List[ConfigError], Unit] =
    val path = PathOps.join(dir, RelativePath)
    try
      PathOps.parent(path).foreach(Platform.fs.createDirectories)
      Platform.fs.writeString(path, render(config))
      Right(())
    catch
      case NonFatal(e) =>
        Left(List(ConfigError(s"could not write $path: ${e.getMessage}")))
