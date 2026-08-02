package auk.config

import auk.platform.{Platform, PathOps}
import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal

/** The `[model]` section of the config: which provider and model to use. Both
  * fields are optional; absent ones fall back to environment overrides and then
  * the built-in catalog default (see `auk.llm.provider.ModelSelection`).
  */
case class ModelConfig(
    provider: Option[String],
    id: Option[String]
) derives ConfigSchema

/** One `[mcp.servers.<name>]` section: a stdio MCP server auk may launch.
  *
  * `command` is the executable (e.g. `npx`), `args` its argv tail, and `env`
  * process environment overrides layered on top of auk's own environment when
  * the child is spawned. Only `command` is mandatory, so the common
  * single-binary server is a one-liner. `args` is an `Option` because an absent
  * list is indistinguishable from an empty one at the leaf level; `env` is an
  * open section and defaults to empty on its own.
  */
case class McpServerEntry(
    command: String,
    args: Option[List[String]],
    env: VectorMap[String, String]
) derives ConfigSchema

/** The `[mcp]` section. Server declaration order is significant — it is the
  * order servers (and hence their tools) are exposed to agents — so it is
  * carried by a [[VectorMap]] all the way from the file.
  */
case class McpSection(
    servers: VectorMap[String, McpServerEntry]
) derives ConfigSchema

/** Auk's on-disk configuration, parsed from `.auk/config` in the working
  * directory. Every section is optional, so a missing or empty file yields
  * [[AppConfig.empty]] and the app runs entirely on defaults.
  */
case class AppConfig(
    model: Option[ModelConfig],
    mcp: Option[McpSection]
) derives ConfigSchema

object AppConfig:
  /** Location of the config file, relative to the working directory. */
  val RelativePath = ".auk/config"

  /** The all-defaults configuration (no file present). */
  val empty: AppConfig = AppConfig(None, None)

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

  /** Serialize a config back to the `.auk/config` text format.
    *
    * Rendering must cover EVERY section: [[save]] is used to persist one edited
    * section (a `/model` switch) by writing the whole file back, so a section
    * that renders to nothing is a section the next write silently deletes.
    *
    * The one thing that cannot survive a round trip is an empty section — a
    * `Some(McpSection(empty))` and a `None` produce the same (absent) text and
    * both read back as `None`.
    */
  def render(config: AppConfig): String =
    val sb = new StringBuilder
    config.model.foreach { m =>
      sb.append("[model]\n")
      m.provider.foreach(p => sb.append(s"provider = ${scalarValue(p)}\n"))
      m.id.foreach(id => sb.append(s"id = ${scalarValue(id)}\n"))
    }
    config.mcp.foreach { mcp =>
      mcp.servers.foreach { (name, server) =>
        sb.append(s"[mcp.servers.$name]\n")
        sb.append(s"command = ${scalarValue(server.command)}\n")
        server.args.foreach { args =>
          val items = args.map(listElement).mkString(" ")
          sb.append(if items.isEmpty then "args =\n" else s"args = $items\n")
        }
        server.env.foreach((k, v) => sb.append(s"env.$k = ${scalarValue(v)}\n"))
      }
    }
    sb.toString

  /** Render a scalar so that re-parsing it yields the same string. The parser
    * trims the RHS and strips one surrounding pair of quotes, so only an empty
    * value, one with edge whitespace, or one that already looks quoted needs
    * quoting — internal spaces, `#` and lone quotes survive verbatim.
    */
  private[config] def scalarValue(s: String): String =
    val looksQuoted = s.length >= 2 && s.startsWith("\"") && s.endsWith("\"")
    if s.isEmpty || s != s.trim || looksQuoted then quoted(s) else s

  /** Render one element of an inline list. A list element is decoded in two
    * stages — `ConfigType.splitInline` cuts the tokens, then the element's own
    * scalar parse runs on each — so the element has to survive both: quote it
    * as a scalar first, then quote THAT for the splitter whenever the splitter
    * would re-interpret it (whitespace, a quote, an escape) or drop it (empty).
    */
  private def listElement(s: String): String =
    val scalar = scalarValue(s)
    if scalar.isEmpty || scalar.exists(c => c == ' ' || c == '\t' || c == '"' || c == '\\')
    then quoted(scalar)
    else scalar

  /** Wrap in double quotes, escaping `\` and `"` as the parser's unescape expects. */
  private def quoted(s: String): String =
    val sb = new StringBuilder("\"")
    s.foreach { c =>
      if c == '"' || c == '\\' then sb.append('\\')
      sb.append(c)
    }
    sb.append('"').toString

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
