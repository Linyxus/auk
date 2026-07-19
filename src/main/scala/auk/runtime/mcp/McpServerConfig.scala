package auk.runtime.mcp

import scala.util.control.NonFatal

import auk.llm.tools.Json
import auk.platform.{Platform, PathOps}

/** One entry from `.auk/mcp.json`: a single stdio MCP server auk may launch.
  *
  * The shape mirrors the de-facto ecosystem format (Claude Desktop / Cursor /
  * the reference servers): a `command` plus optional `args` and `env`. `command`
  * is the executable (e.g. `npx`), `args` its argv tail, and `env` process
  * environment overrides layered on top of auk's own environment when the child
  * is spawned. Only `command` is mandatory; `args` and `env` default to empty so
  * the common single-binary server is a one-liner.
  */
final case class McpServerConfig(
    name: String,
    command: String,
    args: List[String],
    env: Map[String, String]
)

/** Loader for `.auk/mcp.json`, the separate (non-`.auk/config`) file that
  * declares the MCP servers auk connects to.
  *
  * The contract is deliberately forgiving at the edges and strict in the middle:
  * an ABSENT file is a normal state (a project simply uses no MCP servers) and
  * yields `Right(Nil)`, never an error; a PRESENT file that is malformed or
  * misshapen yields `Left(message)` so the app can surface one clear diagnostic
  * at startup while still running with no servers. Servers are returned in the
  * key order of the `mcpServers` object — [[Json.Obj]] preserves insertion order,
  * so the on-disk order is the order servers (and their tools) are exposed to
  * agents.
  */
object McpConfig:
  /** Location of the MCP config, relative to the working directory. */
  val RelativePath = ".auk/mcp.json"

  /** Load and parse `<dir>/.auk/mcp.json`. See [[McpConfig]] for the error
    * contract (absent ⇒ `Right(Nil)`; malformed/misshapen ⇒ `Left`). */
  def load(dir: String = Platform.cwd()): Either[String, List[McpServerConfig]] =
    val path = PathOps.join(dir, RelativePath)
    if !Platform.fs.exists(path) then Right(Nil)
    else
      val text =
        try Right(Platform.fs.readString(path))
        catch case NonFatal(e) => Left(s"could not read $path: ${e.getMessage}")
      text.flatMap(parse(_, path))

  /** Parse the JSON text of an mcp.json document. Separated from [[load]] so the
    * parsing rules are unit-testable without touching the filesystem. */
  def parse(text: String, source: String = RelativePath): Either[String, List[McpServerConfig]] =
    Json.parse(text) match
      case Left(err) => Left(s"$source is not valid JSON: $err")
      case Right(json) => decode(json, source)

  private def decode(json: Json, source: String): Either[String, List[McpServerConfig]] =
    json match
      case root: Json.Obj =>
        // A file that declares no `mcpServers` is as valid as an absent one: it
        // simply contributes no servers. Only a present-but-wrong-typed field is
        // an error.
        root.get("mcpServers") match
          case None => Right(Nil)
          case Some(servers: Json.Obj) => decodeServers(servers, source)
          case Some(other) =>
            Left(s"$source: `mcpServers` must be an object, found ${other.typeName}")
      case other =>
        Left(s"$source: top level must be an object, found ${other.typeName}")

  private def decodeServers(servers: Json.Obj, source: String): Either[String, List[McpServerConfig]] =
    // Fold left-to-right so the first offending server produces the message and
    // ordering follows the object's key order.
    val acc = List.newBuilder[McpServerConfig]
    var error: Option[String] = None
    val it = servers.fields.iterator
    while error.isEmpty && it.hasNext do
      val (name, spec) = it.next()
      decodeServer(name, spec, source) match
        case Right(cfg) => acc += cfg
        case Left(msg)  => error = Some(msg)
    error.toLeft(acc.result())

  private def decodeServer(name: String, spec: Json, source: String): Either[String, McpServerConfig] =
    spec match
      case obj: Json.Obj =>
        for
          command <- requireString(obj, "command", name, source)
          args <- optStringArray(obj, "args", name, source)
          env <- optStringMap(obj, "env", name, source)
        yield McpServerConfig(name, command, args, env)
      case other =>
        Left(s"$source: server '$name' must be an object, found ${other.typeName}")

  private def requireString(obj: Json.Obj, key: String, server: String, source: String): Either[String, String] =
    obj.get(key) match
      case Some(Json.Str(s)) => Right(s)
      case Some(other)       => Left(s"$source: server '$server' field `$key` must be a string, found ${other.typeName}")
      case None              => Left(s"$source: server '$server' is missing required field `$key`")

  private def optStringArray(obj: Json.Obj, key: String, server: String, source: String): Either[String, List[String]] =
    obj.get(key) match
      case None | Some(Json.Null) => Right(Nil)
      case Some(Json.Arr(elems)) =>
        val out = List.newBuilder[String]
        var error: Option[String] = None
        val it = elems.iterator
        while error.isEmpty && it.hasNext do
          it.next() match
            case Json.Str(s) => out += s
            case other       => error = Some(s"$source: server '$server' field `$key` must be an array of strings, found ${other.typeName}")
        error.toLeft(out.result())
      case Some(other) => Left(s"$source: server '$server' field `$key` must be an array of strings, found ${other.typeName}")

  private def optStringMap(obj: Json.Obj, key: String, server: String, source: String): Either[String, Map[String, String]] =
    obj.get(key) match
      case None | Some(Json.Null) => Right(Map.empty)
      case Some(env: Json.Obj) =>
        val out = Map.newBuilder[String, String]
        var error: Option[String] = None
        val it = env.fields.iterator
        while error.isEmpty && it.hasNext do
          it.next() match
            case (k, Json.Str(v)) => out += (k -> v)
            case (k, other)       => error = Some(s"$source: server '$server' env `$k` must be a string, found ${other.typeName}")
        error.toLeft(out.result())
      case Some(other) => Left(s"$source: server '$server' field `$key` must be an object, found ${other.typeName}")
