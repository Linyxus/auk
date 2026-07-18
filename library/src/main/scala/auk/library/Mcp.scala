package auk.library

import scala.scalajs.js

/** The worker side of MCP: a synchronous proxy that reaches the host's shared MCP
  * servers by `spawnSync`-ing the `--mcp-call` helper for ONE request→response
  * round-trip per operation — the same blocking model as `lib.shell`.
  *
  * `eval_scala` user code is fully synchronous, so unlike the team/workflow
  * clients (fire-and-forget over a persistent socket) MCP must return data within
  * one eval. Each op spawns the helper, hands it the request on stdin, and reads
  * the single reply line; the helper does the actual socket round-trip to the host
  * `McpBridge`. Construction is inert (reads no env, spawns nothing): the REPL
  * preamble builds one in every worker, including those with no MCP bridge. See
  * [[Mcp]] for the user-facing contract. */
private[library] final class McpImpl extends Mcp:
  def available: Boolean = McpImpl.sockPath.isDefined

  def servers: List[String] = McpImpl.serversOf(McpImpl.roundTrip("listServers"))

  def overview(): Unit =
    if !available then
      println("MCP is unavailable in this context (the host MCP bridge is not connected).")
    else
      servers match
        case Nil   => println("No MCP servers are configured (see .auk/mcp.json).")
        case names => println(names.mkString("\n"))

  def server(name: String): McpServer = new McpServerImpl(name)

/** A handle to one server; every accessor is a fresh round-trip to the host.
  * Renders as `McpServer(<name>)` so evaluating one in the REPL identifies it. */
private[library] final class McpServerImpl(val name: String) extends McpServer:
  def tools: List[McpTool] =
    McpImpl.toolsOf(McpImpl.roundTrip("listTools", server = Some(name)))
  def resources: List[McpResource] =
    McpImpl.resourcesOf(McpImpl.roundTrip("listResources", server = Some(name)))
  def callTool(tool: String): McpCallResult =
    McpImpl.callResultOf(McpImpl.roundTrip("callTool", server = Some(name), tool = Some(tool)))
  def callTool(tool: String, argsJson: String): McpCallResult =
    McpImpl.callResultOf(McpImpl.roundTrip("callTool", server = Some(name), tool = Some(tool), argsJson = Some(argsJson)))
  def readResource(uri: String): McpReadResult =
    McpImpl.readResultOf(McpImpl.roundTrip("readResource", server = Some(name), uri = Some(uri)))
  override def toString: String = s"McpServer($name)"

// The `*Impl` case classes carry the wire data and render readably (so the REPL's
// value display is the discovery path): a tool as `name — description`, a resource
// as `uri — name`, a content block as its text (or `[kind]` when it has none).
private[library] final case class McpToolImpl(name: String, description: String, inputSchema: String) extends McpTool:
  override def toString: String = if description.isEmpty then name else s"$name — $description"

private[library] final case class McpResourceImpl(uri: String, name: String, description: String, mimeType: String) extends McpResource:
  override def toString: String = if name.isEmpty then uri else s"$uri — $name"

private[library] final case class McpContentImpl(kind: String, text: String, mimeType: String, uri: String) extends McpContent:
  override def toString: String = if text.nonEmpty then text else s"[$kind]"

private[library] object McpImpl:
  // Isolation tax: `library/` cannot depend on `auk.runtime`, so the env-var NAMES
  // are duplicated here as string literals.
  private val SockEnv = "AUK_MCP_SOCK" // mirror auk.runtime.McpBridge.SockEnv
  private val HelperEnv = "AUK_MCP_HELPER_JS" // mirror auk.platform.js.McpHelper.HelperEnv

  /** Worker-side spawnSync deadline, just ABOVE the host's 120s request timeout so
    * the host resolves a clean "timed out" reply rather than the worker killing the
    * helper mid-flight. */
  private val CallTimeoutMs = 130_000
  /** Cap on captured output (~10 MB), mirroring the shell runner. */
  private val MaxBuffer = 10 * 1024 * 1024

  private def env: js.Dynamic = js.Dynamic.global.process.env

  private def optEnv(name: String): Option[String] =
    val v = env.selectDynamic(name)
    if v == null || js.isUndefined(v) then None else Some(v.asInstanceOf[String])

  private def sockPath: Option[String] = optEnv(SockEnv)

  // -- pure request-building / response-decoding (the unit-tested core) -------

  /** Build the request JSON line for `op`. `argsJson`, if given, must be a JSON
    * object (the tool's arguments); it is parsed and embedded as `args`. The `id`
    * is a constant — one spawnSync is one response, so it is never correlated on.
    * Throws a clear error if `argsJson` is not a JSON object. */
  def request(
      op: String,
      server: Option[String] = None,
      tool: Option[String] = None,
      uri: Option[String] = None,
      argsJson: Option[String] = None
  ): String =
    val o = js.Dynamic.literal()
    o.updateDynamic("id")(1)
    o.updateDynamic("op")(op)
    server.foreach(s => o.updateDynamic("server")(s))
    tool.foreach(t => o.updateDynamic("tool")(t))
    uri.foreach(u => o.updateDynamic("uri")(u))
    argsJson.foreach(a => o.updateDynamic("args")(parseArgs(a)))
    js.JSON.stringify(o)

  private def parseArgs(argsJson: String): js.Any =
    val parsed =
      try js.JSON.parse(argsJson)
      catch case _: Throwable => throw new RuntimeException(s"arguments must be a JSON object, but this is not valid JSON: $argsJson")
    if js.typeOf(parsed) != "object" || parsed == null || js.Array.isArray(parsed) then
      throw new RuntimeException("arguments must be a JSON object")
    parsed

  /** Parse the helper's single response line. Returns the `data` value on
    * `ok:true`; throws `RuntimeException(error)` on `ok:false` (a protocol/transport
    * failure); throws on a malformed/unexpected reply. The reply's `id` is ignored —
    * one spawnSync is one response, and the helper's own-failure line omits it. */
  def parseResponse(line: String): js.Dynamic =
    val d =
      try js.JSON.parse(line)
      catch case _: Throwable => throw new RuntimeException(s"the MCP call returned a non-JSON response: ${truncate(line)}")
    val ok = d.ok
    if ok == null || js.isUndefined(ok) then
      throw new RuntimeException(s"the MCP call returned an unexpected response: ${truncate(line)}")
    else if ok.asInstanceOf[Boolean] then d.data.asInstanceOf[js.Dynamic]
    else
      val err = d.error
      throw new RuntimeException(if err == null || js.isUndefined(err) then "unknown MCP error" else err.asInstanceOf[String])

  def serversOf(data: js.Dynamic): List[String] = strArray(data, "servers")

  def toolsOf(data: js.Dynamic): List[McpTool] =
    objArray(data, "tools").map(t => McpToolImpl(str(t, "name"), str(t, "description"), schemaString(t, "inputSchema")))

  def resourcesOf(data: js.Dynamic): List[McpResource] =
    objArray(data, "resources").map(r => McpResourceImpl(str(r, "uri"), str(r, "name"), str(r, "description"), str(r, "mimeType")))

  def callResultOf(data: js.Dynamic): McpCallResult =
    McpCallResult(contentOf(data, "content"), bool(data, "isError"))

  def readResultOf(data: js.Dynamic): McpReadResult =
    McpReadResult(contentOf(data, "contents"))

  private def contentOf(data: js.Dynamic, key: String): List[McpContent] =
    objArray(data, key).map(c => McpContentImpl(str(c, "kind"), str(c, "text"), str(c, "mimeType"), str(c, "uri")))

  // -- js.Dynamic accessors (null/undefined → empty, matching AukImpl style) --

  private def str(o: js.Dynamic, k: String): String =
    val v = o.selectDynamic(k)
    if v == null || js.isUndefined(v) then "" else v.asInstanceOf[String]

  private def bool(o: js.Dynamic, k: String): Boolean =
    val v = o.selectDynamic(k)
    v != null && !js.isUndefined(v) && v.asInstanceOf[Boolean]

  private def strArray(o: js.Dynamic, k: String): List[String] =
    val v = o.selectDynamic(k)
    if js.Array.isArray(v) then v.asInstanceOf[js.Array[js.Any]].toList.map(x => if x == null then "" else x.asInstanceOf[String])
    else Nil

  private def objArray(o: js.Dynamic, k: String): List[js.Dynamic] =
    val v = o.selectDynamic(k)
    if js.Array.isArray(v) then v.asInstanceOf[js.Array[js.Dynamic]].toList else Nil

  /** Re-render a parsed JSON-Schema value back to a compact JSON string for the
    * model to read. `"{}"` when the schema is absent. */
  private def schemaString(o: js.Dynamic, k: String): String =
    val v = o.selectDynamic(k)
    if v == null || js.isUndefined(v) then "{}" else js.JSON.stringify(v)

  private def truncate(s: String): String = if s.length <= 200 then s else s.take(200) + "..."

  // -- spawnSync glue (thin; covered by the Phase 5 e2e, not the unit suite) --

  private def childProcess: js.Dynamic = js.Dynamic.global.require("node:child_process")

  /** The argv to run the helper: dev runs the generated script named by
    * `AUK_MCP_HELPER_JS`; a SEA has none, so it dispatches its own `--mcp-call`
    * entry branch. Mirrors ReplArtifacts' script-vs-flag split. */
  private def spawnArgv(): List[String] =
    val execPath = js.Dynamic.global.process.execPath.asInstanceOf[String]
    optEnv(HelperEnv) match
      case Some(helper) => List(execPath, helper)
      case None         => List(execPath, "--mcp-call")

  /** Do one op end-to-end: fail fast if MCP is unavailable, else spawnSync the
    * helper with the request on stdin and decode the single reply line. */
  def roundTrip(
      op: String,
      server: Option[String] = None,
      tool: Option[String] = None,
      uri: Option[String] = None,
      argsJson: Option[String] = None
  ): js.Dynamic =
    if sockPath.isEmpty then
      throw new RuntimeException(
        "MCP is unavailable in this context: AUK_MCP_SOCK is not set (the host MCP bridge is not connected)")
    val requestJson = request(op, server, tool, uri, argsJson)
    val argv = spawnArgv()
    val opts = js.Dynamic.literal(
      input = requestJson + "\n",
      encoding = "utf8",
      timeout = CallTimeoutMs,
      maxBuffer = MaxBuffer
    )
    val res = childProcess.spawnSync(argv.head, js.Array(argv.tail*), opts)
    val stdout = strOr(res.stdout)
    if stdout.trim.isEmpty then throw new RuntimeException(s"the MCP call failed: ${failureDetail(res)}")
    parseResponse(firstLine(stdout))

  private def strOr(v: js.Dynamic): String =
    if v == null || js.isUndefined(v) then "" else v.asInstanceOf[String]

  private def failureDetail(res: js.Dynamic): String =
    val err = res.error
    val errCode = if err == null || js.isUndefined(err) then "" else strOr(err.code)
    if errCode == "ETIMEDOUT" then "it timed out"
    else if errCode.nonEmpty then s"the helper could not run ($errCode)"
    else
      val stderr = strOr(res.stderr).trim
      if stderr.nonEmpty then s"the helper produced no output; stderr: ${truncate(stderr)}"
      else "the helper produced no output"

  private def firstLine(s: String): String =
    val i = s.indexOf('\n')
    if i >= 0 then s.substring(0, i) else s
