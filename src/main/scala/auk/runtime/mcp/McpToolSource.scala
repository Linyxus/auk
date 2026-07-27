package auk.runtime.mcp

import scala.collection.mutable
import scala.util.control.NonFatal

import gears.async.{Async, Future}

import auk.agent.{McpServerState, McpServerView, McpToolView}
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, Json, Schema, desc}

/** Exposes a hub's MCP tools and resources as native model [[Tool]]s.
  *
  * The model calls an MCP tool exactly as it calls any built-in one: the tool is
  * advertised in the tool list, and its invocation is dispatched host-side
  * through the [[McpHub]] — there is no REPL/worker involvement. This module
  * holds the three pieces that make that work:
  *
  *   - [[McpToolAdapter]] — one model-facing tool per discovered MCP tool, a raw
  *     JSON passthrough that advertises the server's schema verbatim and forwards
  *     the model's arguments to `hub.callTool` unchanged.
  *   - the two resource meta-tools ([[ListMcpResources]] / [[ReadMcpResource]]),
  *     present whenever any server is configured.
  *   - [[McpToolSource]] itself — discovers each server's tools in the background
  *     and holds the growing snapshot the main agent's registry reads each round.
  */
final class McpToolSource(
    hub: McpHub,
    configs: List[McpServerConfig],
    // Called with a fresh [[snapshot]] each time a server's discovery settles,
    // so a UI can track per-server status live. Host-side hook (the TUI's
    // `/mcp` panel rides it via AgentEvent.McpUpdated); defaults to a no-op.
    onUpdate: Vector[McpServerView] => Unit = _ => ()
):
  // Single JS event loop → the snapshot and the used-name set need no locking:
  // discovery's per-server futures only interleave at await points, and each
  // append (below) runs to completion synchronously between them.
  private var discovered: List[Tool] =
    // The resource meta-tools exist as soon as any server is configured, so they
    // are advertised from round one — before any tools/list has resolved.
    if configs.nonEmpty then List(ListMcpResources(hub, configs), ReadMcpResource(hub)) else Nil

  private val usedNames: mutable.Set[String] = mutable.Set.empty[String] ++ discovered.map(_.name)

  // Per-server discovery outcome: absent while still in flight, then the tools
  // the server contributed (Right) or the failure that ended it (Left).
  private var outcomes: Map[String, Either[String, Vector[McpToolView]]] = Map.empty

  /** The current snapshot of MCP-derived tools. Grows as servers respond; the
    * caller (the main registry's dynamic supplier) re-reads it every round. */
  def tools: List[Tool] = discovered

  /** Every configured server's current status, in config order: its discovery
    * state (pending / ready / failed, with the error), the tools it
    * contributed, and — once its handshake has run — the version facts its
    * client recorded. Pure read; safe to call at any point of discovery. */
  def snapshot: Vector[McpServerView] =
    configs.toVector.map: cfg =>
      val client = hub.client(cfg.name)
      val (state, error, toolViews) = outcomes.get(cfg.name) match
        case Some(Right(ts)) => (McpServerState.Ready, None, ts)
        case Some(Left(err)) => (McpServerState.Failed, Some(err), Vector.empty)
        case None            => (McpServerState.Pending, None, Vector.empty)
      McpServerView(
        name = cfg.name,
        command = (cfg.command :: cfg.args).mkString(" "),
        env = cfg.env.keys.toVector.sorted,
        state = state,
        error = error,
        version = client.flatMap(_.serverInfo).flatMap(JsonView.str(_, "version")),
        protocolVersion = client.flatMap(_.negotiatedProtocolVersion),
        tools = toolViews
      )

  /** Discover every configured server's tools, in parallel, appending each
    * server's adapters to the snapshot as its `tools/list` lands (per-server
    * incremental, config order within a server). A server that fails to list
    * contributes nothing — it is not fatal. Runs once at startup; there is no
    * re-listing and no `tools/list_changed` handling (v1 snapshot semantics).
    *
    * Takes only `Async` (not `Async.Spawn`) so a caller can fire it inside a
    * plain background `Future`; it opens its own spawn scope via `Async.group`
    * and blocks there until every server has answered, appending along the way.
    */
  def discover()(using Async): Unit =
    if configs.nonEmpty then
      Async.group:
        val futures = configs.map: cfg =>
          Future:
            // A server must never take its siblings down with it: `Async.group`
            // cancels every still-running child if an exception escapes this block
            // (which `_.await` below would rethrow). `listTools` is Left-total by
            // design, but swallow an unexpected throw too, so one server's failure
            // can only cost its own tools — never the others' in-flight discovery.
            try
              hub.listTools(cfg.name) match
                case Right(infos) => appendServer(cfg.name, infos)
                // A failing server contributes no tools — but its failure is
                // kept, so the `/mcp` panel can say why it has none.
                case Left(err) => recordFailure(cfg.name, err)
            catch case NonFatal(e) => recordFailure(cfg.name, s"MCP discovery for '${cfg.name}' failed: $e")
        // Await keeps the futures alive until they land; without it the group
        // would cancel them the moment this block's last expression completes.
        futures.foreach(_.await)

  private def appendServer(server: String, infos: List[McpToolInfo]): Unit =
    val added = infos.map: info =>
      val wire = McpNaming.dedupe(server, info.name, usedNames)
      usedNames += wire
      (McpToolAdapter(hub, server, info, wire), McpToolView(info.name, wire, info.description))
    discovered = discovered ++ added.map(_._1)
    outcomes = outcomes.updated(server, Right(added.map(_._2).toVector))
    onUpdate(snapshot)

  private def recordFailure(server: String, error: String): Unit =
    outcomes = outcomes.updated(server, Left(error))
    onUpdate(snapshot)

/** One model-facing [[Tool]] backed by an MCP tool on `server`. Arguments are a
  * raw JSON passthrough (no case-class decoding), the advertised schema is the
  * server's own [[McpToolInfo.inputSchema]] verbatim, and invocation is a direct
  * `hub.callTool`. */
final class McpToolAdapter(
    hub: McpHub,
    server: String,
    info: McpToolInfo,
    val wireName: String
) extends Tool:
  type Params = Json

  def name: String = wireName

  def description: String =
    if info.description.nonEmpty then s"[MCP: $server] ${info.description}"
    else s"Tool '${info.name}' from MCP server '$server'."

  def input: ToolInput[Json] = McpToolAdapter.PassthroughInput

  /** The server's JSON Schema, advertised verbatim — the flat conversion would
    * corrupt anything nested. */
  override def rawParametersSchema: Option[Json] = Some(info.inputSchema)

  def execute(params: Json)(using RuntimeContext, Async): ToolResult =
    hub.callTool(server, info.name, params) match
      case Left(err) => ToolResult.error(s"MCP call failed: $err")
      case Right(McpCallResult(content, isError)) =>
        ToolResult(McpToolSource.renderContent(content), isError = isError)

object McpToolAdapter:
  /** A [[ToolInput]] that accepts the model's arguments JSON as-is: an MCP tool
    * forwards the raw object straight to its server, so there is nothing to
    * decode and no shape to reject. The schema here is only a harmless fallback —
    * [[McpToolAdapter.rawParametersSchema]] is what actually reaches the model. */
  val PassthroughInput: ToolInput[Json] =
    ToolInput.instance(Schema(`type` = Some("object")))(json => Right(json))

/** Wire-name construction for MCP tools: `mcp__<server>__<tool>`, sanitized to
  * the tool-name charset, capped at [[MaxLength]], and disambiguated on
  * collision. The dispatcher never reverses this — an adapter carries its own
  * `wireName` and dispatches on the original `info.name`. */
object McpNaming:
  /** Anthropic caps tool names at 64 characters. */
  val MaxLength: Int = 64

  private val Prefix = "mcp__"

  private def isAllowed(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-'

  /** Replace every character outside `[A-Za-z0-9_-]` with `_`. */
  def sanitize(s: String): String = s.map(c => if isAllowed(c) then c else '_')

  /** The wire name for one `(server, tool)`, sanitized and capped at
    * [[MaxLength]] by truncating ONLY the tool part (the prefix and server are
    * never trimmed). `suffix` — a collision disambiguator ("" or "_2"…) — is kept
    * within the cap too. If the prefix alone already exceeds the cap (a
    * pathologically long server name) the result is prefix+suffix, over the cap
    * by design, since the prefix is never truncated. */
  def name(server: String, tool: String, suffix: String = ""): String =
    val prefix = Prefix + sanitize(server) + "__"
    val toolPart = sanitize(tool)
    val room = math.max(0, MaxLength - prefix.length - suffix.length)
    prefix + toolPart.take(room) + suffix

  /** A collision-free name for `(server, tool)` not already in `used`: try the
    * bare name, then `_2`, `_3`, … (deterministic). */
  def dedupe(server: String, tool: String, used: collection.Set[String]): String =
    val first = name(server, tool)
    if !used.contains(first) then first
    else LazyList.from(2).map(n => name(server, tool, s"_$n")).find(!used.contains(_)).get

  /** Assign wire names to `pairs` in order, disambiguating collisions against the
    * names assigned earlier. Pure mirror of the incremental path
    * [[McpToolSource]] takes; used directly by tests. */
  def assign(pairs: List[(String, String)]): List[String] =
    val used = mutable.Set.empty[String]
    pairs.map { (server, tool) =>
      val n = dedupe(server, tool, used)
      used += n
      n
    }

/** Lists resources exposed by one MCP server, or all of them, grouped by server.
  * A server that fails to list contributes an error line rather than failing the
  * whole call. */
final class ListMcpResources(hub: McpHub, configs: List[McpServerConfig]) extends Tool:
  type Params = ListMcpResources.Params

  val name = "list_mcp_resources"

  val description =
    "List the resources exposed by the configured MCP servers. With no argument, " +
      "lists every server's resources grouped by server; pass `server` to list " +
      "just that one. Use `read_mcp_resource` to read a resource's contents by uri."

  val input: ToolInput[ListMcpResources.Params] = ToolInput[ListMcpResources.Params]

  def execute(params: Params)(using RuntimeContext, Async): ToolResult =
    val servers = params.server match
      case Some(s) => List(s)
      case None    => configs.map(_.name)
    val sections = servers.map: server =>
      val body = hub.listResources(server) match
        case Left(err)  => s"  (error: $err)"
        case Right(Nil) => "  (no resources)"
        case Right(res) =>
          res.map(r => s"  ${r.uri} — ${r.name}${r.mimeType.fold("")(m => s" ($m)")}").mkString("\n")
      s"$server:\n$body"
    ToolResult.ok(sections.mkString("\n\n"))

object ListMcpResources:
  case class Params(
      @desc("Optional MCP server name; omit to list resources from every configured server.")
      server: Option[String] = None
  ) derives ToolInput

/** Reads a single MCP resource by uri from a named server, rendering its
  * contents with the same rules as a tool result. */
final class ReadMcpResource(hub: McpHub) extends Tool:
  type Params = ReadMcpResource.Params

  val name = "read_mcp_resource"

  val description =
    "Read the contents of a resource exposed by an MCP server, by its uri. Use " +
      "`list_mcp_resources` first to discover the available server names and uris."

  val input: ToolInput[ReadMcpResource.Params] = ToolInput[ReadMcpResource.Params]

  def execute(params: Params)(using RuntimeContext, Async): ToolResult =
    hub.readResource(params.server, params.uri) match
      case Left(err)                    => ToolResult.error(s"MCP resource read failed: $err")
      case Right(McpReadResult(content)) => ToolResult.ok(McpToolSource.renderContent(content))

object ReadMcpResource:
  case class Params(
      @desc("The MCP server that exposes the resource.")
      server: String,
      @desc("The resource uri, as reported by `list_mcp_resources`.")
      uri: String
  ) derives ToolInput

object McpToolSource:
  /** Render MCP content blocks into the plain text a tool result carries: text
    * blocks joined with a blank line, verbatim; any non-text block replaced by a
    * one-line placeholder naming its kind (and mime type, if known); empty
    * content rendered as `(no content)`. Binary payloads (images/audio/blobs) are
    * out of scope — the tool-result wire is a plain String today. */
  def renderContent(blocks: List[McpContentBlock]): String =
    if blocks.isEmpty then "(no content)"
    else blocks.map(renderBlock).mkString("\n\n")

  private def renderBlock(b: McpContentBlock): String =
    b.text match
      case Some(text) if b.kind == "text" => text
      case _                              => s"[${b.kind}${b.mimeType.fold("")(m => s" $m")}]"
