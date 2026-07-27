package auk.runtime.mcp

import gears.async.Async

import auk.llm.tools.Json

/** The host's registry of MCP clients, one per configured server, shared by every
  * agent (main, workflow nodes, team members) exactly as [[auk.runtime.TeamBridge]]
  * is the single host owner of team state.
  *
  * The hub is built eagerly from config but connects lazily: a [[McpClient]] is
  * created for each server up front (so `serverNames` is known immediately), yet
  * no child process is spawned until an operation first touches that server. Each
  * operation resolves the named server — an unknown name is a `Left`, never an
  * exception — and delegates to its client, which connects on demand. Everything
  * runs on the single JS event loop, so the map needs no locking. */
final class McpHub(
    configs: List[McpServerConfig],
    requestTimeoutMs: Double = McpClient.DefaultRequestTimeoutMs,
    // The seam that keeps the hub unit-testable: production spawns real stdio
    // children (`LineProcessTransport.factory`), while a test injects a factory
    // returning a scripted in-memory transport. Defaulted, so production callers
    // never see it.
    transportFactory: McpServerConfig => (String => Unit, Int => Unit) => McpTransport = LineProcessTransport.factory
):
  // Insertion-ordered so `serverNames` follows the config's order. A duplicate
  // name (pathological, but the JSON AST permits it) collapses to the last spec,
  // matching the config loader's key-order semantics.
  private val clients: Map[String, McpClient] =
    configs.map(c => c.name -> McpClient(c.name, transportFactory(c), requestTimeoutMs)).toMap

  private val order: List[String] = configs.map(_.name).distinct

  /** The configured server names, in config order. */
  def serverNames: List[String] = order

  /** The client for a configured server, if any — exposed for status
    * inspection (handshake facts for the TUI's `/mcp` panel). Operations still
    * go through the typed methods below. */
  def client(server: String): Option[McpClient] = clients.get(server)

  /** List a server's tools. */
  def listTools(server: String)(using Async): Either[String, List[McpToolInfo]] =
    clientFor(server).flatMap(_.listTools())

  /** Invoke a tool on a server. */
  def callTool(server: String, tool: String, arguments: Json)(using Async): Either[String, McpCallResult] =
    clientFor(server).flatMap(_.callTool(tool, arguments))

  /** List a server's resources. */
  def listResources(server: String)(using Async): Either[String, List[McpResourceInfo]] =
    clientFor(server).flatMap(_.listResources())

  /** Read a resource from a server by URI. */
  def readResource(server: String, uri: String)(using Async): Either[String, McpReadResult] =
    clientFor(server).flatMap(_.readResource(uri))

  /** Close every client (kills any spawned transports). */
  def close(): Unit = clients.values.foreach(_.close())

  private def clientFor(server: String): Either[String, McpClient] =
    clients.get(server).toRight(s"unknown MCP server '$server'")
