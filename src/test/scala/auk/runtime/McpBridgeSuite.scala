package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.scalajs.js.timers
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.llm.tools.Json
import auk.runtime.mcp.{McpHub, McpServerConfig, McpTransport, Reply, ScriptedTransport}

/** Host-level tests for [[McpBridge]]: a raw Unix-domain-socket client drives the
  * wire protocol directly (`{id, op, …}` → `{id, ok, data|error}`), and the hub
  * behind the bridge is backed by a scripted in-memory transport (no child
  * process), so a full op round-trips through the real bridge + real
  * [[McpClient]] handshake without spawning anything. */
class McpBridgeSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  // -- fixtures ----------------------------------------------------------------

  private val initResult: Json = Json.Obj(
    List(
      "protocolVersion" -> Json.Str("2025-06-18"),
      "capabilities" -> Json.Obj(Nil),
      "serverInfo" -> Json.Obj(List("name" -> Json.Str("stub")))
    )
  )

  /** A scripted server that handshakes and echoes a `pong` text block from any
    * `tools/call`. */
  private val script: Map[String, Reply] = Map(
    "initialize" -> Reply.Result(initResult),
    "tools/call" -> Reply.Result(
      Json.Obj(
        List(
          "content" -> Json.Arr(List(Json.Obj(List("type" -> Json.Str("text"), "text" -> Json.Str("pong"))))),
          "isError" -> Json.Bool(false)
        )
      )
    )
  )

  /** A transport that completes the handshake (echoes an initialize result, accepts
    * the initialized notification) but throws on any other write — so a post-connect
    * op's write raises inside `McpBridge.respond`, not `connect`. */
  private def throwOnOpFactory: (String => Unit, Int => Unit) => McpTransport =
    (onLine, _) =>
      new McpTransport:
        def writeLine(line: String): Boolean =
          val j = Json.parse(line).getOrElse(Json.Null)
          str(j, "method") match
            case Some("initialize") =>
              val id = field(j, "id").collect { case Json.Num(n) => n.toLong }.getOrElse(0L)
              timers.setTimeout(0)(onLine(ScriptedTransport.responseLine(id, "result", initResult).render))
              true
            case Some("notifications/initialized") => true
            case _                                 => throw new RuntimeException("write blew up mid-op")
        def kill(): Unit = ()
        def alive: Boolean = true

  /** A hub with one server ("stub") whose client talks to the scripted transport. */
  private def makeHub(): McpHub =
    McpHub(
      List(McpServerConfig("stub", "unused", Nil, Map.empty)),
      transportFactory = _ => ((onLine, onExit) => new ScriptedTransport(script, _ => (), onLine, onExit))
    )

  // -- raw wire client ---------------------------------------------------------

  /** A raw UDS client: renders a [[Json]] request line and buffers received lines. */
  private class WireClient(sockPath: String):
    private val net = js.Dynamic.global.require("node:net")
    val incoming = UnboundedChannel[String]()
    private var buf = ""
    private val sock = net.createConnection(sockPath).asInstanceOf[js.Dynamic]
    sock.setEncoding("utf8")
    sock.on(
      "data",
      ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then incoming.sendImmediately(line)
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit]
    )
    def send(req: Json): Unit = { sock.write(req.render + "\n"); () }
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  // -- helpers -----------------------------------------------------------------

  private var counter = 0
  private def tmpSock(name: String): String =
    counter += 1
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-mcp-$name-${js.Dynamic.global.process.pid}-$counter.sock").asInstanceOf[String]

  private def start(bridge: McpBridge)(using Async.Spawn): Unit =
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await

  private def readLine(ch: UnboundedChannel[String])(using Async): String =
    ch.read() match
      case Right(l) => l
      case Left(_)  => fail("channel closed")

  private def req(fields: (String, Json)*): Json = Json.Obj(fields.toList)

  // -- tests -------------------------------------------------------------------

  test("callTool round-trips: request → hub → decoded data reply with ok:true"):
    Async.fromSync:
      val bridge = McpBridge(tmpSock("call"), makeHub())
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.send(
          req(
            "id" -> Json.num(1),
            "op" -> Json.Str("callTool"),
            "server" -> Json.Str("stub"),
            "tool" -> Json.Str("echo"),
            "args" -> Json.Obj(List("x" -> Json.num(1)))
          )
        )
        val line = readLine(client.incoming)
        val resp = Json.parse(line).getOrElse(fail(s"unparseable reply: $line"))
        assertEquals(field(resp, "id"), Some(Json.num(1)))
        assertEquals(bool(resp, "ok"), Some(true))
        // data is McpCallResult.toJson: our-side field names (kind/text), isError.
        val data = obj(resp, "data").getOrElse(fail(s"no data in $line"))
        assertEquals(bool(data, "isError"), Some(false))
        assert(line.contains("\"kind\":\"text\""), line)
        assert(line.contains("\"text\":\"pong\""), line)
      finally
        client.close()
        Async.fromSync(bridge.close())

  test("listServers returns the configured server names in order"):
    Async.fromSync:
      val bridge = McpBridge(tmpSock("servers"), makeHub())
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.send(req("id" -> Json.num(7), "op" -> Json.Str("listServers")))
        val line = readLine(client.incoming)
        assert(line.contains("\"ok\":true"), line)
        assert(line.contains("\"servers\":[\"stub\"]"), line)
      finally
        client.close()
        Async.fromSync(bridge.close())

  test("an op against an unknown server replies ok:false with a clear error"):
    Async.fromSync:
      val bridge = McpBridge(tmpSock("unknown"), makeHub())
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.send(req("id" -> Json.num(2), "op" -> Json.Str("listTools"), "server" -> Json.Str("ghost")))
        val line = readLine(client.incoming)
        val resp = Json.parse(line).getOrElse(fail(s"unparseable reply: $line"))
        assertEquals(field(resp, "id"), Some(Json.num(2)))
        assertEquals(bool(resp, "ok"), Some(false))
        assert(str(resp, "error").exists(_.contains("unknown MCP server 'ghost'")), line)
      finally
        client.close()
        Async.fromSync(bridge.close())

  test("an unknown op replies ok:false"):
    Async.fromSync:
      val bridge = McpBridge(tmpSock("badop"), makeHub())
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.send(req("id" -> Json.num(3), "op" -> Json.Str("frobnicate")))
        val line = readLine(client.incoming)
        assert(line.contains("\"ok\":false"), line)
        assert(line.contains("unknown MCP op 'frobnicate'"), line)
      finally
        client.close()
        Async.fromSync(bridge.close())

  test("an op that throws an unexpected error still replies ok:false, never hangs"):
    Async.fromSync:
      // A transport that handshakes normally but THROWS when the op's line is
      // written — so the throwable surfaces inside respond (past connect, which
      // has its own guard), exercising respond's catch-all. Without it the request
      // fiber would die writing nothing and the client would block until its
      // spawnSync timeout; the read below would then hang until munitTimeout.
      val hub = McpHub(
        List(McpServerConfig("boom", "unused", Nil, Map.empty)),
        transportFactory = _ => throwOnOpFactory
      )
      val bridge = McpBridge(tmpSock("throw"), hub)
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.send(req("id" -> Json.num(9), "op" -> Json.Str("listTools"), "server" -> Json.Str("boom")))
        val line = readLine(client.incoming)
        val resp = Json.parse(line).getOrElse(fail(s"unparseable reply: $line"))
        assertEquals(field(resp, "id"), Some(Json.num(9)))
        assertEquals(bool(resp, "ok"), Some(false))
        assert(str(resp, "error").exists(_.contains("internal error")), line)
      finally
        client.close()
        Async.fromSync(bridge.close())

  // -- reply field accessors ---------------------------------------------------

  private def field(j: Json, k: String): Option[Json] = j match
    case o: Json.Obj => o.get(k)
    case _           => None
  private def str(j: Json, k: String): Option[String] =
    field(j, k).collect { case Json.Str(s) => s }
  private def bool(j: Json, k: String): Option[Boolean] =
    field(j, k).collect { case Json.Bool(b) => b }
  private def obj(j: Json, k: String): Option[Json] =
    field(j, k).collect { case o: Json.Obj => o }
