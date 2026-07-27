package auk.runtime.mcp

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js.timers

import scala.collection.mutable

import gears.async.Async
import gears.async.default.given

import auk.agent.{McpServerState, McpServerView}
import auk.llm.tools.{Json, RuntimeContext}

/** [[McpToolSource]] and the resource meta-tools, driven by a scripted in-memory
  * transport through a real [[McpHub]] — no child process. Covers `renderContent`
  * rendering rules, discovery producing wire-named adapters that advertise the
  * server's schema verbatim, the callTool round-trip (including the tool-level
  * `isError` flag and a protocol Left → tool error), and `read_mcp_resource`. */
class McpToolSourceSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  private given RuntimeContext = RuntimeContext.cwd()

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  private val initResult: Json = Json.Obj(
    List(
      "protocolVersion" -> Json.Str("2025-06-18"),
      "capabilities" -> Json.Obj(List("tools" -> Json.Obj(Nil), "resources" -> Json.Obj(Nil))),
      "serverInfo" -> Json.Obj(List("name" -> Json.Str("stub")))
    )
  )

  private val echoSchema: Json = Json.Obj(
    List(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(List("text" -> Json.Obj(List("type" -> Json.Str("string"))))),
      "required" -> Json.Arr(List(Json.Str("text")))
    )
  )

  private val toolsList: Json = Json.Obj(
    List(
      "tools" -> Json.Arr(
        List(
          Json.Obj(List("name" -> Json.Str("echo"), "description" -> Json.Str("echoes text"), "inputSchema" -> echoSchema)),
          Json.Obj(List("name" -> Json.Str("add"), "description" -> Json.Str("adds"), "inputSchema" -> Json.Obj(List("type" -> Json.Str("object")))))
        )
      )
    )
  )

  private val config = McpServerConfig("srv", "cmd", Nil, Map.empty)

  /** A hub wired to a scripted transport answering `script` for every server. */
  private def hubWith(script: Map[String, Reply]): McpHub =
    val factory: McpServerConfig => (String => Unit, Int => Unit) => McpTransport =
      _ => (onLine, onExit) => new ScriptedTransport(script, _ => (), onLine, onExit)
    McpHub(List(config), transportFactory = factory)

  // -- renderContent (pure) ---------------------------------------------------

  private def text(s: String): McpContentBlock = McpContentBlock("text", Some(s), None, None)

  test("renderContent joins text blocks with a blank line"):
    assertEquals(McpToolSource.renderContent(List(text("hello"), text("world"))), "hello\n\nworld")

  test("renderContent renders empty content as (no content)"):
    assertEquals(McpToolSource.renderContent(Nil), "(no content)")

  test("renderContent placeholders a non-text block, with its mime type when known"):
    val img = McpContentBlock("image", None, Some("image/png"), None)
    assertEquals(McpToolSource.renderContent(List(img)), "[image image/png]")
    val blob = McpContentBlock("blob", None, None, None)
    assertEquals(McpToolSource.renderContent(List(blob)), "[blob]")

  test("renderContent mixes text and placeholders in order"):
    val img = McpContentBlock("image", None, Some("image/png"), None)
    assertEquals(McpToolSource.renderContent(List(text("caption"), img)), "caption\n\n[image image/png]")

  // -- discovery + adapters ---------------------------------------------------

  asyncTest("discover produces one wire-named adapter per tool plus the resource meta-tools"):
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList))), List(config))
    src.discover()
    val names = src.tools.map(_.name)
    assert(names.contains("mcp__srv__echo"), names.toString)
    assert(names.contains("mcp__srv__add"), names.toString)
    assert(names.contains("list_mcp_resources"), names.toString)
    assert(names.contains("read_mcp_resource"), names.toString)

  asyncTest("an adapter advertises the server's inputSchema verbatim and prefixes its description"):
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList))), List(config))
    src.discover()
    val echo = src.tools.find(_.name == "mcp__srv__echo").get
    assertEquals(echo.rawParametersSchema.map(_.render), Some(echoSchema.render))
    assertEquals(echo.description, "[MCP: srv] echoes text")

  asyncTest("calling an adapter round-trips the result content"):
    val callResult = Json.Obj(
      List("content" -> Json.Arr(List(Json.Obj(List("type" -> Json.Str("text"), "text" -> Json.Str("hi back"))))), "isError" -> Json.Bool(false))
    )
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList), "tools/call" -> Reply.Result(callResult))), List(config))
    src.discover()
    val echo = src.tools.find(_.name == "mcp__srv__echo").get
    val res = echo.call("""{"text":"hi"}""")
    assertEquals(res.output, "hi back")
    assert(!res.isError)

  asyncTest("a tool-level isError flag is carried onto the ToolResult"):
    val callResult = Json.Obj(
      List("content" -> Json.Arr(List(Json.Obj(List("type" -> Json.Str("text"), "text" -> Json.Str("boom"))))), "isError" -> Json.Bool(true))
    )
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList), "tools/call" -> Reply.Result(callResult))), List(config))
    src.discover()
    val res = src.tools.find(_.name == "mcp__srv__echo").get.call("""{"text":"hi"}""")
    assertEquals(res.output, "boom")
    assert(res.isError)

  asyncTest("a protocol-level failure surfaces as a tool error, not a thrown exception"):
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList), "tools/call" -> Reply.Error(-32000, "no such tool"))), List(config))
    src.discover()
    val res = src.tools.find(_.name == "mcp__srv__echo").get.call("""{"text":"hi"}""")
    assert(res.isError)
    assert(res.output.contains("MCP call failed"), res.output)
    assert(res.output.contains("no such tool"), res.output)

  asyncTest("read_mcp_resource renders the resource's text contents"):
    val readResult = Json.Obj(
      List(
        "contents" -> Json.Arr(
          List(Json.Obj(List("uri" -> Json.Str("stub://hello"), "mimeType" -> Json.Str("text/plain"), "text" -> Json.Str("hello world"))))
        )
      )
    )
    val hub = hubWith(Map("initialize" -> Reply.Result(initResult), "resources/read" -> Reply.Result(readResult)))
    val res = ReadMcpResource(hub).call("""{"server":"srv","uri":"stub://hello"}""")
    assertEquals(res.output, "hello world")

  asyncTest("read_mcp_resource surfaces a read failure as a tool error"):
    val hub = hubWith(Map("initialize" -> Reply.Result(initResult), "resources/read" -> Reply.Error(-32002, "not found")))
    val res = ReadMcpResource(hub).call("""{"server":"srv","uri":"stub://ghost"}""")
    assert(res.isError)
    assert(res.output.contains("MCP resource read failed"), res.output)

  asyncTest("one server throwing during listTools does not cancel a healthy server's discovery"):
    // The "bad" server throws synchronously when tools/list is written (an
    // unexpected throw, not a Left); the "good" server answers normally. Discovery
    // must still yield "good"'s tools — the throw must not propagate out of the
    // per-server future and cancel its siblings via the surrounding Async.group.
    val factory: McpServerConfig => (String => Unit, Int => Unit) => McpTransport = cfg =>
      if cfg.name == "bad" then (onLine, _) => new ThrowOnListTransport(onLine)
      else (onLine, onExit) => new ScriptedTransport(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList)), _ => (), onLine, onExit)
    val configs = List(McpServerConfig("bad", "cmd", Nil, Map.empty), McpServerConfig("good", "cmd", Nil, Map.empty))
    val src = McpToolSource(McpHub(configs, transportFactory = factory), configs)
    src.discover()
    val names = src.tools.map(_.name)
    assert(names.contains("mcp__good__echo"), names.toString)
    assert(!names.exists(_.startsWith("mcp__bad__")), names.toString)

  asyncTest("list_mcp_resources groups a server's resources, one line each"):
    val resList = Json.Obj(
      List(
        "resources" -> Json.Arr(
          List(Json.Obj(List("uri" -> Json.Str("stub://hello"), "name" -> Json.Str("hello"), "mimeType" -> Json.Str("text/plain"))))
        )
      )
    )
    val hub = hubWith(Map("initialize" -> Reply.Result(initResult), "resources/list" -> Reply.Result(resList)))
    val res = ListMcpResources(hub, List(config)).call("{}")
    assert(!res.isError)
    assert(res.output.contains("srv:"), res.output)
    assert(res.output.contains("stub://hello — hello (text/plain)"), res.output)

  // -- status snapshot (the `/mcp` panel's data) --------------------------------

  test("snapshot before discovery reports every server pending, in config order"):
    val configs = List(
      McpServerConfig("zulu", "npx", List("-y", "pkg"), Map("FOO" -> "1", "BAR" -> "2")),
      McpServerConfig("alpha", "node", Nil, Map.empty)
    )
    val factory: McpServerConfig => (String => Unit, Int => Unit) => McpTransport =
      _ => (onLine, onExit) => new ScriptedTransport(Map.empty, _ => (), onLine, onExit)
    val src = McpToolSource(McpHub(configs, transportFactory = factory), configs)
    val snap = src.snapshot
    assertEquals(snap.map(_.name), Vector("zulu", "alpha"))
    assertEquals(snap.map(_.state), Vector(McpServerState.Pending, McpServerState.Pending))
    assertEquals(snap.head.command, "npx -y pkg")
    assertEquals(snap.head.env, Vector("BAR", "FOO")) // names only, sorted — never values
    assert(snap.forall(_.error.isEmpty))
    assert(snap.forall(_.tools.isEmpty))

  asyncTest("a settled discovery lands a ready snapshot with tools and handshake facts"):
    val initWithVersion = Json.Obj(
      List(
        "protocolVersion" -> Json.Str("2025-06-18"),
        "capabilities" -> Json.Obj(Nil),
        "serverInfo" -> Json.Obj(List("name" -> Json.Str("stub"), "version" -> Json.Str("0.7.1")))
      )
    )
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initWithVersion), "tools/list" -> Reply.Result(toolsList))), List(config))
    src.discover()
    val view = src.snapshot.head
    assertEquals(view.state, McpServerState.Ready)
    assertEquals(view.tools.map(_.name), Vector("echo", "add"))
    assertEquals(view.tools.map(_.wireName), Vector("mcp__srv__echo", "mcp__srv__add"))
    assertEquals(view.tools.head.description, "echoes text")
    assertEquals(view.version, Some("0.7.1"))
    assertEquals(view.protocolVersion, Some("2025-06-18"))
    assertEquals(view.error, None)

  asyncTest("a failing server lands a failed snapshot carrying the error"):
    val src = McpToolSource(hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Error(-32000, "no tools for you"))), List(config))
    src.discover()
    val view = src.snapshot.head
    assertEquals(view.state, McpServerState.Failed)
    assert(view.error.exists(_.contains("no tools for you")), view.error.toString)
    assert(view.tools.isEmpty)

  asyncTest("onUpdate fires with a fresh snapshot each time a server settles"):
    val updates = mutable.ListBuffer.empty[Vector[McpServerView]]
    val src = McpToolSource(
      hubWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(toolsList))),
      List(config),
      onUpdate = updates += _
    )
    src.discover()
    assertEquals(updates.length, 1)
    assertEquals(updates.head.map(_.state), Vector(McpServerState.Ready))

  /** A transport that answers `initialize` normally (so connect succeeds) but
    * throws synchronously when the client writes `tools/list` — the "unexpected
    * throw from listTools" the discovery guard must contain. */
  private final class ThrowOnListTransport(onLine: String => Unit) extends McpTransport:
    def writeLine(line: String): Boolean =
      if line.contains("tools/list") then throw new RuntimeException("boom during list")
      // Answer the initialize request so connect() completes; ignore the id-less
      // notifications/initialized that follows.
      Json.parse(line).foreach: req =>
        for id <- JsonView.field(req, "id").collect { case Json.Num(n) => n.toLong }
        do
          if JsonView.str(req, "method").contains("initialize") then
            timers.setTimeout(0)(onLine(ScriptedTransport.responseLine(id, "result", initResult).render))
      true
    def kill(): Unit = ()
    def alive: Boolean = true
