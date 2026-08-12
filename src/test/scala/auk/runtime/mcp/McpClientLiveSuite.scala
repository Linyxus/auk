package auk.runtime.mcp

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.Json
import auk.platform.Platform

/** Real-protocol interop: a real [[McpClient]] over [[LineProcessTransport]]
  * speaking newline-delimited JSON-RPC 2.0 to a real node stub server
  * (`src/test/resources/mcp/stub-server.mjs`). This is the ONLY test that exercises
  * the actual stdio wire end to end — every other MCP test uses a scripted
  * transport or a mock helper — so it is the proof that our client and a genuine
  * external server agree on the protocol.
  *
  * Gated behind `AUK_MCP_E2E` (it spawns a node child), so the default test run
  * skips it; run with `AUK_MCP_E2E=1 ./mill test.testOnly '*McpClientLiveSuite'`. */
class McpClientLiveSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  private def e2eEnabled: Boolean =
    val v = js.Dynamic.global.process.env.AUK_MCP_E2E
    !js.isUndefined(v) && v != null && v.asInstanceOf[String].nonEmpty

  private def stubPath: String =
    val path = js.Dynamic.global.require("node:path")
    path.join(Platform.cwd(), "src/test/resources/mcp/stub-server.mjs").asInstanceOf[String]

  private def exists(p: String): Boolean =
    js.Dynamic.global.require("node:fs").existsSync(p).asInstanceOf[Boolean]

  private def newClient(): McpClient =
    val config = McpServerConfig("stub", "node", List(stubPath), Map.empty)
    McpClient("stub", LineProcessTransport.factory(config))

  test("real McpClient interop against the node stub server"):
    assume(e2eEnabled, "set AUK_MCP_E2E=1 to run the live MCP interop test (needs node)")
    assert(exists(stubPath), s"stub server not found at $stubPath (cwd=${Platform.cwd()})")
    Async.fromSync:
      val c = newClient()
      try
        // connect: handshake succeeds and records the server's identity.
        assert(c.connect().isRight, "connect")
        assertEquals(c.serverInfo.flatMap(si => JsonView.str(si, "name")), Some("stub"))

        // tools/list: both tools, with echo's schema carried through as data.
        c.listTools() match
          case Right(tools) =>
            assertEquals(tools.map(_.name), List("echo", "add"))
            assert(tools.head.inputSchema.render.contains("text"), tools.head.inputSchema.render)
          case Left(e) => fail(s"listTools: $e")

        // tools/call: echo round-trips the text; add computes; a non-tool is isError.
        c.callTool("echo", Json.Obj(List("text" -> Json.Str("hi")))) match
          case Right(r) => assert(!r.isError, "echo not error"); assertEquals(r.content.flatMap(_.text), List("hi"))
          case Left(e)  => fail(s"callTool echo: $e")
        c.callTool("add", Json.Obj(List("a" -> Json.num(2), "b" -> Json.num(3)))) match
          case Right(r) => assert(!r.isError, "add not error"); assertEquals(r.content.flatMap(_.text), List("5"))
          case Left(e)  => fail(s"callTool add: $e")
        c.callTool("nope", Json.Obj(Nil)) match
          case Right(r) => assert(r.isError, "unknown tool must report isError")
          case Left(e)  => fail(s"callTool nope: $e")

        // resources/list + resources/read.
        c.listResources() match
          case Right(rs) => assertEquals(rs.map(_.uri), List("stub://hello"))
          case Left(e)   => fail(s"listResources: $e")
        c.readResource("stub://hello") match
          case Right(rr) => assertEquals(rr.contents.flatMap(_.text), List("hello world"))
          case Left(e)   => fail(s"readResource: $e")
      finally c.close()
