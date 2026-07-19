package auk.runtime.mcp

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.RuntimeContext
import auk.platform.Platform

/** End-to-end integration: [[McpToolSource]] against the real node stub server
  * (`src/test/resources/mcp/stub-server.mjs`) over a spawned stdio child. Unlike
  * [[McpClientLiveSuite]] this is NOT gated — the stub is a plain node script (no
  * npx/network), and the test runner is node, so it runs by default. It proves
  * the whole native-MCP path: discovery finds the server's tools as adapters,
  * calling an adapter reaches the server and returns its text, and
  * `read_mcp_resource` reads a resource. */
class McpToolSourceIntegrationSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  private given RuntimeContext = RuntimeContext.cwd()

  private def stubPath: String =
    val path = js.Dynamic.global.require("node:path")
    path.join(Platform.cwd(), "src/test/resources/mcp/stub-server.mjs").asInstanceOf[String]

  private def exists(p: String): Boolean =
    js.Dynamic.global.require("node:fs").existsSync(p).asInstanceOf[Boolean]

  test("McpToolSource discovers the stub's tools, calls one, and reads a resource"):
    assert(exists(stubPath), s"stub server not found at $stubPath (cwd=${Platform.cwd()})")
    Async.fromSync:
      val config = McpServerConfig("stub", "node", List(stubPath), Map.empty)
      val hub = McpHub(List(config))
      try
        val src = McpToolSource(hub, List(config))
        src.discover()

        val names = src.tools.map(_.name)
        assert(names.contains("mcp__stub__echo"), names.toString)
        assert(names.contains("mcp__stub__add"), names.toString)
        assert(names.contains("list_mcp_resources"), names.toString)
        assert(names.contains("read_mcp_resource"), names.toString)

        // The echo adapter round-trips through the real server.
        val echoed = src.tools.find(_.name == "mcp__stub__echo").get.call("""{"text":"hi there"}""")
        assert(!echoed.isError, echoed.output)
        assertEquals(echoed.output, "hi there")

        // read_mcp_resource reads the stub's resource by uri.
        val read = ReadMcpResource(hub).call("""{"server":"stub","uri":"stub://hello"}""")
        assert(!read.isError, read.output)
        assertEquals(read.output, "hello world")

        // list_mcp_resources surfaces the stub's resource line.
        val listed = ListMcpResources(hub, List(config)).call("{}")
        assert(listed.output.contains("stub://hello — hello (text/plain)"), listed.output)
      finally hub.close()
