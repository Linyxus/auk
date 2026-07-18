package auk.library

import scala.scalajs.js

/** End-to-end coverage of the `lib.mcp` proxy GLUE: a mock helper `.mjs` pointed
  * to by `AUK_MCP_HELPER_JS` echoes canned per-op responses, so a real `spawnSync`
  * exercises argv-building, request-on-stdin, the single-line reply parse, and the
  * decoders together — without a host bridge. The canned responses carry the decode
  * edge cases (a tool error, a non-text block, a resource with absent fields, an
  * empty content list, a protocol `ok:false`). The real bridge path is Phase 5.
  *
  * `spawnSync` in a test is the established `ShellSuite` pattern; the mock ignores
  * the (dummy) socket, so there is no host dependency and no deadlock. */
class McpProxySuite extends LibSuite:

  // A mock helper: read the request on stdin, print a canned response, flush, exit.
  // Written flush-left (no stripMargin) so the JS `||` is untouched.
  private val MockHelper =
    """let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (c) => { input += c; });
process.stdin.on("end", () => {
  let req = {};
  try { req = JSON.parse(input.trim()); } catch (_) {}
  const send = (obj) => process.stdout.write(JSON.stringify(obj) + "\n", () => process.exit(0));
  const ok = (data) => send({ ok: true, data });
  if (req.server === "nope") { send({ ok: false, error: "unknown MCP server 'nope'" }); return; }
  switch (req.op) {
    case "listServers": ok({ servers: ["alpha", "beta"] }); break;
    case "listTools": ok({ tools: [
      { name: "add", description: "adds two numbers", inputSchema: { type: "object", properties: { a: { type: "integer" } } } },
      { name: "noschema" }
    ] }); break;
    case "callTool":
      if (req.tool === "boom") ok({ content: [{ kind: "text", text: "it failed" }], isError: true });
      else if (req.tool === "mixed") ok({ content: [{ kind: "text", text: "hi" }, { kind: "image", mimeType: "image/png" }], isError: false });
      else ok({ content: [{ kind: "text", text: "args=" + JSON.stringify(req.args || {}) }], isError: false });
      break;
    case "listResources": ok({ resources: [{ uri: "res://a", name: "A" }] }); break;
    case "readResource":
      if (req.uri === "empty") ok({ contents: [] });
      else ok({ contents: [{ kind: "text", text: "body of " + req.uri, uri: req.uri }] });
      break;
    default: send({ ok: false, error: "mock: unknown op " + req.op });
  }
});
"""

  private lazy val mockHelperPath: String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    val fs = js.Dynamic.global.require("node:fs")
    val p = path.join(os.tmpdir(), s"auk-mcp-mock-${js.Dynamic.global.process.pid}.mjs").asInstanceOf[String]
    fs.writeFileSync(p, MockHelper, "utf8")
    p

  /** Run `body` with the env the proxy reads: a (dummy) socket so it is "available",
    * and the mock helper as `AUK_MCP_HELPER_JS`. Restores prior values. */
  private def withMcp(body: => Unit): Unit =
    val env = js.Dynamic.global.process.env
    val prevSock = env.selectDynamic("AUK_MCP_SOCK")
    val prevHelper = env.selectDynamic("AUK_MCP_HELPER_JS")
    env.updateDynamic("AUK_MCP_SOCK")("dummy")
    env.updateDynamic("AUK_MCP_HELPER_JS")(mockHelperPath)
    try body
    finally
      restore(env, "AUK_MCP_SOCK", prevSock)
      restore(env, "AUK_MCP_HELPER_JS", prevHelper)

  private def restore(env: js.Dynamic, name: String, prev: js.Dynamic): Unit =
    if js.isUndefined(prev) || prev == null then js.special.delete(env, name)
    else env.updateDynamic(name)(prev)

  // -- tests -------------------------------------------------------------------

  test("available is true with a socket; servers round-trips through the helper"):
    withMcp:
      assert(lib.mcp.available)
      assertEquals(lib.mcp.servers, List("alpha", "beta"))

  test("tools decode end-to-end, with a readable toString and a defaulted schema"):
    withMcp:
      val tools = lib.mcp.server("alpha").tools
      assertEquals(tools.map(_.name), List("add", "noschema"))
      assertEquals(tools.head.toString, "add — adds two numbers")
      assert(tools.head.inputSchema.contains("\"type\":\"object\""), tools.head.inputSchema)
      assertEquals(tools(1).inputSchema, "{}")

  test("callTool forwards the JSON args through the round-trip"):
    withMcp:
      val r = lib.mcp.server("alpha").callTool("echo", """{"path":"/x"}""")
      assert(!r.isError)
      assert(r.text.contains("\"path\":\"/x\""), r.text)

  test("a tool error is data (isError:true), not thrown"):
    withMcp:
      val r = lib.mcp.server("alpha").callTool("boom")
      assert(r.isError)
      assertEquals(r.text, "it failed")
      assert(r.toString.contains("[tool error]"), r.toString)

  test("a non-text content block decodes with empty text; text joins only text blocks"):
    withMcp:
      val r = lib.mcp.server("alpha").callTool("mixed")
      assertEquals(r.content.map(_.kind), List("text", "image"))
      assertEquals(r.content(1).text, "")
      assertEquals(r.text, "hi")

  test("resources decode with absent description/mimeType as empty, and a readable toString"):
    withMcp:
      val rs = lib.mcp.server("alpha").resources
      assertEquals(rs.map(_.uri), List("res://a"))
      assertEquals(rs.head.description, "")
      assertEquals(rs.head.mimeType, "")
      assertEquals(rs.head.toString, "res://a — A")

  test("readResource returns text; an empty contents list yields empty text"):
    withMcp:
      assertEquals(lib.mcp.server("alpha").readResource("res://a").text, "body of res://a")
      assertEquals(lib.mcp.server("alpha").readResource("empty").text, "")

  test("a protocol failure (ok:false) is thrown, not returned"):
    withMcp:
      interceptContains("unknown MCP server 'nope'")(lib.mcp.server("nope").tools)
