package auk.library

import scala.scalajs.js

/** `lib.mcp` — the pure request-building and response-decoding of [[McpImpl]]
  * (the thin `spawnSync` glue is exercised end-to-end in Phase 5), plus the
  * "MCP unavailable" path, which short-circuits before any spawn. */
class McpSuite extends LibSuite:

  // -- request building --------------------------------------------------------

  test("request builds a well-formed op line with embedded object args"):
    val d = js.JSON.parse(McpImpl.request("callTool", server = Some("fs"), tool = Some("read"), argsJson = Some("""{"path":"/x"}""")))
    assertEquals(d.op.asInstanceOf[String], "callTool")
    assertEquals(d.server.asInstanceOf[String], "fs")
    assertEquals(d.tool.asInstanceOf[String], "read")
    assertEquals(d.args.path.asInstanceOf[String], "/x")
    assert(!js.isUndefined(d.id), "request carries an id")

  test("request omits absent fields"):
    val d = js.JSON.parse(McpImpl.request("listServers"))
    assertEquals(d.op.asInstanceOf[String], "listServers")
    assert(js.isUndefined(d.server), "server omitted")
    assert(js.isUndefined(d.args), "args omitted")

  test("request rejects arguments that are not valid JSON"):
    interceptContains("JSON object")(McpImpl.request("callTool", server = Some("s"), tool = Some("t"), argsJson = Some("{not json")))

  test("request rejects arguments that are not a JSON object"):
    interceptContains("JSON object")(McpImpl.request("callTool", server = Some("s"), tool = Some("t"), argsJson = Some("[1,2]")))

  // -- response parsing --------------------------------------------------------

  test("parseResponse returns data on ok:true"):
    val data = McpImpl.parseResponse("""{"id":1,"ok":true,"data":{"servers":["a"]}}""")
    assertEquals(McpImpl.serversOf(data), List("a"))

  test("parseResponse throws the server's error on ok:false, tolerating an id-less line"):
    interceptContains("boom")(McpImpl.parseResponse("""{"ok":false,"error":"boom"}"""))

  test("parseResponse throws on a non-JSON response"):
    interceptContains("non-JSON")(McpImpl.parseResponse("not json at all"))

  test("parseResponse throws on a response missing the ok field"):
    interceptContains("unexpected")(McpImpl.parseResponse("""{"foo":1}"""))

  // -- decoders ----------------------------------------------------------------

  test("serversOf decodes the server-name list"):
    assertEquals(McpImpl.serversOf(js.JSON.parse("""{"servers":["fs","github"]}""")), List("fs", "github"))

  test("toolsOf decodes tools and renders inputSchema back to a JSON string, defaulting absent fields"):
    val data = js.JSON.parse(
      """{"tools":[{"name":"add","description":"adds","inputSchema":{"type":"object","properties":{"a":{"type":"integer"}}}},{"name":"noschema"}]}"""
    )
    val tools = McpImpl.toolsOf(data)
    assertEquals(tools.map(_.name), List("add", "noschema"))
    assertEquals(tools.head.description, "adds")
    assert(tools.head.inputSchema.contains("\"type\":\"object\""), tools.head.inputSchema)
    assertEquals(tools(1).description, "")
    assertEquals(tools(1).inputSchema, "{}")

  test("resourcesOf decodes, defaulting absent optional fields to empty strings"):
    val data = js.JSON.parse("""{"resources":[{"uri":"u1","name":"A","mimeType":"text/plain"},{"uri":"u2"}]}""")
    val rs = McpImpl.resourcesOf(data)
    assertEquals(rs.map(_.uri), List("u1", "u2"))
    assertEquals(rs.head.mimeType, "text/plain")
    assertEquals(rs(1).name, "")
    assertEquals(rs(1).description, "")
    assertEquals(rs(1).mimeType, "")

  test("callResultOf decodes content + isError; text joins text blocks"):
    val data = js.JSON.parse("""{"content":[{"kind":"text","text":"hello","mimeType":null,"uri":null},{"kind":"text","text":"world"}],"isError":false}""")
    val r = McpImpl.callResultOf(data)
    assert(!r.isError)
    assertEquals(r.content.map(_.kind), List("text", "text"))
    assertEquals(r.text, "hello\nworld")

  test("callResultOf carries isError:true; toString marks a tool error"):
    val r = McpImpl.callResultOf(js.JSON.parse("""{"content":[{"kind":"text","text":"boom"}],"isError":true}"""))
    assert(r.isError)
    assertEquals(r.text, "boom")
    assert(r.toString.contains("[tool error]"), r.toString)

  test("readResultOf decodes contents and their uris"):
    val r = McpImpl.readResultOf(js.JSON.parse("""{"contents":[{"kind":"text","text":"file body","uri":"u1"}]}"""))
    assertEquals(r.text, "file body")
    assertEquals(r.contents.head.uri, "u1")

  test("callResultOf decodes a non-text block, leaving its text empty"):
    val r = McpImpl.callResultOf(js.JSON.parse("""{"content":[{"kind":"image","mimeType":"image/png"}],"isError":false}"""))
    assertEquals(r.content.head.kind, "image")
    assertEquals(r.content.head.text, "")
    assertEquals(r.content.head.mimeType, "image/png")
    assertEquals(r.text, "")

  test("readResultOf decodes an empty contents list"):
    val r = McpImpl.readResultOf(js.JSON.parse("""{"contents":[]}"""))
    assertEquals(r.contents, Nil)
    assertEquals(r.text, "")

  // -- unavailable path (short-circuits before any spawn) ----------------------

  test("every op is unavailable when AUK_MCP_SOCK is unset"):
    withoutSock:
      assert(!lib.mcp.available)
      interceptContains("unavailable")(lib.mcp.servers)
      interceptContains("unavailable")(lib.mcp.server("x").tools)
      val out = captured(lib.mcp.overview())
      assert(out.toLowerCase.contains("unavailable"), out)

  /** Run `body` with `AUK_MCP_SOCK` guaranteed unset, restoring any prior value. */
  private def withoutSock(body: => Unit): Unit =
    val env = js.Dynamic.global.process.env
    val prev = env.selectDynamic("AUK_MCP_SOCK")
    js.special.delete(env, "AUK_MCP_SOCK")
    try body
    finally if !js.isUndefined(prev) && prev != null then env.updateDynamic("AUK_MCP_SOCK")(prev)
