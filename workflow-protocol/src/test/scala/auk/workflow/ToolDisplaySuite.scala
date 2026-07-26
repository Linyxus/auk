package auk.workflow

class ToolDisplaySuite extends munit.FunSuite:

  /* ---- prettyName ---- */

  test("an mcp wire name renders as server.tool"):
    assertEquals(ToolDisplay.prettyName("mcp__linear__create_issue"), "linear.create_issue")

  test("the split takes the FIRST separator, so a tool name may contain __"):
    // `sanitize` turns any odd character into `_`, so `__` inside either half is
    // legitimate and the wire name is not uniquely decodable. First wins.
    assertEquals(ToolDisplay.prettyName("mcp__linear__create__issue"), "linear.create__issue")

  test("a name that does not fit the shape is returned unchanged"):
    assertEquals(ToolDisplay.prettyName("mcp__noseparator"), "mcp__noseparator")
    assertEquals(ToolDisplay.prettyName("mcp____tool"), "mcp____tool")   // empty server half
    assertEquals(ToolDisplay.prettyName("mcp__server__"), "mcp__server__") // empty tool half

  test("a non-mcp tool name passes through verbatim"):
    assertEquals(ToolDisplay.prettyName("eval_scala"), "eval_scala")
    assertEquals(ToolDisplay.prettyName("list_mcp_resources"), "list_mcp_resources")
    assertEquals(ToolDisplay.prettyName(""), "")

  /* ---- isMcpFamily ---- */

  test("the mcp family is a server's tools plus the two resource meta-tools"):
    assert(ToolDisplay.isMcpFamily("mcp__linear__create_issue"))
    assert(ToolDisplay.isMcpFamily("list_mcp_resources"))
    assert(ToolDisplay.isMcpFamily("read_mcp_resource"))
    assert(!ToolDisplay.isMcpFamily("eval_scala"))
    assert(!ToolDisplay.isMcpFamily("submit_result"))

  /* ---- compactArgs ---- */

  test("an object compacts to one line with bare keys and JSON values"):
    assertEquals(
      ToolDisplay.compactArgs("""{"title": "Fix crash", "count": 3}""", 60),
      Some("""{title: "Fix crash", count: 3}""")
    )

  test("key order follows the document, not a hash"):
    val out = ToolDisplay.compactArgs("""{"zebra":1,"apple":2,"mango":3}""", 60)
    assertEquals(out, Some("{zebra: 1, apple: 2, mango: 3}"))

  test("nested objects and arrays keep their JSON punctuation"):
    assertEquals(
      ToolDisplay.compactArgs("""{"filter":{"state":"open"},"ids":[1,2]}""", 80),
      Some("""{filter: {"state":"open"}, ids: [1,2]}""")
    )

  test("over budget, the text is cut and closed with an ellipsis brace"):
    val out = ToolDisplay.compactArgs("""{"title":"Fix crash on empty config","teamId":"ENG"}""", 40).get
    assertEquals(out.length, 40)
    assert(out.startsWith("""{title: "Fix crash on empty config""""), out)
    assert(out.endsWith("…}"), out)

  test("a result exactly at the budget is not truncated"):
    val exact = ToolDisplay.compactArgs("""{"a":"bcd"}""", 60).get
    assertEquals(exact, """{a: "bcd"}""")
    assertEquals(ToolDisplay.compactArgs("""{"a":"bcd"}""", exact.length), Some(exact))

  test("partial streamed JSON yields nothing rather than a broken preview"):
    assertEquals(ToolDisplay.compactArgs("""{"title": "Fix cra""", 60), None)
    assertEquals(ToolDisplay.compactArgs("", 60), None)
    assertEquals(ToolDisplay.compactArgs("not json at all", 60), None)

  test("an empty object is nothing to show"):
    assertEquals(ToolDisplay.compactArgs("{}", 60), None)

  test("a JSON null is nothing to show"):
    assertEquals(ToolDisplay.compactArgs("null", 60), None)

  test("a non-object root renders as plain JSON, elided without a brace"):
    assertEquals(ToolDisplay.compactArgs("[1,2,3]", 60), Some("[1,2,3]"))
    assertEquals(ToolDisplay.compactArgs("42", 60), Some("42"))
    val cut = ToolDisplay.compactArgs("[100,200,300,400,500]", 10).get
    assertEquals(cut.length, 10)
    assert(cut.endsWith("…") && !cut.endsWith("…}"), cut)
