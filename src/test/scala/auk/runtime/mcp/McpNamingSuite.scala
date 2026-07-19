package auk.runtime.mcp

/** Pure tests for MCP wire-name construction: sanitization, the 64-char cap
  * (tool part only), and collision disambiguation. */
class McpNamingSuite extends munit.FunSuite:

  test("a plain name is mcp__<server>__<tool>, unchanged"):
    assertEquals(McpNaming.name("files", "read"), "mcp__files__read")

  test("characters outside [A-Za-z0-9_-] are replaced with _ in both server and tool"):
    assertEquals(McpNaming.name("my server", "get/thing.v2"), "mcp__my_server__get_thing_v2")
    // _ and - survive.
    assertEquals(McpNaming.name("a-b_c", "x-y_z"), "mcp__a-b_c__x-y_z")

  test("the name is capped at 64 by truncating only the tool part"):
    val long = McpNaming.name("s", "x" * 100)
    assertEquals(long.length, 64)
    assert(long.startsWith("mcp__s__"), long)
    // the prefix (11 chars here) is intact; the remaining 56 chars are the tool.
    assertEquals(long, "mcp__s__" + "x" * 56)

  test("a pathologically long server name is not truncated (may exceed the cap)"):
    val server = "s" * 80
    val n = McpNaming.name(server, "tool")
    assert(n.startsWith("mcp__" + server + "__"), n)
    // tool part gets no room, but the prefix/server is preserved verbatim.
    assertEquals(n, "mcp__" + server + "__")

  test("assign disambiguates post-sanitization collisions with _2, _3…"):
    // Two tools that sanitize to the same wire name.
    val names = McpNaming.assign(List("srv" -> "a.b", "srv" -> "a/b", "srv" -> "a b"))
    assertEquals(names, List("mcp__srv__a_b", "mcp__srv__a_b_2", "mcp__srv__a_b_3"))

  test("distinct tools on the same server keep distinct names"):
    val names = McpNaming.assign(List("srv" -> "read", "srv" -> "write"))
    assertEquals(names, List("mcp__srv__read", "mcp__srv__write"))

  test("a collision suffix keeps the whole name within the 64-char cap"):
    val tool = "a" * 100
    val names = McpNaming.assign(List("s" -> tool, "s" -> tool))
    assertEquals(names(0).length, 64)
    assertEquals(names(1).length, 64)
    assert(names(1).endsWith("_2"), names(1))
    assertNotEquals(names(0), names(1))
