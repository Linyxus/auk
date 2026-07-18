package auk.runtime.mcp

import auk.platform.{Platform, PathOps}

/** Parsing rules for `.auk/mcp.json`. The shape logic is tested through
  * [[McpConfig.parse]] (no filesystem), and the absent/present-file contract of
  * [[McpConfig.load]] through a scratch directory. */
class McpConfigSuite extends munit.FunSuite:

  private var counter = 0
  private def freshDir(): String =
    counter += 1
    val os = scala.scalajs.js.Dynamic.global.require("node:os")
    val dir = PathOps.join(os.tmpdir().asInstanceOf[String], s"auk-mcp-cfg-${scala.scalajs.js.Dynamic.global.process.pid}-$counter")
    Platform.fs.createDirectories(dir)
    dir

  test("parses a valid config, preserving server order and per-field values"):
    val text =
      """{
        |  "mcpServers": {
        |    "filesystem": {
        |      "command": "npx",
        |      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
        |      "env": { "FOO": "bar" }
        |    },
        |    "git": { "command": "uvx", "args": ["mcp-server-git"] }
        |  }
        |}""".stripMargin
    McpConfig.parse(text) match
      case Right(servers) =>
        assertEquals(servers.map(_.name), List("filesystem", "git"))
        val fs = servers.head
        assertEquals(fs.command, "npx")
        assertEquals(fs.args, List("-y", "@modelcontextprotocol/server-filesystem", "/tmp"))
        assertEquals(fs.env, Map("FOO" -> "bar"))
        val git = servers(1)
        assertEquals(git.args, List("mcp-server-git"))
        // args present, env absent → defaulted empty
        assertEquals(git.env, Map.empty[String, String])
      case Left(err) => fail(err)

  test("args and env default to empty when omitted"):
    val text = """{ "mcpServers": { "solo": { "command": "run-me" } } }"""
    McpConfig.parse(text) match
      case Right(List(cfg)) =>
        assertEquals(cfg.command, "run-me")
        assertEquals(cfg.args, Nil)
        assertEquals(cfg.env, Map.empty[String, String])
      case other => fail(s"unexpected: $other")

  test("a file with no mcpServers key yields no servers"):
    assertEquals(McpConfig.parse("""{ "other": 1 }"""), Right(Nil))

  test("malformed JSON is a Left"):
    val r = McpConfig.parse("{ not json ")
    assert(r.isLeft, r.toString)
    assert(r.left.exists(_.contains("not valid JSON")), r.toString)

  test("a server missing `command` is a Left naming the server"):
    val r = McpConfig.parse("""{ "mcpServers": { "broken": { "args": ["x"] } } }""")
    assert(r.isLeft, r.toString)
    assert(r.left.exists(m => m.contains("broken") && m.contains("command")), r.toString)

  test("a non-string command is a Left"):
    val r = McpConfig.parse("""{ "mcpServers": { "s": { "command": 42 } } }""")
    assert(r.left.exists(_.contains("must be a string")), r.toString)

  test("non-string args elements are a Left"):
    val r = McpConfig.parse("""{ "mcpServers": { "s": { "command": "c", "args": [1, 2] } } }""")
    assert(r.left.exists(_.contains("array of strings")), r.toString)

  test("a non-string env value is a Left"):
    val r = McpConfig.parse("""{ "mcpServers": { "s": { "command": "c", "env": { "K": 3 } } } }""")
    assert(r.left.exists(_.contains("must be a string")), r.toString)

  test("mcpServers of the wrong type is a Left"):
    val r = McpConfig.parse("""{ "mcpServers": [] }""")
    assert(r.left.exists(_.contains("`mcpServers` must be an object")), r.toString)

  test("a non-object top level is a Left"):
    assert(McpConfig.parse("[]").left.exists(_.contains("top level must be an object")))

  test("load: an absent file is not an error"):
    val dir = freshDir()
    assertEquals(McpConfig.load(dir), Right(Nil))

  test("load: a present file is read and parsed"):
    val dir = freshDir()
    Platform.fs.createDirectories(PathOps.join(dir, ".auk"))
    Platform.fs.writeString(
      PathOps.join(dir, McpConfig.RelativePath),
      """{ "mcpServers": { "echo": { "command": "cat" } } }"""
    )
    McpConfig.load(dir) match
      case Right(List(cfg)) => assertEquals(cfg.name, "echo"); assertEquals(cfg.command, "cat")
      case other            => fail(s"unexpected: $other")
