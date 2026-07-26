package auk.config

import auk.platform.{Platform, PathOps}
import scala.collection.immutable.VectorMap

class AppConfigSuite extends munit.FunSuite:

  private def tempDir(): String =
    val d = PathOps.join(Platform.tmpdir(), "auk-cfg-" + Platform.uuid.random())
    Platform.fs.createDirectories(PathOps.join(d, ".auk"))
    d

  private def writeConfig(dir: String, text: String): Unit =
    Platform.fs.writeString(PathOps.join(dir, AppConfig.RelativePath), text)

  private def model(provider: String, id: String): Option[ModelConfig] =
    Some(ModelConfig(Some(provider), Some(id)))

  /** A config survives `render` iff parsing the rendered text gives it back. */
  private def assertRoundTrips(cfg: AppConfig)(implicit loc: munit.Location): Unit =
    val text = AppConfig.render(cfg)
    Config.parse[AppConfig](text) match
      case Right(back) =>
        assertEquals(back, cfg, s"rendered as:\n$text")
        // Map equality ignores order, so check the significant order explicitly.
        assertEquals(
          back.mcp.map(_.servers.keys.toList),
          cfg.mcp.map(_.servers.keys.toList),
          s"rendered as:\n$text"
        )
      case Left(errs) =>
        fail(s"rendered text did not parse: ${errs.map(_.render).mkString("; ")}\n$text")

  test("a missing config file yields the empty AppConfig") {
    assertEquals(AppConfig.load(tempDir()), Right(AppConfig.empty))
  }

  test("loads and parses a [model] section") {
    val d = tempDir()
    writeConfig(d, """[model]
                     |provider = openrouter
                     |id = minimax/minimax-m3""".stripMargin)
    assertEquals(AppConfig.load(d), Right(AppConfig(model("openrouter", "minimax/minimax-m3"), None)))
  }

  test("a partial section leaves the other field None") {
    val d = tempDir()
    writeConfig(d, "model.id = x")
    assertEquals(AppConfig.load(d), Right(AppConfig(Some(ModelConfig(None, Some("x"))), None)))
  }

  test("an unknown key is reported as an error") {
    val d = tempDir()
    writeConfig(d, "model.bogus = 1")
    assert(AppConfig.load(d).isLeft)
  }

  test("save then load round-trips a config") {
    val d = tempDir()
    val cfg = AppConfig(model("openrouter", "z-ai/glm-5.1"), None)
    assertEquals(AppConfig.save(cfg, d), Right(()))
    assertEquals(AppConfig.load(d), Right(cfg))
  }

  test("save creates the .auk directory if absent") {
    val bare = PathOps.join(Platform.tmpdir(), "auk-cfg-bare-" + Platform.uuid.random())
    val cfg = AppConfig(model("anthropic", "claude-opus-4-8"), None)
    assertEquals(AppConfig.save(cfg, bare), Right(()))
    assertEquals(AppConfig.load(bare), Right(cfg))
  }

  // ---------------------------------------------------------------------------
  // [mcp.servers.*]
  // ---------------------------------------------------------------------------

  test("loads a model section alongside several MCP servers") {
    val d = tempDir()
    writeConfig(d, """[model]
                     |provider = zai
                     |id = glm-5.2
                     |
                     |[mcp.servers.everything]
                     |command = npx
                     |args = -y @modelcontextprotocol/server-everything
                     |env.FOO = bar
                     |
                     |[mcp.servers.linear]
                     |command = linear-mcp""".stripMargin)
    assertEquals(
      AppConfig.load(d),
      Right(
        AppConfig(
          model("zai", "glm-5.2"),
          Some(
            McpSection(
              VectorMap(
                "everything" -> McpServerEntry(
                  "npx",
                  Some(List("-y", "@modelcontextprotocol/server-everything")),
                  VectorMap("FOO" -> "bar")
                ),
                "linear" -> McpServerEntry("linear-mcp", None, VectorMap.empty)
              )
            )
          )
        )
      )
    )
  }

  test("server declaration order is preserved") {
    val d = tempDir()
    writeConfig(d, """[mcp.servers.zulu]
                     |command = z
                     |[mcp.servers.alpha]
                     |command = a
                     |[mcp.servers.mike]
                     |command = m""".stripMargin)
    val names = AppConfig.load(d).toOption.flatMap(_.mcp).map(_.servers.keys.toList)
    assertEquals(names, Some(List("zulu", "alpha", "mike")))
  }

  test("a config with no [mcp] section has no MCP servers") {
    val d = tempDir()
    writeConfig(d, "model.id = x")
    assertEquals(AppConfig.load(d).map(_.mcp), Right(None))
  }

  test("a server missing `command` is an error naming the server") {
    val d = tempDir()
    writeConfig(d, "mcp.servers.foo.commnad = npx")
    AppConfig.load(d) match
      case Left(errs) =>
        assertEquals(
          errs.map(_.render),
          List(
            "mcp.servers.foo.command: missing required value",
            "line 1: mcp.servers.foo.commnad: unknown key"
          )
        )
      case Right(cfg) => fail(s"expected an error, got: $cfg")
  }

  test("a misspelled optional server field is an error, not a silent no-op") {
    // `arg = -y` used to decode cleanly and launch the server with no args.
    val d = tempDir()
    writeConfig(d, """[mcp.servers.foo]
                     |command = npx
                     |arg = -y""".stripMargin)
    assertEquals(
      AppConfig.load(d).left.map(_.map(_.render)),
      Left(List("line 3: mcp.servers.foo.arg: unknown key"))
    )
  }

  test("an unknown key beside [mcp.servers] is reported") {
    val d = tempDir()
    writeConfig(d, "mcp.bogus = 1")
    assert(AppConfig.load(d).left.exists(_.exists(_.message.contains("unknown key 'mcp.bogus'"))))
  }

  test("an args block form decodes like the inline form") {
    val d = tempDir()
    writeConfig(d, """[mcp.servers.s]
                     |command = uvx
                     |args =
                     |  mcp-server-git
                     |  --repository
                     |  /tmp""".stripMargin)
    assertEquals(
      AppConfig.load(d).toOption.flatMap(_.mcp).map(_.servers("s").args),
      Some(Some(List("mcp-server-git", "--repository", "/tmp")))
    )
  }

  // ---------------------------------------------------------------------------
  // render
  // ---------------------------------------------------------------------------

  test("render round-trips a model-only config") {
    assertRoundTrips(AppConfig(model("zai", "glm-5.2"), None))
    assertRoundTrips(AppConfig(Some(ModelConfig(None, Some("x"))), None))
  }

  test("render round-trips model plus MCP servers, in order") {
    assertRoundTrips(
      AppConfig(
        model("zai", "glm-5.2"),
        Some(
          McpSection(
            VectorMap(
              "zulu" -> McpServerEntry("npx", Some(List("-y", "server-everything")), VectorMap("FOO" -> "bar")),
              "alpha" -> McpServerEntry("linear-mcp", None, VectorMap.empty)
            )
          )
        )
      )
    )
  }

  test("render distinguishes an empty args list from an absent one") {
    val cfg = AppConfig(
      None,
      Some(
        McpSection(
          VectorMap(
            "empty" -> McpServerEntry("c", Some(Nil), VectorMap.empty),
            "absent" -> McpServerEntry("c", None, VectorMap.empty)
          )
        )
      )
    )
    assertRoundTrips(cfg)
    val servers = Config.parse[AppConfig](AppConfig.render(cfg)).toOption.flatMap(_.mcp).get.servers
    assertEquals(servers("empty").args, Some(Nil))
    assertEquals(servers("absent").args, None)
  }

  test("render round-trips values the parser would otherwise reinterpret") {
    assertRoundTrips(
      AppConfig(
        None,
        Some(
          McpSection(
            VectorMap(
              "nasty" -> McpServerEntry(
                command = "my server",
                args = Some(List("a b", "say \"hi\"", "C:\\path", "", "\"quoted\"", "trailing\\")),
                env = VectorMap(
                  "SPACED" -> "  padded  ",
                  "QUOTED" -> "\"quoted\"",
                  "EMPTY" -> "",
                  "HASH" -> "a # b",
                  "EQUALS" -> "a=b",
                  "SLASH" -> "C:\\path"
                )
              )
            )
          )
        )
      )
    )
  }

  test("a model switch preserves the MCP section on disk") {
    // `save` rewrites the whole file, so a section render forgot would be lost.
    val d = tempDir()
    val original = AppConfig(
      model("zai", "glm-5.2"),
      Some(McpSection(VectorMap("linear" -> McpServerEntry("linear-mcp", None, VectorMap("K" -> "v")))))
    )
    assertEquals(AppConfig.save(original, d), Right(()))
    val loaded = AppConfig.load(d).toOption.get
    assertEquals(AppConfig.save(loaded.copy(model = model("anthropic", "claude-opus-5")), d), Right(()))
    assertEquals(AppConfig.load(d), Right(original.copy(model = model("anthropic", "claude-opus-5"))))
  }
