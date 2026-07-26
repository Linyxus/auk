package auk.runtime.mcp

import scala.collection.immutable.VectorMap

import auk.config.{AppConfig, Config, McpSection, McpServerEntry}

/** The `.auk/config` → runtime projection: `[mcp.servers.*]` becomes the list of
  * servers the hub launches, in declaration order.
  */
class McpServerConfigSuite extends munit.FunSuite:

  private def fromText(text: String): List[McpServerConfig] =
    Config.parse[AppConfig](text) match
      case Right(cfg) => McpServerConfig.fromAppConfig(cfg)
      case Left(errs) => fail(s"unexpected errors: ${errs.map(_.render).mkString("; ")}")

  test("a config with no [mcp] section yields no servers") {
    assertEquals(McpServerConfig.fromAppConfig(AppConfig.empty), Nil)
    assertEquals(fromText("model.id = x"), Nil)
  }

  test("servers keep their declaration order") {
    val servers = fromText("""[mcp.servers.zulu]
                             |command = z
                             |[mcp.servers.alpha]
                             |command = a
                             |[mcp.servers.mike]
                             |command = m""".stripMargin)
    assertEquals(servers.map(_.name), List("zulu", "alpha", "mike"))
  }

  test("every field carries over, keyed by the section name") {
    val servers = fromText("""[mcp.servers.everything]
                             |command = npx
                             |args = -y @modelcontextprotocol/server-everything
                             |env.FOO = bar
                             |env.BAZ = qux""".stripMargin)
    assertEquals(
      servers,
      List(
        McpServerConfig(
          "everything",
          "npx",
          List("-y", "@modelcontextprotocol/server-everything"),
          Map("FOO" -> "bar", "BAZ" -> "qux")
        )
      )
    )
  }

  test("an omitted args or env defaults to empty") {
    val entry = McpServerEntry("run-me", None, VectorMap.empty)
    assertEquals(
      McpServerConfig.fromAppConfig(AppConfig(None, Some(McpSection(VectorMap("solo" -> entry))))),
      List(McpServerConfig("solo", "run-me", Nil, Map.empty))
    )
  }
