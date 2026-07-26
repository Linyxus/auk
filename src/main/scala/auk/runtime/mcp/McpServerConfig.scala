package auk.runtime.mcp

import auk.config.AppConfig

/** One configured MCP server: a single stdio server auk may launch.
  *
  * The shape mirrors the de-facto ecosystem format (Claude Desktop / Cursor /
  * the reference servers): a `command` plus optional `args` and `env`. `command`
  * is the executable (e.g. `npx`), `args` its argv tail, and `env` process
  * environment overrides layered on top of auk's own environment when the child
  * is spawned. Only `command` is mandatory; `args` and `env` default to empty so
  * the common single-binary server is a one-liner.
  */
final case class McpServerConfig(
    name: String,
    command: String,
    args: List[String],
    env: Map[String, String]
)

object McpServerConfig:
  /** The runtime view of the `[mcp.servers.*]` sections of `.auk/config`.
    *
    * Declaration order is preserved: it is the order servers — and therefore
    * their tools — are exposed to agents. A config with no `[mcp]` section
    * contributes no servers, which is a normal state, not an error.
    */
  def fromAppConfig(config: AppConfig): List[McpServerConfig] =
    config.mcp.toList.flatMap(_.servers.toList).map { (name, entry) =>
      McpServerConfig(name, entry.command, entry.args.getOrElse(Nil), entry.env.toMap)
    }
