package auk.workflow

import scala.scalajs.js
import scala.util.control.NonFatal

/** Display helpers for tool calls, shared by every surface that shows one: the
  * TUI transcript and its detail overlays, and the browser dashboard. They live
  * here, in the protocol module, because that is what the TUI and the webui have
  * in common — the alternative is the same string surgery written twice and
  * drifting.
  *
  * Everything here is BEST-EFFORT display. Nothing reverses a wire name or
  * re-types an argument; a shape that is not recognised falls back to showing
  * the raw text rather than guessing.
  */
object ToolDisplay:

  private val McpPrefix = "mcp__"

  /** The resource meta-tools auk injects alongside a server's own tools. */
  private val ResourceTools = Set("list_mcp_resources", "read_mcp_resource")

  /** `mcp__<server>__<tool>` rendered as `<server>.<tool>`, splitting at the
    * FIRST `__` after the prefix.
    *
    * The split cannot be exact: `McpNaming.sanitize` maps every character
    * outside `[A-Za-z0-9_-]` to `_`, so a server or tool name can legitimately
    * contain `__` and the wire name is not uniquely decodable. Splitting at the
    * first separator is the best guess for display. Anything that does not match
    * the shape — no separator, an empty server or tool half, or a name that is
    * not an MCP tool at all — is returned unchanged.
    */
  def prettyName(raw: String): String =
    if !raw.startsWith(McpPrefix) then raw
    else
      val rest = raw.substring(McpPrefix.length)
      val sep = rest.indexOf("__")
      if sep <= 0 || sep + 2 >= rest.length then raw
      else s"${rest.substring(0, sep)}.${rest.substring(sep + 2)}"

  /** Whether `name` belongs to the MCP family: a server's own tool, or one of the
    * resource meta-tools. This is the set of tools that gets the compact display
    * and folds into the transcript's quiet summary. */
  def isMcpFamily(name: String): Boolean =
    name.startsWith(McpPrefix) || ResourceTools.contains(name)

  /** A tool call's JSON arguments compacted onto ONE line, within `budget`
    * characters, or None when there is nothing worth showing.
    *
    * A top-level object renders as `{key: value, …}` — keys bare, values as
    * standard JSON, so nested objects and arrays keep their punctuation. Any
    * other root renders as plain JSON. Over budget, the text is cut mid-token
    * and closed with `…}` (`…` when the root is not an object), which is what
    * makes a long argument read as an elided preview rather than a truncation.
    *
    * None covers three cases that all mean "show nothing": arguments that do not
    * parse — the streaming path calls this on partial JSON every frame — a JSON
    * `null`, and an empty object, where `{}` would be pure noise.
    */
  def compactArgs(json: String, budget: Int): Option[String] =
    val parsed: js.Dynamic | Null =
      try js.JSON.parse(json)
      catch case NonFatal(_) => null
    if parsed == null then None
    else
      val value = parsed.nn
      val isObject = js.typeOf(value) == "object" && !js.Array.isArray(value)
      val rendered =
        if isObject then
          val keys = js.Object.keys(value.asInstanceOf[js.Object]).toList
          if keys.isEmpty then ""
          else keys.map(k => s"$k: ${js.JSON.stringify(value.selectDynamic(k))}").mkString("{", ", ", "}")
        else js.JSON.stringify(value)
      if rendered.isEmpty then None
      else if rendered.length <= budget then Some(rendered)
      else
        val ellipsis = if isObject then "…}" else "…"
        Some(rendered.take(math.max(0, budget - ellipsis.length)) + ellipsis)
