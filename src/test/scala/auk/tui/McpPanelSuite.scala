package auk.tui

import auk.tui.app.{Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, McpServerState, McpServerView, McpToolView, UserCommand}

/** The MCP server inspector: `/mcp` (or Ctrl+C s) opens a fullscreen list of
  * every configured server with its discovery status; Enter opens one server's
  * detail page (fact sheet + tool list). The data is [[ChatState.mcpServers]],
  * replaced wholesale by each [[AgentEvent.McpUpdated]] snapshot. */
class McpPanelSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

  private def keyEventFor(app: ChatApp, state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil
    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  /** Render the fullscreen view of `state` to plain text. */
  private def fullscreenLines(app: ChatApp, state: ChatState, width: Int = 100, rows: Int = 30): Vector[String] =
    app.view(state, Viewport(width, rows)).fullscreen match
      case Some(el) => Layout.lay(el, width).map(_.plain)
      case None     => fail("expected a fullscreen element")

  private val ready = McpServerView(
    name = "everything",
    command = "npx -y @modelcontextprotocol/server-everything",
    env = Vector("FOO"),
    state = McpServerState.Ready,
    error = None,
    version = Some("0.6.2"),
    protocolVersion = Some("2025-06-18"),
    tools = Vector(
      McpToolView("echo", "mcp__everything__echo", "Echoes back whatever message you send"),
      McpToolView("add", "mcp__everything__add", "Adds two numbers")
    )
  )
  private val pending =
    McpServerView("slowpoke", "node slow.js", Vector.empty, McpServerState.Pending, None, None, None, Vector.empty)
  private val failed = McpServerView(
    "broken", "node missing.js", Vector.empty, McpServerState.Failed,
    Some("MCP connect to 'broken' failed: spawn ENOENT"), None, None, Vector.empty
  )

  private def withServers(overlay: Overlay = Overlay.McpServers(0)): ChatState =
    ChatState.initial.copy(mcpServers = Vector(ready, pending, failed), overlay = overlay)

  // -- registration: one Command, two doors -----------------------------------

  test("/mcp is a registered slash command riding the Ctrl+C s chord"):
    val cmds = ChatApp.defaultCommands(UnboundedChannel[UserCommand](), UnboundedChannel[Unit](), Vector.empty)
    val mcp = ChatApp.slashMatches(cmds, "mcp")
    assertEquals(mcp.map(_.names.head), Vector("mcp"))
    assertEquals(mcp.head.keys, Vector("s"))

  test("running the command opens the server list, selection at the top"):
    val cmds = ChatApp.defaultCommands(UnboundedChannel[UserCommand](), UnboundedChannel[Unit](), Vector.empty)
    val (next, _) = ChatApp.slashMatches(cmds, "mcp").head.run(ChatState.initial)
    assertEquals(next.overlay, Overlay.McpServers(0))

  // -- key routing --------------------------------------------------------------

  test("list keys: ↑/↓ select, Enter opens the detail, Esc closes"):
    val app = appUI
    val list = withServers()
    assertEquals(keyEventFor(app, list, Key.Up), Some(Event.McpListUp))
    assertEquals(keyEventFor(app, list, Key.Down), Some(Event.McpListDown))
    assertEquals(keyEventFor(app, list, Key.Enter), Some(Event.McpOpen))
    assertEquals(keyEventFor(app, list, Key.Esc), Some(Event.HideOverlay))

  test("detail keys: arrows scroll (top-anchored), g jumps to the top, Esc goes back"):
    val app = appUI
    val detail = withServers(Overlay.McpServerDetail("everything", 0))
    assertEquals(keyEventFor(app, detail, Key.Down), Some(Event.McpDetailScroll(1)))
    assertEquals(keyEventFor(app, detail, Key.Up), Some(Event.McpDetailScroll(-1)))
    assertEquals(keyEventFor(app, detail, Key.Char('g')), Some(Event.McpDetailTop))
    assertEquals(keyEventFor(app, detail, Key.Esc), Some(Event.McpBack))

  // -- update flow ---------------------------------------------------------------

  test("selection moves, clamps, opens the selected server, and is restored on back"):
    val app = appUI
    val (s1, _) = app.update(Event.McpListDown, withServers())
    assertEquals(s1.overlay, Overlay.McpServers(1))
    val (s2, _) = app.update(Event.McpListDown, s1)
    val (s3, _) = app.update(Event.McpListDown, s2) // clamped at the last server
    assertEquals(s3.overlay, Overlay.McpServers(2))
    val (s4, _) = app.update(Event.McpOpen, s3)
    assertEquals(s4.overlay, Overlay.McpServerDetail("broken", 0))
    val (s5, _) = app.update(Event.McpBack, s4)
    assertEquals(s5.overlay, Overlay.McpServers(2)) // restored by name, not by stale index

  test("detail scroll floors at the top; g re-pins it"):
    val app = appUI
    val (s1, _) = app.update(Event.McpDetailScroll(-3), withServers(Overlay.McpServerDetail("everything", 0)))
    assertEquals(s1.overlay, Overlay.McpServerDetail("everything", 0))
    val (s2, _) = app.update(Event.McpDetailScroll(5), s1)
    assertEquals(s2.overlay, Overlay.McpServerDetail("everything", 5))
    val (s3, _) = app.update(Event.McpDetailTop, s2)
    assertEquals(s3.overlay, Overlay.McpServerDetail("everything", 0))

  test("an McpUpdated snapshot replaces the server list wholesale"):
    val app = appUI
    val (s1, _) = app.update(Event.Inbound1(AgentEvent.McpUpdated(Vector(ready))), ChatState.initial)
    assertEquals(s1.mcpServers, Vector(ready))
    val (s2, _) = app.update(Event.Inbound1(AgentEvent.McpUpdated(Vector(ready, failed))), s1)
    assertEquals(s2.mcpServers.map(_.name), Vector("everything", "broken"))

  // -- rendering ------------------------------------------------------------------

  test("the list shows one card per server: status, name, digest, command, error"):
    val lines = fullscreenLines(appUI, withServers())
    assert(lines.head.contains("MCP servers · 3"), lines.head)
    assert(lines.head.contains("1 ready · 1 connecting · 1 failed · 2 tools"), lines.head)
    assert(lines.exists(l => l.contains("› ● everything") && l.contains("ready · 2 tools · v0.6.2")), lines.mkString("\n"))
    assert(lines.exists(_.contains("npx -y @modelcontextprotocol/server-everything")))
    assert(lines.exists(l => l.contains("◌ slowpoke") && l.contains("connecting…")))
    assert(lines.exists(_.contains("✗ MCP connect to 'broken' failed: spawn ENOENT")))
    assert(lines.last.contains("↑/↓ select  Enter details  Esc close"), lines.last)

  test("with nothing configured, the list explains how to declare a server"):
    val lines = fullscreenLines(appUI, ChatState.initial.copy(overlay = Overlay.McpServers(0)))
    assert(lines.exists(_.contains("No MCP servers configured")), lines.mkString("\n"))
    assert(lines.exists(_.contains("[mcp.servers.everything]")))
    assert(lines.exists(_.contains("command = npx")))

  test("the detail page shows the fact sheet and the wrapped tool list"):
    val lines = fullscreenLines(appUI, withServers(Overlay.McpServerDetail("everything", 0)))
    assert(lines.head.contains("MCP · everything"), lines.head)
    assert(lines.head.contains("ready"), lines.head)
    assert(lines.exists(l => l.contains("command") && l.contains("npx -y @modelcontextprotocol/server-everything")))
    assert(lines.exists(l => l.contains("env") && l.contains("FOO")))
    assert(lines.exists(l => l.contains("version") && l.contains("0.6.2")))
    assert(lines.exists(l => l.contains("protocol") && l.contains("2025-06-18")))
    assert(lines.exists(_.contains("▸ tools · 2")))
    assert(lines.exists(l => l.contains("echo") && l.contains("Echoes back whatever message you send")))

  test("a failed server's detail carries its error; a vanished name degrades"):
    val lines = fullscreenLines(appUI, withServers(Overlay.McpServerDetail("broken", 0)))
    assert(lines.exists(l => l.contains("error") && l.contains("spawn ENOENT")), lines.mkString("\n"))
    val gone = fullscreenLines(appUI, withServers(Overlay.McpServerDetail("ghost", 0)))
    assert(gone.exists(_.contains("This MCP server is gone")))

  test("the detail page clamps a runaway offset against the content length"):
    // A huge stored offset (scrolled far past the end) still renders the tail
    // rather than a blank page — the clamp lives at render.
    val lines = fullscreenLines(appUI, withServers(Overlay.McpServerDetail("everything", 999)), rows = 8)
    assert(lines.exists(l => l.contains("add") && l.contains("Adds two numbers")), lines.mkString("\n"))
