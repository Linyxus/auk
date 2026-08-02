package auk.tui

import gears.async.{Async, ReadableChannel, UnboundedChannel}
import auk.tui.app.{Handshake, Key, Runtime, RuntimeConfig, WidthProbe}
import auk.tui.render.{HeadlessTerminal, Terminal}
import auk.platform.js.{NodeTerminal, TtyGuard}
import auk.agent.{AgentEvent, UserCommand, Inbox}

/** How the chat transcript occupies the terminal.
  *
  *   - [[DisplayMode.Fullscreen]]: the alternate screen buffer with an in-app
  *     scrollable viewport and native mouse-wheel scrolling.
  *   - [[DisplayMode.Inline]]: today's hybrid model — finalized entries printed
  *     into the terminal's native scrollback, only the live region cell-diffed.
  *     Mouse reporting is never enabled inline (native scroll/selection stay).
  */
enum DisplayMode:
  case Fullscreen, Inline

object DisplayMode:
  /** Fullscreen is the product default; `--inline` anywhere in argv opts out.
    * Argv is scanned rather than indexed because the executable/script prefix
    * varies across node/Bun/SEA. */
  def fromArgv(argv: List[String]): DisplayMode =
    if argv.contains("--inline") then Inline else Fullscreen

/** A terminal UI for auk, driven entirely by two channels.
  *
  * The seam between the agent and its frontend: the UI consumes a stream of
  * agent events and produces user commands. Any frontend that honors this contract
  * can stand in for another. [[run]] blocks the calling thread until the user
  * quits.
  *
  * `requestDashboard` is the one thing that does not fit the channel shape: a UI
  * key that has to ask the host to START the browser dashboard, whose answer comes
  * back down the event stream like everything else. Defaulted to a no-op, so a
  * frontend with no such key — or a host with no dashboard — ignores it.
  */
trait Tui:
  def run(
      events: ReadableChannel[AgentEvent],
      commands: UnboundedChannel[UserCommand],
      interrupts: UnboundedChannel[Unit],
      inbox: UnboundedChannel[Inbox],
      modelName: String = "",
      contextWindow: Int = 0,
      provider: String = "",
      modelId: String = "",
      baseUrl: String = "",
      mode: DisplayMode = DisplayMode.Fullscreen,
      requestDashboard: () => Unit = () => (),
      // The active provider had no API key at startup (session runs on a stub
      // endpoint), and — stronger — no provider at all had one, in which case
      // the UI should open straight onto its key-entry flow.
      keyless: Boolean = false,
      onboardLogin: Boolean = false
  )(using Async.Spawn): Unit

/** The default TUI: a streaming chat transcript on auk's own rendering library.
  *
  * A thin factory — it sets up the terminal and hands the channels to a
  * [[ChatApp]] running on the gears-based [[Runtime]], keeping [[Tui]] purely
  * about the channel contract.
  */
object ChatTui extends Tui:
  override def run(
      events: ReadableChannel[AgentEvent],
      commands: UnboundedChannel[UserCommand],
      interrupts: UnboundedChannel[Unit],
      inbox: UnboundedChannel[Inbox],
      modelName: String = "",
      contextWindow: Int = 0,
      provider: String = "",
      modelId: String = "",
      baseUrl: String = "",
      mode: DisplayMode = DisplayMode.Fullscreen,
      requestDashboard: () => Unit = () => (),
      keyless: Boolean = false,
      onboardLogin: Boolean = false
  )(using Async.Spawn): Unit =
    // Real terminal when we have a TTY; a headless stub otherwise (piped/CI).
    // Only a real terminal gets the width probe: it answers CPR, and its widths
    // are the ones that matter; a headless stub would just stall the first
    // frame until the probe timeout.
    val (terminal: Terminal, probe: Option[Handshake]) = NodeTerminal.create() match
      case Some(t) =>
        // The renderer owns the tty from here on: route every other writer
        // (console.*, direct stderr, Node warnings) to a log so stray output
        // can never smear the screen between diff frames — and so the log
        // names the culprit if something does try.
        TtyGuard.install(".auk/tty.log")
        (t, Some(WidthProbe()))
      case None => (HeadlessTerminal, None)
    // Render at ~60fps; Ctrl+Q still provides a direct quit shortcut. run()
    // blocks until the user quits.
    Runtime.run(
      ChatApp(
        events,
        commands,
        interrupts,
        inbox,
        modelName = modelName,
        contextWindow = contextWindow,
        provider = provider,
        modelId = modelId,
        baseUrl = baseUrl,
        mode = mode,
        // A completed drag-selection is copied to the system clipboard via the
        // terminal (OSC 52); headless/inline terminals no-op.
        copyToClipboard = terminal.copyToClipboard,
        requestDashboard = requestDashboard,
        keyless = keyless,
        onboardLogin = onboardLogin
      ),
      terminal,
      // The single place requirement "no mouse reporting inline" is enforced:
      // mouse reporting is on exactly when the transcript owns the alt screen.
      RuntimeConfig(
        frameMs = 16,
        quitKey = Key.Ctrl('Q'),
        enableMouse = mode == DisplayMode.Fullscreen,
        handshake = probe
      )
    )
