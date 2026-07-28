package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import auk.tui.render.{Color, Style, StyledLine, Width}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, UserCommand, Inbox}
import auk.llm.endpoint.{ChatResponse, FinishReason, Message, StreamEvent, Usage}
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}
import auk.workflow.{Forest, RunStatus}
import auk.llm.tools.Json
import auk.tui.markdown.render.MarkdownRender

class ChatAppViewSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())

  /** An app paired with the inbox channel it sends conversation input to, for
    * asserting on what a Submit queues. */
  private def appWithInbox: (ChatApp, UnboundedChannel[Inbox]) =
    val inbox = UnboundedChannel[Inbox]()
    val app = ChatApp(UnboundedChannel[AgentEvent]().asReadable, UnboundedChannel[UserCommand](), UnboundedChannel[Unit](), inbox)
    (app, inbox)

  private def fireAndReadInbox(cmd: Cmd[Event], inbox: UnboundedChannel[Inbox]): Inbox =
    cmd match
      case Cmd.Fire(effect) => effect()
      case other            => fail(s"expected Cmd.Fire, got $other")
    inbox.asReadable.readSource.poll() match
      case Some(Right(item)) => item
      case Some(Left(err))   => fail(s"inbox channel closed: $err")
      case None              => fail("no inbox item was sent")

  private def fireAndRead(cmd: Cmd[Event], commands: UnboundedChannel[UserCommand]): UserCommand =
    cmd match
      case Cmd.Fire(effect) => effect()
      case other            => fail(s"expected Cmd.Fire, got $other")
    // The fire effect sends synchronously to the unbounded channel, so a
    // non-blocking poll retrieves it without an Async context.
    commands.asReadable.readSource.poll() match
      case Some(Right(command)) => command
      case Some(Left(err))      => fail(s"command channel closed: $err")
      case None                 => fail("no command was sent")

  private def assertCompactCommand(command: UserCommand): Long =
    command match
      case UserCommand.CompactContext(requestedAtMs) => requestedAtMs
      case other                                     => fail(s"expected CompactContext, got $other")

  private def plainLines(state: ChatState, width: Int = 60): (Vector[String], Vector[String]) =
    val screen = appUI.view(state, Viewport(width, 30))
    val committed = screen.committed.flatMap(Layout.lay(_, width)).map(_.plain)
    val live = Layout.lay(screen.live, width).map(_.plain)
    (committed, live)

  private def panelLines(state: ChatState, width: Int = 60): Vector[String] =
    panelLinesFor(appUI, state, width)

  private def panelLinesFor(app: ChatApp, state: ChatState, width: Int = 60): Vector[String] =
    val lines = Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)
    val start = lines.indexWhere(_.startsWith("┌"))
    if start < 0 then Vector.empty
    else
      val tail = lines.drop(start)
      val end = tail.indexWhere(_.startsWith("└"))
      if end < 0 then tail else tail.take(end + 1)

  private def keyEvent(state: ChatState, key: Key): Option[Event] =
    keyEventFor(appUI, state, key)

  private def keyEventFor(app: ChatApp, state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil

    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  private def hasTimer(sub: Sub[Event]): Boolean =
    sub match
      case Sub.Batch(ss)         => ss.exists(hasTimer)
      case Sub.TimeEveryMs(_, _) => true
      case _                     => false

  test("initial view: header is committed; prompt and footer are live") {
    val (committed, live) = plainLines(ChatState.initial)
    assert(committed.take(2).forall(_.isEmpty), committed.mkString("|"))
    assert(committed.exists(_.contains("Auk")), committed.mkString("|"))
    assert(committed.exists(_.contains(s"v${auk.generated.BuildInfo.version}")), committed.mkString("|"))
    val cwd = auk.platform.Platform.cwd()
    val workdir = auk.platform.Platform.env.get("HOME") match
      case Some(home) if home.nonEmpty && cwd.startsWith(home) => s"~${cwd.drop(home.length)}"
      case _                                                   => cwd
    assert(committed.exists(_.contains(workdir)), committed.mkString("|"))
    assert(live.exists(_.contains("›")), "prompt arrow missing")
    assert(live.exists(_.contains("ctrl+c or / for commands")), "ctrl+c footer hint missing")
  }

  test("context compaction marker hides the compacted summary"):
    val summary = "## Current Goal\nkeep this private"
    val (committed, _) = plainLines(ChatState.initial.copy(history = Vector(Entry.ContextCompacted(summary))))
    assert(committed.exists(_.contains("Context Compacted")), committed.mkString("|"))
    assert(!committed.exists(_.contains("Current Goal")), committed.mkString("|"))
    assert(!committed.exists(_.contains("keep this private")), committed.mkString("|"))

  test("Ctrl-C opens key bindings; command keys dispatch; other keys dismiss"):
    val open = ChatState.initial.showKeyBindings
    assertEquals(keyEvent(ChatState.initial, Key.Ctrl('C')), Some(Event.ShowKeyBindings))
    assertEquals(keyEvent(open, Key.Char('c')), Some(Event.RunCommand("c")))
    assertEquals(keyEvent(open, Key.Char('C')), Some(Event.RunCommand("c")))
    assertEquals(keyEvent(open, Key.Char('q')), Some(Event.RunCommand("q")))
    assertEquals(keyEvent(open, Key.Char('r')), Some(Event.RunCommand("r")))
    assertEquals(keyEvent(open, Key.Char('n')), Some(Event.RunCommand("n")))
    assertEquals(keyEvent(open, Key.Char('x')), Some(Event.HideOverlay))
    assertEquals(keyEvent(open, Key.Esc), Some(Event.HideOverlay))

  test("Ctrl-C raises the which-key strip from the very bottom of the live region"):
    val screen = appUI.view(ChatState.initial.showKeyBindings, Viewport(60, 30))
    assert(screen.overlay.isEmpty)
    val live = Layout.lay(screen.live, 60).map(_.plain)
    // No framed panel anywhere — the menu is a full-bleed strip, not a box.
    assert(panelLines(ChatState.initial.showKeyBindings).isEmpty)
    // The strip: a dim `ctrl+c` label row, then the key grid, at the very end
    // of the live region — below the prompt and the footer.
    val label = live.indexWhere(_.trim == "ctrl+c")
    assert(label > 0, live.mkString("|"))
    assert(label > live.indexWhere(_.contains("›")), live.mkString("|"))
    assert(label > live.indexWhere(_.contains("ctrl+c or / for commands")), live.mkString("|"))
    // A divider separates the strip from the chrome above it.
    assert(live(label - 1).startsWith("──"), live.mkString("|"))
    val grid = live.drop(label + 1)
    assert(grid.nonEmpty && grid.forall(_.length == 60), grid.mkString("|"))
    assert(grid.exists(l => l.contains("c,q") && l.contains("exit")), grid.mkString("|"))
    assert(grid.exists(l => l.contains("r") && l.contains("resume session")), grid.mkString("|"))
    assert(grid.exists(l => l.contains("n") && l.contains("new session")), grid.mkString("|"))
    assert(grid.exists(l => l.contains("k") && l.contains("interrupt")), grid.mkString("|"))
    // Entries flow into more than one column at this width.
    assert(grid.exists(l => l.contains("exit") && l.contains("resume session")), grid.mkString("|"))

  test("the which-key strip dims entries the current phase makes inert"):
    // The disabled tone is the strip's one use of indexed color 243, so its SGR
    // is a reliable fingerprint in the rendered (styled) lines.
    val DimSeq = ";38;5;243"
    def rendered(state: ChatState): Vector[String] =
      Layout.lay(appUI.view(state, Viewport(60, 30)).live, 60).map(_.render)
    // Idle: interrupt would be a no-op — its row (alone on the grid's last line)
    // is dimmed; the idle-only commands are live.
    val idle = rendered(ChatState.initial.showKeyBindings)
    val idleInterrupt = idle.filter(_.contains("interrupt"))
    assert(idleInterrupt.nonEmpty && idleInterrupt.forall(_.contains(DimSeq)), idle.mkString("|"))
    val idleResume = idle.filter(_.contains("resume session"))
    assert(idleResume.nonEmpty && idleResume.forall(!_.contains(DimSeq)), idle.mkString("|"))
    // Mid-turn: the gates flip — interrupt is live, the idle-only ones recede.
    val busy = rendered(ChatState.initial.copy(phase = Phase.Waiting).showKeyBindings)
    assert(busy.filter(_.contains("interrupt")).forall(!_.contains(DimSeq)), busy.mkString("|"))
    assert(busy.exists(l => l.contains("resume session") && l.contains(DimSeq)), busy.mkString("|"))

  test("Ctrl-C b opens the debug panel; Esc dismisses it"):
    val open = ChatState.initial.showKeyBindings
    assertEquals(keyEvent(open, Key.Char('b')), Some(Event.RunCommand("b")))
    val (next, cmd) = appUI.update(Event.RunCommand("b"), open)
    assertEquals(next.overlay, Overlay.DebugInfo)
    assertEquals(cmd, Cmd.none)
    assertEquals(keyEvent(next, Key.Esc), Some(Event.HideOverlay))
    // Read-only: a stray key neither dispatches nor leaks into the prompt.
    assertEquals(keyEvent(next, Key.Char('x')), None)

  test("debug panel renders the live model, provider, endpoint, and context usage"):
    val state = ChatState.initial
      .copy(
        modelName = "GLM 5.2",
        modelId = "glm-5.2",
        provider = "ZAI",
        baseUrl = "https://api.z.ai/api/anthropic",
        contextWindow = 1_000_000,
        contextTokens = 12345
      )
      .submitted("hello")
      .showDebugInfo
    val overlay = panelLines(state)
    assert(overlay.head.startsWith("┌"), overlay.mkString("|"))
    assert(overlay.last.startsWith("└"), overlay.mkString("|"))
    assert(overlay.exists(_.contains("Debug info")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("Model") && line.contains("GLM 5.2")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("Model ID") && line.contains("glm-5.2")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("Provider") && line.contains("ZAI")), overlay.mkString("|"))
    assert(overlay.exists(_.contains("https://api.z.ai/api/anthropic")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("Used") && line.contains("12,345") && line.contains("1%")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("Messages") && line.contains("1")), overlay.mkString("|"))
    assert(overlay.exists(_.contains("Esc to close")), overlay.mkString("|"))
    // Every framed row shares one width, so the box stays rectangular.
    assert(overlay.map(_.length).distinct.size == 1, overlay.mkString("|"))

  test("ctrl+c l (and /repaint) returns a refresh command and closes the overlay"):
    val (next, cmd) = appUI.update(Event.RunCommand("l"), ChatState.initial.showKeyBindings)
    assertEquals(next.overlay, Overlay.None)
    cmd match
      case Cmd.Refresh => ()
      case other       => fail(s"expected Cmd.Refresh, got $other")

  test("command exit returns a quit command and closes the overlay"):
    val (next, cmd) = appUI.update(Event.RunCommand("c"), ChatState.initial.showKeyBindings)
    assertEquals(next.overlay, Overlay.None)
    cmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit, got $other")

    val (nextFromAlias, aliasCmd) = appUI.update(Event.RunCommand("q"), ChatState.initial.showKeyBindings)
    assertEquals(nextFromAlias.overlay, Overlay.None)
    aliasCmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit from alias, got $other")

  test("resume and new-session commands send engine commands while idle"):
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())

    val (resumeState, resumeCmd) = app.update(Event.RunCommand("r"), ChatState.initial.showKeyBindings)
    assertEquals(resumeState.overlay, Overlay.ResumeLoading("Loading sessions"))
    assertEquals(fireAndRead(resumeCmd, commands), UserCommand.ListSessions)

    val (newState, newCmd) = app.update(Event.RunCommand("n"), ChatState.initial.showKeyBindings)
    assertEquals(newState.overlay, Overlay.ResumeLoading("Starting new session"))
    assertEquals(fireAndRead(newCmd, commands), UserCommand.NewSession)

  test("slash compact sends a compaction command while idle"):
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())

    val state = ChatState.initial.copy(input = "/compact", cursor = 8, overlay = Overlay.SlashPalette(0))
    val (next, cmd) = app.update(Event.SlashSelected, state)
    assertEquals(next.overlay, Overlay.None)
    assert(assertCompactCommand(fireAndRead(cmd, commands)) > 0L)

  test("Ctrl-C compact appears in the command menu and sends a compaction command"):
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())
    val state = ChatState.initial.showKeyBindings
    val strip = Layout.lay(app.view(state, Viewport(60, 30)).live, 60).map(_.plain)
    assert(strip.exists(l => l.contains("p") && l.contains("compact context")), strip.mkString("|"))
    assertEquals(keyEventFor(app, state, Key.Char('p')), Some(Event.RunCommand("p")))

    val (next, cmd) = app.update(Event.RunCommand("p"), state)
    assertEquals(next.overlay, Overlay.None)
    assert(assertCompactCommand(fireAndRead(cmd, commands)) > 0L)

  test("compact command enters a visible compacting phase when there is transcript history"):
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())
    val state = ChatState.initial.copy(history = Vector(Entry.User("question"))).showKeyBindings
    val (next, cmd) = app.update(Event.RunCommand("p"), state)
    assertEquals(next.phase, Phase.Compacting)
    assert(assertCompactCommand(fireAndRead(cmd, commands)) > 0L)
    val (_, live) = plainLines(next.copy(clockMs = next.turnStartMs + 1200))
    assert(live.exists(_.contains("Compacting context…")), live.mkString("|"))
    assert(live.exists(_.contains("compacting context")), live.mkString("|"))
    assert(!live.exists(_.contains("ctrl+c k to interrupt")), live.mkString("|"))

  test("compact command is ignored while compaction is already running"):
    val busy = ChatState.initial.copy(phase = Phase.Compacting, input = "/compact", cursor = 8, overlay = Overlay.SlashPalette(0))
    val (slashNext, slashCmd) = appUI.update(Event.SlashSelected, busy)
    assertEquals(slashNext.overlay, Overlay.None)
    assertEquals(slashCmd, Cmd.none)

    val keyed = ChatState.initial.copy(phase = Phase.Compacting).showKeyBindings
    val (keyNext, keyCmd) = appUI.update(Event.RunCommand("p"), keyed)
    assertEquals(keyNext.overlay, Overlay.None)
    assertEquals(keyCmd, Cmd.none)

    val (interruptNext, interruptCmd) = appUI.update(Event.RunCommand("k"), keyed)
    assertEquals(interruptNext.overlay, Overlay.None)
    assertEquals(interruptCmd, Cmd.none)

  test("slash compact is idle-only"):
    val busy = ChatState.initial.copy(phase = Phase.Waiting, input = "/compact", cursor = 8, overlay = Overlay.SlashPalette(0))
    val (next, cmd) = appUI.update(Event.SlashSelected, busy)
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("resume and new-session commands are idle-only"):
    val busy = ChatState.initial.showKeyBindings.copy(phase = Phase.Waiting)
    val (next, cmd) = appUI.update(Event.RunCommand("r"), busy)
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("key command registry drives dispatch and overlay rows"):
    val customApp =
      val events = UnboundedChannel[AgentEvent]()
      val commands = UnboundedChannel[UserCommand]()
      ChatApp(
        events.asReadable,
        commands,
        UnboundedChannel[Unit](),
        UnboundedChannel[Inbox](),
        keyCommands = Vector(ChatApp.Command(Vector("m", "n"), "mock command")(state => (state.copy(input = "ran"), Cmd.none)))
      )
    val state = ChatState.initial.showKeyBindings
    val strip = Layout.lay(customApp.view(state, Viewport(60, 30)).live, 60).map(_.plain)
    assert(strip.exists(l => l.contains("m,n") && l.contains("mock command")), strip.mkString("|"))
    assert(!strip.exists(l => l.contains("c,q") && l.contains("exit")), strip.mkString("|"))

    val event = keyEventFor(customApp, state, Key.Char('m'))
    assertEquals(event, Some(Event.RunCommand("m")))
    assertEquals(keyEventFor(customApp, state, Key.Char('n')), Some(Event.RunCommand("n")))
    val (next, cmd) = customApp.update(Event.RunCommand("m"), state)
    assertEquals(next.input, "ran")
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("session list event opens an elegant resume picker"):
    val sessions = List(
      SessionSummary("abcdef123456", Some(System.currentTimeMillis()), 3, "continue this work")
    )
    val (next, _) = appUI.update(Event.Inbound1(AgentEvent.SessionsListed(sessions)), ChatState.initial)
    val overlay = panelLines(next, width = 90)
    assert(overlay.head.startsWith("┌"), overlay.mkString("|"))
    assert(overlay.last.startsWith("└"), overlay.mkString("|"))
    assert(overlay.exists(_.contains("Resume session")), overlay.mkString("|"))
    assert(overlay.exists(_.contains("abcdef12")), overlay.mkString("|"))
    assert(overlay.exists(_.contains("continue this work")), overlay.mkString("|"))
    assert(overlay.exists(_.contains("Enter resume")), overlay.mkString("|"))
    assert(overlay.map(_.length).distinct.size == 1, overlay.mkString("|"))

  test("resume picker handles arrows, enter, escape, and empty lists"):
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox]())
    val sessions = Vector(
      SessionSummary("a-session", None, 1, "first"),
      SessionSummary("b-session", None, 2, "second")
    )
    val picker = ChatState.initial.showSessionPicker(sessions)

    assertEquals(keyEventFor(app, picker, Key.Down), Some(Event.SessionPickerDown))
    assertEquals(keyEventFor(app, picker, Key.Up), Some(Event.SessionPickerUp))
    assertEquals(keyEventFor(app, picker, Key.Enter), Some(Event.ResumeSelected))
    assertEquals(keyEventFor(app, picker, Key.Esc), Some(Event.HideOverlay))

    val selected = picker.moveSessionSelection(1)
    val (loading, cmd) = app.update(Event.ResumeSelected, selected)
    assertEquals(loading.overlay, Overlay.ResumeLoading("Opening session"))
    assertEquals(fireAndRead(cmd, commands), UserCommand.ResumeSession("b-session"))

    val empty = ChatState.initial.showSessionPicker(Vector.empty)
    val emptyOverlay = panelLinesFor(app, empty, 90)
    assert(emptyOverlay.exists(_.contains("No saved sessions yet")), emptyOverlay.mkString("|"))

    val many = (1 to 10).map(i => SessionSummary(f"session-$i%02d", None, i, s"item $i")).toVector
    val scrolled = ChatState.initial.showSessionPicker(many).moveSessionSelection(9)
    val scrolledOverlay = panelLinesFor(app, scrolled, 90)
    assert(scrolledOverlay.exists(_.contains("item 10")), scrolledOverlay.mkString("|"))
    assert(scrolledOverlay.exists(_.contains("3-10 of 10")), scrolledOverlay.mkString("|"))

  test("session switched event replaces the visible transcript"):
    val events = List(
      SessionEvent.UserSubmitted("previous question"),
      SessionEvent.AssistantResponded(ChatResponse(Message.assistant("previous answer"), FinishReason.Stop))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val state = ChatState.initial.copy(input = "draft", cursor = 5).showResumeLoading("Opening session")
    val (next, _) = appUI.update(Event.Inbound1(AgentEvent.SessionSwitched(snapshot)), state)
    assertEquals(next.history, Vector(Entry.User("previous question"), Entry.Assistant(Vector(Block.shownAnswer("previous answer")))))
    assertEquals(next.input, "")
    assertEquals(next.overlay, Overlay.None)
    assertEquals(appUI.view(next, Viewport(60, 30)).committedEpoch, 1L)

  test("a submitted message commits a user entry; the hint disappears") {
    val state = ChatState.initial.submitted("hello there").copy(phase = Phase.Waiting)
    val (committed, live) = plainLines(state)
    // The committed message keeps the box it was typed in — same frame and arrow
    // as the input prompt (only the arrow's colour differs), no "You" header.
    val boxed = committed.indexWhere(_.startsWith("│ › hello there"))
    assert(boxed > 0, committed.mkString("|"))
    assert(committed(boxed - 1).startsWith("╭─"), committed.mkString("|"))
    assert(committed(boxed + 1).startsWith("╰─"), committed.mkString("|"))
    assert(!committed.exists(_.trim == "You"), committed.mkString("|"))
    assert(!live.exists(_.contains("Type a message")), "hint should be gone once history is non-empty")
    // Waiting phase shows the spinner label in the live region.
    assert(live.exists(_.contains("Working…")), live.mkString("|"))
  }

  test("the input box frame expands to the layout width") {
    val (_, live) = plainLines(ChatState.initial, width = 30)
    assert(live.exists(l => l == "╭" + "─" * 28 + "╮"), live.mkString("|"))
    assert(live.exists(l => l == "╰" + "─" * 28 + "╯"), live.mkString("|"))
  }

  test("input prompt wraps inside the frame width") {
    val input = "abcdefghijklmnopqrstuvwxyz"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 12)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    val promptRows = live.drop(start).takeWhile(_.startsWith("│"))
    val content = promptRows.map(_.stripPrefix("│ ").stripSuffix(" │").drop(2)).mkString

    assert(promptRows.length > 1, promptRows.mkString("|"))
    assert(promptRows.forall(line => Width.stringWidth(line) == 12), promptRows.mkString("|"))
    assert(promptRows.head.startsWith("│ › "), promptRows.head)
    assert(promptRows.tail.forall(_.startsWith("│   ")), promptRows.mkString("|"))
    assert(content.startsWith(input), content)
  }

  test("input prompt renders explicit newlines") {
    val input = "alpha\nbeta"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 40)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    val promptRows = live.drop(start).takeWhile(_.startsWith("│"))
    val content = promptRows.map(_.stripPrefix("│ ").stripSuffix(" │").stripTrailing())

    assertEquals(content, Vector("› alpha", "  beta"))
  }

  test("newline event inserts a line break into the draft") {
    val (next, _) = appUI.update(Event.Newline, ChatState.initial.copy(input = "ab", cursor = 1))
    assertEquals(next.input, "a\nb")
    assertEquals(next.cursor, 2)
  }

  test("a streaming answer shows the breathing cursor at its tail in the live region") {
    val streaming = ChatState.initial
      .submitted("q")
      .copy(phase = Phase.Waiting)
      .appendReply("hello world", now = 1000)
    // Reveal a few characters, as the tick loop would.
    val revealed = Iterator.iterate(streaming)(_.advanceReveal).drop(3).next()
    val (_, live) = plainLines(revealed)
    assert(live.exists(_.contains("_")), s"cursor missing from live region: ${live.mkString("|")}")
    assert(live.exists(_.contains("h")), live.mkString("|"))
  }

  test("the working indicator stays through streaming, just above the input") {
    val streaming = ChatState.initial
      .submitted("q")
      .copy(phase = Phase.Waiting)
      .appendReply("hello world", now = 1000)
    val revealed = Iterator.iterate(streaming)(_.advanceReveal).drop(3).next()
    val (_, live) = plainLines(revealed)
    val spinnerIdx = live.indexWhere(_.contains("Working…"))
    val promptIdx = live.indexWhere(_.contains("›"))
    assert(spinnerIdx >= 0, s"spinner missing: ${live.mkString("|")}")
    assert(promptIdx >= 0, s"prompt missing: ${live.mkString("|")}")
    // The indicator sits below the streaming answer and above the input prompt.
    assert(spinnerIdx < promptIdx, s"spinner($spinnerIdx) should precede prompt($promptIdx)")
  }

  test("the working indicator shows elapsed time, estimated tokens, and throughput") {
    // 40 streamed chars ≈ 10 tokens at 4 chars/token; 2s elapsed ⇒ 5 token/s.
    val streaming = ChatState.initial
      .submitted("q")
      .copy(phase = Phase.Waiting, turnStartMs = 1000, clockMs = 3000)
      .appendReply("a" * 40, now = 3000)
    val (_, live) = plainLines(streaming)
    val line = live.find(_.contains("Working…")).getOrElse("")
    assert(line.contains("2.0s"), s"elapsed missing: $line")
    assert(line.contains("10 tokens"), s"token estimate missing: $line")
    assert(line.contains("5 token/s"), s"throughput missing: $line")
  }

  test("the working indicator anchors tokens to exact per-round usage, estimating only the open round") {
    // Round 1: 100 chars of reasoning, then 30 exact output tokens. Round 2: a
    // 40-char answer in flight ⇒ ≈10 estimated, so 40 tokens total at 2s ⇒ 20/s.
    val streaming = ChatState.initial
      .submitted("q")
      .startingTurn(now = 1000)
      .appendThinking("x" * 100, now = 1000)
      .anchorRoundUsage(Usage(inputTokens = 500, outputTokens = 30))
      .appendReply("y" * 40, now = 2000)
      .copy(clockMs = 3000)
    val (_, live) = plainLines(streaming)
    val line = live.find(_.contains("Working…")).getOrElse("")
    assert(line.contains("40 tokens"), s"hybrid token tally missing: $line")
    assert(line.contains("20 token/s"), s"throughput missing: $line")
  }

  test("typing is allowed while a reply is streaming") {
    val busy = ChatState.initial.copy(phase = Phase.Waiting, input = "dra", cursor = 3)
    val (next, _) = appUI.update(Event.KeyChar('f'), busy)
    assertEquals(next.input, "draf")
    assertEquals(next.cursor, 4)

  }

  test("history navigation works while a reply is streaming (input box is phase-agnostic)") {
    // Regression: Up/Down recalled history only when idle, so history nav did
    // nothing while Auk was working. The input box must behave identically in
    // every phase; only Submit is gated on idle.
    val busy = ChatState.initial
      .submitted("first")
      .submitted("second")
      .copy(phase = Phase.Waiting) // a turn is in flight, draft empty on the boundary line
    val (up, _) = appUI.update(Event.HistoryPrev, busy)
    assertEquals(up.input, "second")
    val (up2, _) = appUI.update(Event.HistoryPrev, up)
    assertEquals(up2.input, "first")
    val (down, _) = appUI.update(Event.HistoryNext, up2)
    assertEquals(down.input, "second")
  }

  test("the input is editable in the live region while streaming (no ellipsis)") {
    val busy = ChatState.initial.copy(phase = Phase.Waiting, input = "my draft", cursor = 8)
    val (_, live) = plainLines(busy)
    // The draft sits in the input box verbatim: no ellipsis anywhere on that row,
    // which would mean it had been truncated to a read-only placeholder. Other
    // live rows may carry one — the working line reads "Working…".
    val promptRow = live.find(_.contains("my draft")).getOrElse("")
    assert(promptRow.startsWith("│ › "), live.mkString("|"))
    assert(!promptRow.contains("…"), live.mkString("|"))
  }

  test("Enter while a reply is streaming queues the line on the inbox and clears the input") {
    val (app, inbox) = appWithInbox
    val busy = ChatState.initial.copy(phase = Phase.Waiting, input = "follow up", cursor = 9)
    val (next, cmd) = app.update(Event.Submit, busy)
    // Input clears; the transcript is NOT touched (the engine echoes it back); phase unchanged.
    assertEquals(next.input, "")
    assertEquals(next.history, Vector.empty)
    assertEquals(next.phase, Phase.Waiting)
    // It also records input-history for ↑/↓ recall.
    assertEquals(next.inputHistory, Vector("follow up"))
    assertEquals(fireAndReadInbox(cmd, inbox), Inbox.UserMessage("follow up"))
  }

  test("Enter while idle also queues on the inbox (the engine starts the turn)") {
    val (app, inbox) = appWithInbox
    val (next, cmd) = app.update(Event.Submit, ChatState.initial.copy(input = "hello", cursor = 5))
    assertEquals(next.input, "")
    // No optimistic transcript entry — the engine's InputsConsumed echo renders it.
    assertEquals(next.history, Vector.empty)
    assertEquals(fireAndReadInbox(cmd, inbox), Inbox.UserMessage("hello"))
  }

  test("InputQueued appends to the panel; InputsConsumed drops the FIFO prefix and shows items inline") {
    val s0 = ChatState.initial.copy(phase = Phase.Streaming(Vector.empty))
    val s1 = s0
      .inputQueued(Inbox.UserMessage("a"))
      .inputQueued(Inbox.SystemNotice("b"))
      .inputQueued(Inbox.UserMessage("c"))
    assertEquals(s1.pendingQueue, Vector(Inbox.UserMessage("a"), Inbox.SystemNotice("b"), Inbox.UserMessage("c")))
    val s2 = s1.inputsConsumed(List(Inbox.UserMessage("a"), Inbox.SystemNotice("b")))
    assertEquals(s2.pendingQueue, Vector(Inbox.UserMessage("c")))
    s2.phase match
      case Phase.Streaming(blocks, _) =>
        assertEquals(blocks, Vector(Block.Injected(Inbox.UserMessage("a")), Block.Injected(Inbox.SystemNotice("b"))))
      case other => fail(s"expected streaming, got $other")
  }

  test("a turn-start InputsConsumed leads the turn as transcript entries and enters Waiting") {
    val consumed = ChatState.initial.inputsConsumed(List(Inbox.UserMessage("go"), Inbox.SystemNotice("fyi")))
    assertEquals(consumed.history, Vector(Entry.User("go"), Entry.System("fyi")))
    assertEquals(consumed.phase, Phase.Waiting)
    assertEquals(consumed.pendingQueue, Vector.empty)
  }

  test("the queued panel renders a count and both kinds above the input; empty queue shows no panel") {
    val streaming = ChatState.initial.copy(phase = Phase.Streaming(Vector.empty))
      .inputQueued(Inbox.UserMessage("refactor the parser"))
      .inputQueued(Inbox.SystemNotice("context is 82% full"))
    val (_, live) = plainLines(streaming)
    assert(live.exists(_.contains("queued · 2")), live.mkString("|"))
    assert(live.exists(_.contains("refactor the parser")), live.mkString("|"))
    assert(live.exists(_.contains("context is 82% full")), live.mkString("|"))

    val (_, emptyLive) = plainLines(ChatState.initial.copy(phase = Phase.Streaming(Vector.empty)))
    assert(!emptyLive.exists(_.contains("queued ·")), emptyLive.mkString("|"))
  }

  test("a long queued message stays on one line, ellipsis-truncated inside the rail") {
    val long = (Vector.fill(40)("word").mkString(" "))
    val streaming = ChatState.initial.copy(phase = Phase.Streaming(Vector.empty)).inputQueued(Inbox.UserMessage(long))
    val (_, live) = plainLines(streaming, width = 40)
    val rows = live.filter(l => l.contains("│") && l.contains("word"))
    assertEquals(rows.length, 1, live.mkString("\n"))
    assert(rows.head.contains("…"), rows.head)
    assert(rows.head.length <= 40, rows.head)
  }

  test("a finalized assistant turn is committed, not live") {
    val finished = ChatState.initial
      .submitted("q")
      .copy(phase = Phase.Waiting)
      .appendReply("the answer", now = 1000)
      .finishReply("the answer", now = 2000)
    // Drain the reveal to completion, as the tick loop would, then commit.
    val streamed = Iterator.iterate(finished)(_.advanceReveal).find(_.revealSettled).get.commitIfDrained
    val (committed, live) = plainLines(streamed)
    assert(committed.exists(_.contains("the answer")), committed.mkString("|"))
    // once committed, the live region no longer carries the answer
    assert(!live.exists(_.contains("the answer")), live.mkString("|"))
  }

  private def sampleChoices: Vector[ModelChoice] = Vector(
    ModelChoice("OpenRouter", "openrouter", "z-ai/glm-5.1", "GLM 5.1", 202752),
    ModelChoice("Anthropic", "anthropic", "claude-opus-4-8", "Claude Opus 4.8", 1000000)
  )

  private def appWithChoices(choices: Vector[ModelChoice]): (ChatApp, UnboundedChannel[UserCommand]) =
    val commands = UnboundedChannel[UserCommand]()
    val app = ChatApp(UnboundedChannel[AgentEvent]().asReadable, commands, UnboundedChannel[Unit](), UnboundedChannel[Inbox](), modelChoices = choices)
    (app, commands)

  test("the m command opens the model picker") {
    val (app, _) = appWithChoices(sampleChoices)
    val opened = ChatState.initial.showKeyBindings
    assertEquals(keyEventFor(app, opened, Key.Char('m')), Some(Event.RunCommand("m")))
    val (next, _) = app.update(Event.RunCommand("m"), opened)
    assert(next.overlay.isInstanceOf[Overlay.ModelPicker], next.overlay.toString)
  }

  test("model picker lists every model under a 'Switch model' title") {
    val (app, _) = appWithChoices(sampleChoices)
    val panel = panelLinesFor(app, ChatState.initial.showModelPicker(sampleChoices), 90)
    assert(panel(1).contains("Switch model"), panel.mkString("|"))
    assert(panel.exists(_.contains("GLM 5.1")), panel.mkString("|"))
    assert(panel.exists(_.contains("z-ai/glm-5.1")), panel.mkString("|"))
    assert(panel.exists(_.contains("Claude Opus 4.8")), panel.mkString("|"))
    assert(panel.exists(_.contains("Enter switch")), panel.mkString("|"))
  }

  test("model picker has a column header and stays aligned with overlong fields") {
    val long = Vector(
      ModelChoice("OpenRouter", "openrouter", "vendor/an-extremely-long-model-identifier-that-overflows",
        "An Extremely Long Display Name", 200000),
      ModelChoice("Anthropic", "anthropic", "claude-opus-4-8", "Claude Opus 4.8", 1000000)
    )
    val (app, _) = appWithChoices(long)
    val panel = panelLinesFor(app, ChatState.initial.showModelPicker(long), 90)

    val header = panel.find(l => l.contains("Model") && l.contains("Provider") && l.contains("Model id") && l.contains("Context"))
    assert(header.isDefined, panel.mkString("\n"))
    // Every framed row is the same width (the panel is a clean rectangle).
    assertEquals(panel.map(_.length).distinct.size, 1, panel.mkString("\n"))
    // The overflowing fields are ellipsis-truncated rather than pushing columns.
    assert(panel.exists(_.contains("…")), panel.mkString("\n"))
    // The provider column starts at the same offset in the header and every row.
    val col = header.get.indexOf("Provider")
    val longRow = panel.find(_.contains("…")).get
    val claudeRow = panel.find(_.contains("Claude Opus 4.8")).get
    assert(longRow.substring(col).startsWith("OpenRouter"), longRow)
    assert(claudeRow.substring(col).startsWith("Anthropic"), claudeRow)
  }

  test("model picker handles arrows, enter, and escape") {
    val (app, _) = appWithChoices(sampleChoices)
    val picker = ChatState.initial.showModelPicker(sampleChoices)
    assertEquals(keyEventFor(app, picker, Key.Down), Some(Event.ModelPickerDown))
    assertEquals(keyEventFor(app, picker, Key.Up), Some(Event.ModelPickerUp))
    assertEquals(keyEventFor(app, picker, Key.Enter), Some(Event.ModelSelected))
    assertEquals(keyEventFor(app, picker, Key.Esc), Some(Event.HideOverlay))
  }

  test("model picker consumes typed search text and backspace") {
    val (app, _) = appWithChoices(sampleChoices)
    val picker = ChatState.initial.showModelPicker(sampleChoices)
    assertEquals(keyEventFor(app, picker, Key.Char('g')), Some(Event.ModelPickerSearchChar('g')))
    assertEquals(keyEventFor(app, picker, Key.Backspace), Some(Event.ModelPickerSearchBackspace))

    val (typed, _) = app.update(Event.ModelPickerSearchChar('g'), picker)
    assertEquals(typed.input, "")
    typed.overlay match
      case Overlay.ModelPicker(_, query, _) => assertEquals(query, "g")
      case other                           => fail(s"expected model picker, got $other")

    val (backspaced, _) = app.update(Event.ModelPickerSearchBackspace, typed)
    backspaced.overlay match
      case Overlay.ModelPicker(_, query, _) => assertEquals(query, "")
      case other                           => fail(s"expected model picker, got $other")
  }

  test("model picker search flexibly matches provider, label, and id") {
    val choices = sampleChoices :+ ModelChoice(
      "OpenRouter",
      "openrouter",
      "deepseek/deepseek-v4-flash",
      "DeepSeek V4 Flash",
      1048576
    )
    val (app, _) = appWithChoices(choices)
    val searched = "open flash".foldLeft(ChatState.initial.showModelPicker(choices)) { (state, c) =>
      app.update(Event.ModelPickerSearchChar(c), state)._1
    }

    assertEquals(searched.selectedModel.map(_.modelId), Some("deepseek/deepseek-v4-flash"))
    val panel = panelLinesFor(app, searched, 100)
    assert(panel.exists(_.contains("Search: open flash")), panel.mkString("|"))
    assert(panel.exists(_.contains("DeepSeek V4 Flash")), panel.mkString("|"))
    assert(!panel.exists(_.contains("Claude Opus 4.8")), panel.mkString("|"))

    val compact = "opus48".foldLeft(ChatState.initial.showModelPicker(choices)) { (state, c) =>
      app.update(Event.ModelPickerSearchChar(c), state)._1
    }
    assertEquals(compact.selectedModel.map(_.modelId), Some("claude-opus-4-8"))
  }

  test("moveModelSelection clamps and selectedModel tracks the cursor") {
    val picker = ChatState.initial.showModelPicker(sampleChoices)
    assertEquals(picker.selectedModel.map(_.modelId), Some("z-ai/glm-5.1"))
    assertEquals(picker.moveModelSelection(1).selectedModel.map(_.modelId), Some("claude-opus-4-8"))
    assertEquals(picker.moveModelSelection(5).selectedModel.map(_.modelId), Some("claude-opus-4-8"))
    assertEquals(picker.moveModelSelection(-5).selectedModel.map(_.modelId), Some("z-ai/glm-5.1"))
  }

  test("selecting a model closes the picker and asks the engine to switch") {
    val (app, commands) = appWithChoices(sampleChoices)
    val picker = ChatState.initial.showModelPicker(sampleChoices).moveModelSelection(1)
    val (next, cmd) = app.update(Event.ModelSelected, picker)
    assertEquals(next.overlay, Overlay.None)
    assertEquals(fireAndRead(cmd, commands), UserCommand.SwitchModel("anthropic", "claude-opus-4-8"))
  }

  test("selecting after a model search switches the filtered choice") {
    val choices = sampleChoices :+ ModelChoice(
      "OpenRouter",
      "openrouter",
      "deepseek/deepseek-v4-flash",
      "DeepSeek V4 Flash",
      1048576
    )
    val (app, commands) = appWithChoices(choices)
    val searched = "deep flash".foldLeft(ChatState.initial.showModelPicker(choices)) { (state, c) =>
      app.update(Event.ModelPickerSearchChar(c), state)._1
    }
    val (next, cmd) = app.update(Event.ModelSelected, searched)
    assertEquals(next.overlay, Overlay.None)
    assertEquals(fireAndRead(cmd, commands), UserCommand.SwitchModel("openrouter", "deepseek/deepseek-v4-flash"))
  }

  test("a ModelSwitched event updates the footer") {
    val (app, _) = appWithChoices(sampleChoices)
    val (ok, _) = app.update(
      Event.Inbound1(AgentEvent.ModelSwitched("GLM 5.1", 200_000, "ZAI", "glm-5.1", "https://api.z.ai/api/anthropic")),
      ChatState.initial
    )
    assertEquals(ok.modelName, "GLM 5.1")
    assertEquals(ok.contextWindow, 200_000)
    assertEquals(ok.provider, "ZAI")
    assertEquals(ok.modelId, "glm-5.1")
    assertEquals(ok.baseUrl, "https://api.z.ai/api/anthropic")
    val live = Layout.lay(app.view(ok, Viewport(80, 30)).live, 80).map(_.plain)
    assert(live.exists(_.contains("GLM 5.1")), live.mkString("|"))
  }

  /* ---- quiet-block merging (settled reasoning + finished eval_scala) ---- */

  private def evalTool(
      rawArgs: String,
      output: Option[String] = None,
      isError: Boolean = false,
      elapsedMs: Option[Long] = Some(0L)
  ): Block.Tool =
    Block.Tool("e1", "eval_scala", rawArgs, elapsedMs = elapsedMs, output = output, isError = isError)

  private def committedWith(block: Block): Vector[String] =
    committedBlocks(block)

  private def committedBlocks(blocks: Block*): Vector[String] =
    plainLines(ChatState.initial.copy(history = Vector(Entry.Assistant(blocks.toVector))))._1

  test("a run of quiet blocks folds to one summary line; code and reasoning are hidden"):
    val lines = committedBlocks(
      Block.Thinking(Typewriter.shown("secret reasoning"), 0L, Some(10000L)),
      evalTool("""{"code":"val a = 111"}""", output = Some("out_aaa")),
      evalTool("""{"code":"val b = 222"}""", output = Some("out_bbb")),
      evalTool("""{"code":"val c = 333"}""", output = Some("out_ccc"))
    )
    // Exactly one merged summary line, and nothing of the old card survives.
    val summaries = lines.filter(_.contains("✻"))
    assertEquals(summaries.length, 1, lines.mkString("|"))
    assert(summaries.head.contains("✻ Thought for 10.0s, executed 3 code snippets"), lines.mkString("|"))
    assert(!lines.exists(_.contains("val a = 111")), lines.mkString("|"))
    assert(!lines.exists(_.contains("out_aaa")), lines.mkString("|"))
    assert(!lines.exists(_.contains("secret reasoning")), lines.mkString("|"))
    assert(!lines.exists(_.contains("╭─ execution")), lines.mkString("|"))

  test("a lone finished eval folds to 'Executed a code snippet', hiding its code and output"):
    val lines = committedBlocks(evalTool("""{"code":"1 + 1"}""", output = Some("val res0: Int = 2\n")))
    assert(lines.exists(_.contains("✻ Executed a code snippet")), lines.mkString("|"))
    assert(!lines.exists(_.contains("1 + 1")), lines.mkString("|"))
    assert(!lines.exists(_.contains("val res0")), lines.mkString("|"))

  test("failed evals are tallied in the summary"):
    val lines = committedBlocks(
      evalTool("""{"code":"ok"}""", output = Some("fine")),
      evalTool("""{"code":"boom"}""", output = Some("error"), isError = true)
    )
    assert(lines.exists(_.contains("✻ Executed 2 code snippets (1 failed)")), lines.mkString("|"))

  private def mcpTool(
      name: String = "mcp__linear__create_issue",
      rawArgs: String = """{"title":"t"}""",
      output: Option[String] = Some("ok"),
      isError: Boolean = false,
      elapsedMs: Option[Long] = Some(0L)
  ): Block.Tool =
    Block.Tool("m1", name, rawArgs, elapsedMs = elapsedMs, output = output, isError = isError)

  test("a running MCP call shows the dotted name and a one-line argument digest"):
    val state = ChatState.initial.copy(
      phase = Phase.Streaming(Vector(
        Block.Tool(
          "m1",
          "mcp__linear__create_issue",
          """{"title":"Fix crash on empty config","teamId":"ENG"}""",
          startedMs = Some(1000L),
          elapsedMs = None
        )
      )),
      clockMs = 2200L
    )
    val live = plainLines(state, 100)._2
    val row = live.find(_.contains("Calling")).getOrElse(fail(live.mkString("|")))
    assert(row.contains("Calling linear.create_issue"), row)
    assert(row.contains("""{title: "Fix crash on empty config""""), row)
    assert(row.contains("1.2s"), row)
    // The wire name never reaches the screen.
    assert(!live.exists(_.contains("mcp__linear")), live.mkString("|"))

  test("finished MCP calls fold into the quiet summary alongside thinking and evals"):
    val lines = committedBlocks(
      Block.Thinking(Typewriter.shown("reasoning"), 0L, Some(3000L)),
      evalTool("""{"code":"val a = 1"}""", output = Some("out")),
      evalTool("""{"code":"val b = 2"}""", output = Some("out")),
      mcpTool(),
      mcpTool(name = "list_mcp_resources", rawArgs = "{}"),
      mcpTool(name = "read_mcp_resource", rawArgs = """{"uri":"file:///x"}""", isError = true)
    )
    val summaries = lines.filter(_.contains("✻"))
    assertEquals(summaries.length, 1, lines.mkString("|"))
    assert(
      summaries.head.contains("✻ Thought for 3.0s, executed 2 code snippets, called 3 tools (1 failed)"),
      summaries.head
    )

  test("a lone finished MCP call folds to 'Called a tool'"):
    val lines = committedBlocks(mcpTool())
    assert(lines.exists(_.contains("✻ Called a tool")), lines.mkString("|"))
    assert(!lines.exists(_.contains("linear.create_issue")), lines.mkString("|"))

  test("a tool with no special-cased label shows its name and an argument digest"):
    // Nothing is known about an arbitrary tool's argument shape, so it gets the
    // same budgeted one-line digest an MCP call gets, rather than only its name.
    val lines = committedBlocks(
      Block.Tool("g1", "grep", """{"pattern":"NodeProcess","glob":"*.scala"}""", elapsedMs = Some(0L))
    )
    assert(lines.exists(_.contains("""grep {pattern: "NodeProcess", glob: "*.scala"}""")), lines.mkString("|"))

  test("an unparseable argument string leaves just the tool name"):
    // The digest is only ever the parsed rendering — raw JSON never reaches a label.
    val lines = committedBlocks(Block.Tool("g1", "grep", "NodeProcess", elapsedMs = Some(0L)))
    assert(lines.exists(_.trim.endsWith("grep")), lines.mkString("|"))
    assert(!lines.exists(_.contains("NodeProcess")), lines.mkString("|"))

  test("the digest never outgrows the label budget"):
    val long = "x" * 400
    val lines = committedBlocks(Block.Tool("g1", "grep", s"""{"pattern":"$long"}""", elapsedMs = Some(0L)))
    val label = lines.find(_.contains("grep")).getOrElse("")
    assert(label.contains("…}"), label)
    assert(label.length < 120, label)

  test("the path-labelled tools keep their own phrasing, digest-free"):
    // read/edit/write are special-cased ahead of the fallback and must not change.
    val lines = committedBlocks(
      Block.Tool("t1", "read", """{"path":"foo.scala"}""", elapsedMs = Some(0L)),
      Block.Tool("t2", "write", """{"path":"bar.scala","content":"x"}""", elapsedMs = Some(0L))
    )
    assert(lines.exists(_.contains("Reading foo.scala")), lines.mkString("|"))
    assert(lines.exists(_.contains("Writing bar.scala")), lines.mkString("|"))
    assert(!lines.exists(_.contains("{path:")), lines.mkString("|"))

  test("a tool outside the MCP family stays visible instead of folding"):
    // submit_result and friends keep the fallback rendering and their own line.
    val lines = committedBlocks(
      Block.Tool("s1", "submit_result", """{"result":1}""", elapsedMs = Some(0L), output = Some("ok"))
    )
    assert(!lines.exists(_.contains("✻")), lines.mkString("|"))
    assert(lines.exists(_.contains("submit_result")), lines.mkString("|"))

  test("a running MCP call is visible and splits the fold around it"):
    val state = ChatState.initial.copy(
      phase = Phase.Streaming(Vector(
        evalTool("""{"code":"1"}""", output = Some("out")),
        mcpTool(output = None, elapsedMs = None).copy(startedMs = Some(0L)),
        mcpTool()
      )),
      clockMs = 500L
    )
    val live = plainLines(state, 100)._2
    val firstAt = live.indexWhere(_.contains("✻ Executed a code snippet"))
    val callAt = live.indexWhere(_.contains("Calling linear.create_issue"))
    val lastAt = live.indexWhere(_.contains("✻ Called a tool"))
    assert(firstAt >= 0 && callAt >= 0 && lastAt >= 0, live.mkString("|"))
    assert(firstAt < callAt && callAt < lastAt, live.mkString("|"))

  test("a resumed session's MCP calls fold too, replayed from persisted args"):
    // Replay reconstructs Block.Tool from the persisted name + input, so the fold
    // must survive the round trip through the session log, not just live blocks.
    import auk.llm.endpoint.{Content, Role}
    val events = List(
      SessionEvent.UserSubmitted("go"),
      SessionEvent.AssistantResponded(ChatResponse(
        Message(Role.Assistant, List(
          Content.ToolUse("m1", "mcp__linear__create_issue", """{"title":"Fix crash"}"""),
          Content.ToolUse("m2", "mcp__linear__list_issues", """{"teamId":"ENG"}""")
        )),
        FinishReason.ToolUse,
        None
      )),
      SessionEvent.ToolResultsReceived(List(
        Content.ToolResult("m1", "created ENG-1"),
        Content.ToolResult("m2", "3 issues")
      ))
    )
    val lines = plainLines(ChatState.initial.copy(history = ChatState.historyFrom(events)))._1
    assert(lines.exists(_.contains("✻ Called 2 tools")), lines.mkString("|"))
    assert(!lines.exists(_.contains("mcp__linear")), lines.mkString("|"))

  test("a visible tool splits a quiet run into two summaries around it"):
    val lines = committedBlocks(
      Block.Thinking(Typewriter.shown("t"), 0L, Some(2000L)),
      Block.Tool("t1", "read", """{"path":"foo.scala"}""", elapsedMs = Some(0L)),
      evalTool("""{"code":"1 + 1"}""", output = Some("val res0: Int = 2\n"))
    )
    val thoughtAt = lines.indexWhere(_.contains("✻ Thought for 2.0s"))
    val readAt = lines.indexWhere(_.contains("Reading foo.scala"))
    val evalAt = lines.indexWhere(_.contains("✻ Executed a code snippet"))
    assert(thoughtAt >= 0 && readAt >= 0 && evalAt >= 0, lines.mkString("|"))
    assert(thoughtAt < readAt && readAt < evalAt, lines.mkString("|"))

  test("a lone committed thinking block keeps the byte-identical thought label"):
    val lines = committedBlocks(Block.Thinking(Typewriter.shown("private reasoning"), 0L, Some(2500L)))
    assert(lines.exists(_.contains("✻ Thought for 2.5s")), lines.mkString("|"))
    assert(!lines.exists(_.contains("private reasoning")), lines.mkString("|"))

  test("live reasoning shows a sliding window of the last four wrapped lines"):
    // 60 distinct fixed-width words wrap to more than four rows at width 60.
    val words = (1 to 60).map(i => f"w$i%02d").mkString(" ")
    val state = ChatState.initial.copy(
      phase = Phase.Streaming(Vector(Block.Thinking(Typewriter.shown(words), 0L, None)))
    )
    val live = plainLines(state, 60)._2
    val headerAt = live.indexWhere(_.contains("thinking ▸"))
    assert(headerAt >= 0, live.mkString("|"))
    // The window body: the barred rows after the header (up to four of them).
    val body = live.drop(headerAt + 1).takeWhile(_.contains("│"))
    assert(body.length <= 4, body.mkString("|"))
    assert(body.exists(_.contains("w60")), body.mkString("|"))   // the newest text
    assert(!body.exists(_.contains("w01")), body.mkString("|"))  // the earliest scrolled off

  test("skill tool calls read as sentences, not JSON digests"):
    val saveArgs = Json.Obj(List(
      "id" -> Json.Str("Greeter"),
      "description" -> Json.Str("greets people"),
      "code" -> Json.Str("object Greeter { def greet(n: String): String = n }"),
      "tests" -> Json.Arr(List(Json.Str("assert(Greeter.greet(\"x\") == \"x\")")))
    )).render
    val saved = committedWith(
      Block.Tool("s1", "skill_save", saveArgs, elapsedMs = Some(8200L), output = Some("Skill 'Greeter' saved. …"))
    )
    assert(saved.exists(_.contains("Saving skill Greeter (greets people)")), saved.mkString("|"))
    assert(!saved.exists(_.contains("\"id\"")), saved.mkString("|"))
    // The same call whose result reports an existing id reads "Updating".
    val updated = committedWith(
      Block.Tool("s1", "skill_save", saveArgs, elapsedMs = Some(8200L),
        output = Some("Skill 'Greeter' updated. The whole set…"))
    )
    assert(updated.exists(_.contains("Updating skill Greeter (greets people)")), updated.mkString("|"))
    val removed = committedWith(
      Block.Tool("s2", "skill_remove", """{"id":"Greeter"}""", elapsedMs = Some(3000L), output = Some("Skill 'Greeter' removed…"))
    )
    assert(removed.exists(_.contains("Removing skill Greeter")), removed.mkString("|"))
    val reloaded = committedWith(
      Block.Tool("s3", "skill_reload", "{}", elapsedMs = Some(3000L), output = Some("Reloaded 2 skill(s)…"))
    )
    assert(reloaded.exists(_.contains("Reloading skills from disk")), reloaded.mkString("|"))

  test("in the full transcript, skill_save unfolds to code and tests, not JSON"):
    val app = fullscreenApp
    val tool = Block.Tool(
      "s1",
      "skill_save",
      Json.Obj(List(
        "id" -> Json.Str("Greeter"),
        "description" -> Json.Str("greets people"),
        "code" -> Json.Str("object Greeter {\n  def greet(n: String): String = n\n}"),
        "tests" -> Json.Arr(List(Json.Str("assert(Greeter.greet(\"x\") == \"x\")")))
      )).render,
      elapsedMs = Some(8200L),
      output = Some("Skill 'Greeter' saved.")
    )
    val state = ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(tool))))
    val full = fsLines(app, state.showFullTranscript, 80, 30)
    assert(full.exists(_.contains("def greet(n: String): String = n")), full.mkString("|"))
    assert(full.exists(_.contains("// test 1")), full.mkString("|"))
    assert(full.exists(_.contains("assert(Greeter.greet")), full.mkString("|"))
    assert(!full.exists(_.contains("\"code\"")), full.mkString("|"))
    assert(!full.exists(_.contains("\"tests\"")), full.mkString("|"))

  test("a live running eval renders 'Executing code' with a ticking duration, no code"):
    val state = ChatState.initial.copy(
      phase = Phase.Streaming(Vector(
        Block.Tool("e1", "eval_scala", """{"code":"secretCode123"}""", startedMs = Some(1000L), elapsedMs = None)
      )),
      clockMs = 3300L
    )
    val live = plainLines(state, 60)._2
    assert(live.exists(l => l.contains("Executing code") && l.contains("2.3s")), live.mkString("|"))
    assert(!live.exists(_.contains("secretCode123")), live.mkString("|"))

  test("live merging: a settled thought and finished eval fold above the live window"):
    val state = ChatState.initial.copy(
      phase = Phase.Streaming(Vector(
        Block.Thinking(Typewriter.shown("earlier reasoning"), 0L, Some(3000L)),
        evalTool("""{"code":"1 + 1"}""", output = Some("val res0: Int = 2\n")),
        Block.Thinking(Typewriter.shown("live thought text"), 0L, None)
      ))
    )
    val live = plainLines(state, 60)._2
    val summaryAt = live.indexWhere(_.contains("✻ Thought for 3.0s, executed a code snippet"))
    val windowAt = live.indexWhere(_.contains("thinking ▸"))
    assert(summaryAt >= 0, live.mkString("|"))
    assert(windowAt >= 0 && summaryAt < windowAt, live.mkString("|"))
    assert(live.exists(_.contains("live thought text")), live.mkString("|"))
    // The folded eval's code and output are gone.
    assert(!live.exists(_.contains("1 + 1")), live.mkString("|"))
    assert(!live.exists(_.contains("val res0")), live.mkString("|"))

  /* ---- committed-history Element memoization ---- */

  private def committedPlain(app: ChatApp, state: ChatState, width: Int): Vector[String] =
    app.view(state, Viewport(width, 30)).committed.flatMap(Layout.lay(_, width)).map(_.plain)

  test("committed cache: warm-cache render is byte-identical and reflows on resize"):
    // A long answer so the laid-out width genuinely differs between 80 and 40.
    val longAnswer =
      "The quick brown fox jumps over the lazy dog and keeps on running far " +
        "beyond the visible edge of the terminal window into the night."
    val history = Vector(
      Entry.User("first question that is also long enough to wrap at narrow widths"),
      Entry.Assistant(Vector(Block.shownAnswer(longAnswer))),
      Entry.User("second question"),
      Entry.Assistant(Vector(Block.shownAnswer("a short reply")))
    )
    val state = ChatState.initial.copy(history = history)

    // Source of truth: cold apps (empty cache) laid directly at each width.
    val cold80 = committedPlain(appUI, state, 80)
    val cold40 = committedPlain(appUI, state, 40)
    assertNotEquals(cold40, cold80, "the answer must wrap differently at 40 vs 80 for this test to mean anything")

    // One app, cache warmed at 80, then queried again at 80 and at 40.
    val warm = appUI
    val first80 = committedPlain(warm, state, 80)
    val second80 = committedPlain(warm, state, 80)
    assertEquals(second80, first80, "a cache hit must reproduce the first render exactly")
    assertEquals(first80, cold80, "warm-cache output must equal a cold render at the same width")

    // The width is NOT in the cache key: cached Elements must reflow at 40.
    val warm40 = committedPlain(warm, state, 40)
    assertEquals(warm40, cold40, "cached Elements must reflow on resize, not serve stale-width lines")

  test("committed cache: appended entries extend the committed prefix"):
    val app = appUI
    val base = Vector(
      Entry.User("q1"),
      Entry.Assistant(Vector(Block.shownAnswer("answer one")))
    )
    committedPlain(app, ChatState.initial.copy(history = base), 60) // warm with 2 entries
    val grown = base ++ Vector(
      Entry.User("q2"),
      Entry.Assistant(Vector(Block.shownAnswer("answer two")))
    )
    val grownState = ChatState.initial.copy(history = grown)
    val out = committedPlain(app, grownState, 60)
    assertEquals(out, committedPlain(appUI, grownState, 60), "tail append must match a cold full render")
    assert(out.exists(_.contains("answer one")) && out.exists(_.contains("answer two")), out.mkString("|"))

  test("committed cache: a transcript epoch bump rebuilds committed entries"):
    val app = appUI
    val first = Vector(
      Entry.User("q1"),
      Entry.Assistant(Vector(Block.shownAnswer("answer one")))
    )
    val warm = committedPlain(app, ChatState.initial.copy(history = first), 60)
    assert(warm.exists(_.contains("answer one")), warm.mkString("|"))

    // A session switch bumps transcriptEpoch and may shrink the history; the
    // cache must rebuild rather than serve entries from the previous epoch.
    val switched = ChatState.initial.copy(
      history = Vector(Entry.User("fresh"), Entry.Assistant(Vector(Block.shownAnswer("fresh answer")))),
      transcriptEpoch = 1
    )
    val rebuilt = committedPlain(app, switched, 60)
    assertEquals(rebuilt, committedPlain(appUI, switched, 60), "epoch rebuild must match a cold render")
    assert(rebuilt.exists(_.contains("fresh answer")), rebuilt.mkString("|"))
    assert(!rebuilt.exists(_.contains("answer one")), "stale entries from the previous epoch leaked")

  test("a RoundComplete event refreshes the context gauge, not only the terminal Done"):
    // The fold applies each round's usage to the gauge, so it tracks occupancy
    // round-by-round through a long agentic turn and stays truthful when a turn is
    // interrupted — no Done ever arrives then, but the last round reported usage.
    val state = ChatState.initial.copy(contextWindow = 200_000)
    val event = Event.Inbound1(AgentEvent.Stream(Right(StreamEvent.RoundComplete(Usage(60_000, 2_000)))))
    val (updated, _) = appUI.update(event, state)
    assertEquals(updated.contextTokens, 62_000L)
    assertEquals(updated.contextPercentUsed, Some(31))

  /* ---- Fullscreen chat (DisplayMode.Fullscreen) ---- */

  private def fullscreenApp: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      mode = DisplayMode.Fullscreen
    )

  /** The fullscreen frame laid to plain-text rows at `(width, rows)`. */
  private def fsLines(app: ChatApp, state: ChatState, width: Int, rows: Int): Vector[String] =
    val el = app.view(state, Viewport(width, rows)).fullscreen.getOrElse(fail("expected a fullscreen element"))
    Layout.lay(el, width).map(_.plain)

  /** A transcript of `n` short user/assistant rounds. */
  private def rounds(n: Int): Vector[Entry] =
    (1 to n).flatMap(i => Vector(Entry.User(s"question $i"), Entry.Assistant(Vector(Block.shownAnswer(s"answer $i"))))).toVector

  /** One round: a marked user question and a many-paragraph assistant answer, so
    * a scrolled viewport can sit inside the answer with the user line above. */
  private def longRound(marker: String, paragraphs: Int): Vector[Entry] =
    Vector(
      Entry.User(s"$marker the important question"),
      Entry.Assistant(Vector(Block.shownAnswer((1 to paragraphs).map(i => s"answer paragraph $i").mkString("\n\n"))))
    )

  test("fullscreen chat: the frame is exactly viewport.rows lines; the inline chat has no fullscreen element"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(3))
    val screen = app.view(state, Viewport(60, 24))
    assert(screen.fullscreen.isDefined)
    assertEquals(Layout.lay(screen.fullscreen.get, 60).length, 24)
    // The inline app (no workflow overlay) produces no fullscreen element.
    assertEquals(appUI.view(state, Viewport(60, 24)).fullscreen, None)

  test("fullscreen chat: the frame is exactly viewport.rows lines at every scroll anchor"):
    // Sweeps the anchor across the whole transcript (including exact user-line
    // boundaries, where reserving the sticky row can otherwise flip the count).
    val app = fullscreenApp
    val base = ChatState.initial.copy(history = longRound("MARK", 40) ++ rounds(6))
    for top <- 0 to 400 by 1 do
      val lines = fsLines(app, base.copy(chatScroll = Some(top)), 60, 18)
      assertEquals(lines.length, 18, s"anchor $top produced ${lines.length} lines")
    // Follow mode too.
    assertEquals(fsLines(app, base, 60, 18).length, 18)

  test("fullscreen chat follow-tail: the newest transcript line sits above the input box"):
    val app = fullscreenApp
    val lines = fsLines(app, ChatState.initial.copy(history = rounds(40)), 60, 20)
    assertEquals(lines.length, 20)
    val ans40 = lines.indexWhere(_.contains("answer 40"))
    // The input prompt is the last `›` line (a sticky round header can carry one too).
    val arrow = lines.lastIndexWhere(_.contains("›"))
    assert(ans40 >= 0 && arrow >= 0 && ans40 < arrow, lines.mkString("|"))

  test("fullscreen chat detached: a scroll anchor shows an earlier slice with a range footer"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40), chatScroll = Some(0))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    // The detached footer REPLACES the keyboard hints with the scroll range and
    // the re-follow hint (the only actionable thing while scrolled off the tail).
    val footerLine = lines.find(_.contains("↕")).getOrElse(fail("no scroll-range footer"))
    assert(footerLine.contains("of") && footerLine.contains("scroll to bottom to follow"), footerLine)
    assert(!footerLine.contains("ctrl+"), s"detached footer must drop the ctrl hints: $footerLine")
    // Follow mode keeps the keyboard hints (only the detached state replaces them).
    val followLines = fsLines(app, state.copy(chatScroll = None), 60, 20)
    assert(followLines.exists(_.contains("ctrl+")), "follow-mode footer must keep the ctrl hints")
    assert(!followLines.exists(_.contains("↕")), "follow-mode footer has no scroll range")

  test("fullscreen chat detached: a huge scroll anchor clamps to the tail"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40), chatScroll = Some(100000))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    assert(lines.exists(_.contains("answer 40")), lines.mkString("|"))

  test("a committed user box carries one blank line above it"):
    val (committed, _) = plainLines(ChatState.initial.copy(history = rounds(2)))
    val boxed = committed.indexWhere(_.contains("› question 2"))
    assert(boxed >= 3, committed.mkString("|"))
    assert(committed(boxed - 1).startsWith("╭─"), committed.mkString("|"))
    assert(committed(boxed - 2).trim.isEmpty, committed.mkString("|"))
    assert(committed(boxed - 3).contains("answer 1"), committed.mkString("|"))

  test("fullscreen chat idle: a blank separator row keeps the transcript off the input box"):
    val app = fullscreenApp
    val lines = fsLines(app, ChatState.initial.copy(history = rounds(40)), 60, 20)
    assertEquals(lines.length, 20)
    val boxTop = lines.lastIndexWhere(_.startsWith("╭"))
    assert(boxTop > 1, lines.mkString("|"))
    assertEquals(lines(boxTop - 1).trim, "")
    assert(lines(boxTop - 2).contains("answer 40"), lines.mkString("|"))

  test("fullscreen chat working at the tail: the working line sits flush above the input box"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(3), phase = Phase.Waiting)
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    val boxTop = lines.lastIndexWhere(_.startsWith("╭"))
    assert(lines(boxTop - 1).contains("Working…"), lines.mkString("|"))

  test("fullscreen chat scrolled: a centered ↓ marker counts the lines below the viewport"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40), chatScroll = Some(0))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    val boxTop = lines.lastIndexWhere(_.startsWith("╭"))
    val sep = lines(boxTop - 1)
    assert(sep.contains("↓") && sep.contains("more"), lines.mkString("|"))
    // The marker's count agrees with the footer's `↕ a-b of n` range: everything
    // after the visible window is below the viewport.
    val nums = raw"\d+".r
    val footer = lines.find(_.contains("↕")).getOrElse(fail("no range footer"))
    val footerNums = nums.findAllIn(footer).toVector.map(_.toInt)
    val below = nums.findAllIn(sep).toVector.head.toInt
    assertEquals(below, footerNums(2) - footerNums(1))
    // The marker sits mid-row, not at the left edge.
    assert(sep.takeWhile(_ == ' ').length > 10, sep)

  test("fullscreen chat scrolled while working: the ↓ marker still separates body and input box"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40), phase = Phase.Waiting, chatScroll = Some(0))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    val boxTop = lines.lastIndexWhere(_.startsWith("╭"))
    assert(lines(boxTop - 1).contains("↓"), lines.mkString("|"))

  test("inline: the blank above the input box is omitted while the working line is there"):
    val (_, live) = plainLines(ChatState.initial.submitted("q").copy(phase = Phase.Waiting))
    val boxTop = live.lastIndexWhere(_.startsWith("╭"))
    assert(live(boxTop - 1).contains("Working…"), live.mkString("|"))
    // Idle keeps the separating blank.
    val (_, idleLive) = plainLines(ChatState.initial.copy(history = rounds(1)))
    val idleBox = idleLive.lastIndexWhere(_.startsWith("╭"))
    assertEquals(idleLive(idleBox - 1).trim, "")

  test("fullscreen chat scroll: wheel detaches from the tail, re-follows at the bottom, and floors at 0"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40))
    app.view(state, Viewport(60, 20)) // populate the scroll snapshot (maxTop, bodyHeight)
    // A wheel-up detaches from the tail.
    val (up, _) = app.update(Event.ChatScroll(-3), state)
    up.chatScroll match
      case Some(t) => assert(t >= 0, s"expected a floored anchor, got $t")
      case None    => fail("wheel-up from follow should detach")
    // Wheeling back down onto the tail re-enters follow mode.
    val (down, _) = app.update(Event.ChatScroll(3), up)
    assertEquals(down.chatScroll, None)
    // A large wheel-up floors the absolute top at 0.
    val (floored, _) = app.update(Event.ChatScroll(-100000), state)
    assertEquals(floored.chatScroll, Some(0))
    // ChatFollow always re-pins to the tail.
    assertEquals(app.update(Event.ChatFollow, state.copy(chatScroll = Some(4)))._1.chatScroll, None)

  test("fullscreen chat page scroll steps by nearly a full page (bodyHeight - 1)"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40))
    app.view(state, Viewport(60, 20))
    val (page, _) = app.update(Event.ChatScrollPage(-1), state)
    val (wheel, _) = app.update(Event.ChatScroll(-3), state)
    (page.chatScroll, wheel.chatScroll) match
      case (Some(p), Some(w)) => assert(p < w, s"a page ($p) should scroll further than a 3-line wheel notch ($w)")
      case other              => fail(s"expected detached anchors, got $other")

  test("switchedTo resets the fullscreen chat scroll to follow"):
    val events = List(SessionEvent.UserSubmitted("q"))
    val snapshot = SessionSnapshot(SessionSummary.from("s", None, events), events)
    assertEquals(ChatState.initial.copy(chatScroll = Some(5)).switchedTo(snapshot).chatScroll, None)

  test("fullscreen chat sticky header: the round's user line is pinned when scrolled off the top"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = longRound("STICKYMARK", 60), chatScroll = Some(40))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    assert(lines.head.contains("›") && lines.head.contains("STICKYMARK"), lines.head)

  test("fullscreen chat sticky header: absent at the very top of a round"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = longRound("STICKYMARK", 60), chatScroll = Some(0))
    val lines = fsLines(app, state, 60, 20)
    assertEquals(lines.length, 20)
    assert(!lines.head.trim.startsWith("› STICKYMARK"), lines.head)

  test("fullscreen chat sticky header: shown while following a long streaming answer"):
    val app = fullscreenApp
    val streaming = ChatState.initial.copy(
      history = Vector(Entry.User("STREAMMARK the running question")),
      phase = Phase.Streaming(Vector(Block.shownAnswer((1 to 80).map(i => s"streaming line $i").mkString("\n\n"))))
    )
    val lines = fsLines(app, streaming, 60, 20)
    assertEquals(lines.length, 20)
    assert(lines.head.contains("›") && lines.head.contains("STREAMMARK"), lines.head)

  test("fullscreen chat: the logo banner shines with the render clock"):
    val app = fullscreenApp
    def styled(clock: Long): Vector[String] =
      val el = app.view(ChatState.initial.copy(clockMs = clock), Viewport(60, 24)).fullscreen
        .getOrElse(fail("expected a fullscreen element"))
      Layout.lay(el, 60).map(_.render)
    // At 750 ms the shine band is mid-sweep across the logo, so the frame's
    // colours differ from the resting frame — while the visible text does not.
    assertNotEquals(styled(0L), styled(750L))
    assertEquals(
      fsLines(app, ChatState.initial.copy(clockMs = 0L), 60, 24),
      fsLines(app, ChatState.initial.copy(clockMs = 750L), 60, 24)
    )

  test("idle ticks only while the logo banner is on screen"):
    val app = fullscreenApp
    // A fresh fullscreen session: the banner is at the top of the viewport, so
    // the idle screen keeps a clock for the logo's shine.
    assert(hasTimer(app.subscriptions(ChatState.initial)), "welcome screen should tick for the shine")
    // A long transcript in follow mode scrolls the banner away; after that
    // render, the idle screen is a static frame again.
    val long = ChatState.initial.copy(history = rounds(40))
    app.view(long, Viewport(60, 20))
    assert(!hasTimer(app.subscriptions(long)), "idle with the banner off screen must not tick")
    // The inline app prints its header into native scrollback (it cannot
    // animate), so an idle inline screen never ticks.
    assert(!hasTimer(appUI.subscriptions(ChatState.initial)), "inline idle must stay static")

  test("idle ticks through the overlays that keep the chat backdrop"):
    val app = fullscreenApp
    // The ctrl+c menu is a which-key strip at the frame's bottom edge and the
    // slash palette a popup above the input: the banner stays visible under
    // both, so the shine must keep sweeping.
    assert(hasTimer(app.subscriptions(ChatState.initial.showKeyBindings)), "which-key strip should keep ticking")
    val slash = ChatState.initial.copy(input = "/", cursor = 1).openSlashPalette
    assert(hasTimer(app.subscriptions(slash)), "slash popup should keep ticking")
    // A floating panel gets a deliberately still backdrop.
    assert(!hasTimer(app.subscriptions(ChatState.initial.showDebugInfo)), "modal overlay must stay static")
    // The backdrop exemption is not a licence to tick: with the banner scrolled
    // away there is nothing to animate, overlay or not.
    val long = ChatState.initial.copy(history = rounds(40))
    app.view(long, Viewport(60, 20))
    assert(!hasTimer(app.subscriptions(long.showKeyBindings)), "banner off screen must not tick")

  test("only a running workflow holds the idle clock awake"):
    val app = fullscreenApp
    // Scroll the banner away, so the workflows are the only thing that could ask
    // for a clock.
    val long = ChatState.initial.copy(history = rounds(40))
    app.view(long, Viewport(60, 20))
    // activeWorkflows retains settled runs, but their notice is gone and their
    // glyph is static — nothing to animate.
    val settled = long.copy(activeWorkflows = Vector("r1" -> Forest(status = RunStatus.Done), "r2" -> Forest(status = RunStatus.Failed)))
    assert(!hasTimer(app.subscriptions(settled)), "retained settled runs must not tick")
    val running = settled.copy(activeWorkflows = settled.activeWorkflows :+ ("r3" -> Forest(status = RunStatus.Running)))
    assert(hasTimer(app.subscriptions(running)), "a live run must keep the spinner going")

  /* ---- Full transcript (ctrl+c o) ---- */

  /** An assistant turn the chat folds away whole: eight lines of settled
    * reasoning, an eval whose code and output both overrun the unfolded view's
    * budgets, and the answer that followed. */
  private def foldedTurn: Entry =
    val thinking =
      Block.Thinking(Typewriter.shown((1 to 8).map(i => s"thought line $i").mkString("\n")), 0L, Some(12_000L))
    val eval = Block.Tool(
      "t1",
      "eval_scala",
      Json.Obj(List("code" -> Json.Str((1 to 4).map(i => s"val x$i = $i").mkString("\n")))).render,
      elapsedMs = Some(500L),
      output = Some((1 to 14).map(i => s"out line $i").mkString("\n"))
    )
    Entry.Assistant(Vector(thinking, eval, Block.shownAnswer("the answer")))

  /** `start-end of total` from a fullscreen view's footer. */
  private val RangeRe = """(\d+)-(\d+) of (\d+)""".r

  test("ctrl+c o opens the full transcript, the strip advertises it, and /transcript reaches it"):
    val open = ChatState.initial.showKeyBindings
    assertEquals(keyEvent(open, Key.Char('o')), Some(Event.RunCommand("o")))
    val (opened, cmd) = appUI.update(Event.RunCommand("o"), open)
    assertEquals(opened.overlay, Overlay.FullTranscript(0))
    assertEquals(cmd, Cmd.none)
    val live = Layout.lay(appUI.view(open, Viewport(60, 30)).live, 60).map(_.plain)
    assert(live.exists(l => l.contains("o") && l.contains("full transcript")), live.mkString("|"))
    // The same command answers to /transcript in the slash palette.
    val slash = ChatState.initial.copy(input = "/transcript", cursor = 11, overlay = Overlay.SlashPalette(0))
    val (viaSlash, slashCmd) = appUI.update(Event.SlashSelected, slash)
    assertEquals(viaSlash.overlay, Overlay.FullTranscript(0))
    assertEquals(slashCmd, Cmd.none)

  test("the full transcript unfolds what the chat folds away"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = Vector(Entry.User("hello"), foldedTurn))
    val full = fsLines(app, state.showFullTranscript, 70, 40)
    assertEquals(full.length, 40)
    // Reasoning keeps its last five rows, behind a "…" standing in for the rest.
    assert(full.exists(_.contains("thought line 8")), full.mkString("|"))
    assert(full.exists(_.contains("thought line 4")), full.mkString("|"))
    assert(!full.exists(_.contains("thought line 3")), full.mkString("|"))
    assert(full.exists(_.trim.endsWith("…")), full.mkString("|"))
    // The eval's input is shown whole, as code rather than the JSON carrying it.
    assert((1 to 4).forall(i => full.exists(_.contains(s"val x$i = $i"))), full.mkString("|"))
    assert(!full.exists(_.contains("\"code\"")), full.mkString("|"))
    // Its output keeps the last ten rows, with the clip counted off above them.
    assert(full.exists(_.contains("… +4 lines")), full.mkString("|"))
    assert(full.exists(_.contains("out line 14")), full.mkString("|"))
    assert(full.exists(_.contains("out line 5")), full.mkString("|"))
    assert(!full.exists(_.contains("out line 4")), full.mkString("|"))
    // The call is a framed card — square corners, unlike the rounded message
    // boxes — with the code above a splitter and what it printed below.
    val top = full.indexWhere(_.contains("┌"))
    val splitter = full.indexWhere(_.contains("├"))
    val bottom = full.indexWhere(_.contains("└"))
    assert(top >= 0 && splitter >= 0 && bottom >= 0, full.mkString("|"))
    assert(full(top).contains("┐") && full(splitter).contains("┤") && full(bottom).contains("┘"), full.mkString("|"))
    assert(top < full.indexWhere(_.contains("val x1 = 1")), full.mkString("|"))
    assert(full.indexWhere(_.contains("val x4 = 4")) < splitter, full.mkString("|"))
    assert(splitter < full.indexWhere(_.contains("… +4 lines")), full.mkString("|"))
    assert(full.indexWhere(_.contains("… +4 lines")) < full.indexWhere(_.contains("out line 14")), full.mkString("|"))
    assert(full.indexWhere(_.contains("out line 14")) < bottom, full.mkString("|"))
    // The two sections no longer look alike: code carries the markdown
    // renderer's code colour, output stays dim.
    val styledEl = app.view(state.showFullTranscript, Viewport(70, 40)).fullscreen
      .getOrElse(fail("expected a fullscreen element"))
    val styled = Layout.lay(styledEl, 70).map(_.render)
    assert(styled.find(_.contains("val x1 = 1")).exists(_.contains(MarkdownRender.CodeSeq)), styled.mkString("|"))
    assert(styled.find(_.contains("out line 14")).exists(!_.contains(MarkdownRender.CodeSeq)), styled.mkString("|"))
    // The chat itself is unchanged: one summary line, none of the detail.
    val chat = fsLines(app, state, 70, 40)
    assert(chat.exists(_.contains("Thought for 12.0s, executed a code snippet")), chat.mkString("|"))
    assert(!chat.exists(_.contains("val x1 = 1")), chat.mkString("|"))
    assert(!chat.exists(_.contains("out line 14")), chat.mkString("|"))

  test("a tool card with nothing to show on one side drops that section"):
    val app = fullscreenApp
    def card(output: Option[String]): Vector[String] =
      val tool = Block.Tool("t1", "eval_scala", Json.Obj(List("code" -> Json.Str("val x = 1"))).render,
        elapsedMs = Some(5L), output = output)
      fsLines(app, ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(tool)))).showFullTranscript, 70, 20)
    // Input only: still framed, but nothing to split off.
    val quiet = card(None)
    assert(quiet.exists(_.contains("val x = 1")), quiet.mkString("|"))
    assert(quiet.exists(_.contains("┌")) && quiet.exists(_.contains("└")), quiet.mkString("|"))
    assert(!quiet.exists(_.contains("├")), quiet.mkString("|"))
    // With output, the splitter arrives.
    assert(card(Some("res0: Int = 1")).exists(_.contains("├")), "a two-section card splits")

  test("the full transcript takes scroll keys and nothing else"):
    val app = fullscreenApp
    val open = ChatState.initial.showFullTranscript
    assertEquals(keyEventFor(app, open, Key.Up), Some(Event.FullTranscriptScroll(1)))
    assertEquals(keyEventFor(app, open, Key.Down), Some(Event.FullTranscriptScroll(-1)))
    assertEquals(keyEventFor(app, open, Key.Char('g')), Some(Event.FullTranscriptFollow))
    assertEquals(keyEventFor(app, open, Key.Esc), Some(Event.FullTranscriptBack))
    // Deliberately command-less: no hotkey lands, and Ctrl+C raises no strip.
    assertEquals(keyEventFor(app, open, Key.Char('w')), None)
    assertEquals(keyEventFor(app, open, Key.Ctrl('C')), None)
    val (closed, cmd) = app.update(Event.FullTranscriptBack, open)
    assertEquals(closed.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("the full transcript follows the tail at offset 0 and clamps a huge offset"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = Vector(Entry.User("hello"), foldedTurn))
    val tail = fsLines(app, state.showFullTranscript, 70, 12)
    assert(tail.exists(_.contains("the answer")), tail.mkString("|"))
    val following = RangeRe.findFirstMatchIn(tail.last).getOrElse(fail(s"no range in '${tail.last}'"))
    assertEquals(following.group(2), following.group(3)) // the window ends at the tail
    // Scrolled past the top, the window clamps there rather than running off it.
    val top = fsLines(app, state.copy(overlay = Overlay.FullTranscript(9999)), 70, 12)
    assert(top.exists(_.contains("hello")), top.mkString("|"))
    assertEquals(RangeRe.findFirstMatchIn(top.last).map(_.group(1)), Some("1"))
    val (followed, _) = app.update(Event.FullTranscriptFollow, state.copy(overlay = Overlay.FullTranscript(9999)))
    assertEquals(followed.overlay, Overlay.FullTranscript(0))

  test("the folded summary points at the full transcript, but only while the turn is live"):
    val blocks = Vector(Block.Thinking(Typewriter.shown("mulling"), 0L, Some(3_000L)), Block.shownAnswer("done"))
    val (_, live) = plainLines(ChatState.initial.copy(phase = Phase.Streaming(blocks)))
    assert(live.exists(_.contains("ctrl+c o to view the full transcript")), live.mkString("|"))
    // Committed, the same turn is scrolled-past history and carries no pointer.
    val (committed, _) = plainLines(ChatState.initial.copy(history = Vector(Entry.Assistant(blocks))))
    assert(committed.exists(_.contains("Thought for 3.0s")), committed.mkString("|"))
    assert(!committed.exists(_.contains("ctrl+c o")), committed.mkString("|"))

  test("fullscreen chat: the which-key strip is pinned to the frame's bottom edge"):
    val app = fullscreenApp
    val lines = fsLines(app, ChatState.initial.copy(history = rounds(3)).showKeyBindings, 60, 24)
    assertEquals(lines.length, 24)
    assert(lines.exists(_.trim == "ctrl+c"), lines.mkString("|"))
    assert(lines.exists(l => l.contains("c,q") && l.contains("exit")), lines.mkString("|"))
    // The grid's last row is the frame's very last line, below the footer.
    assert(lines.last.contains("interrupt"), lines.mkString("|"))
    assert(lines.indexWhere(_.trim == "ctrl+c") > lines.indexWhere(_.contains("›")), lines.mkString("|"))

  test("fullscreen chat: wheel and page keys map to chat scroll; inline leaves them unbound"):
    val app = fullscreenApp
    val state = ChatState.initial
    assertEquals(keyEventFor(app, state, Key.WheelUp(1, 1)), Some(Event.ChatScroll(-3)))
    assertEquals(keyEventFor(app, state, Key.WheelDown(1, 1)), Some(Event.ChatScroll(3)))
    assertEquals(keyEventFor(app, state, Key.PageUp), Some(Event.ChatScrollPage(-1)))
    assertEquals(keyEventFor(app, state, Key.PageDown), Some(Event.ChatScrollPage(1)))
    // Inline mode: native scroll/selection own the wheel, so these fall through.
    assertEquals(keyEventFor(appUI, state, Key.WheelUp(1, 1)), None)
    assertEquals(keyEventFor(appUI, state, Key.PageUp), None)

  /* ---- Wheel/page in the workflow views and pickers (Phase 6) ---- */

  test("workflow transcript: wheel and page scroll the bottom-anchored offset"):
    val app = fullscreenApp
    val ts = ChatState.initial.copy(overlay = Overlay.WorkflowTranscript("r", "n", 0))
    // The ±1 arrows now route through the same parameterized scroll event.
    assertEquals(keyEventFor(app, ts, Key.Up), Some(Event.WorkflowTranscriptScroll(1)))
    assertEquals(keyEventFor(app, ts, Key.Down), Some(Event.WorkflowTranscriptScroll(-1)))
    assertEquals(keyEventFor(app, ts, Key.WheelUp(1, 1)), Some(Event.WorkflowTranscriptScroll(3)))
    assertEquals(keyEventFor(app, ts, Key.WheelDown(1, 1)), Some(Event.WorkflowTranscriptScroll(-3)))
    // No prior render, so the page step falls back to one row (older is "up").
    assertEquals(keyEventFor(app, ts, Key.PageUp), Some(Event.WorkflowTranscriptScroll(1)))
    assertEquals(keyEventFor(app, ts, Key.PageDown), Some(Event.WorkflowTranscriptScroll(-1)))
    // The update applies the delta and floors the offset at 0.
    val (up, _) = app.update(Event.WorkflowTranscriptScroll(3), ts)
    assertEquals(up.overlay, Overlay.WorkflowTranscript("r", "n", 3))
    val (floored, _) = app.update(Event.WorkflowTranscriptScroll(-10), up)
    assertEquals(floored.overlay, Overlay.WorkflowTranscript("r", "n", 0))

  test("workflow list and detail step their selection on the wheel"):
    val app = fullscreenApp
    val list = ChatState.initial.copy(overlay = Overlay.WorkflowList(0))
    assertEquals(keyEventFor(app, list, Key.WheelUp(1, 1)), Some(Event.WorkflowListUp))
    assertEquals(keyEventFor(app, list, Key.WheelDown(1, 1)), Some(Event.WorkflowListDown))
    val detail = ChatState.initial.copy(overlay = Overlay.WorkflowDetail("r", 0))
    assertEquals(keyEventFor(app, detail, Key.WheelUp(1, 1)), Some(Event.WorkflowCursorUp))
    assertEquals(keyEventFor(app, detail, Key.WheelDown(1, 1)), Some(Event.WorkflowCursorDown))

  test("session and model pickers step their selection on the wheel"):
    val app = fullscreenApp
    val sessions = ChatState.initial.showSessionPicker(Vector(SessionSummary("a-session", None, 1, "x")))
    assertEquals(keyEventFor(app, sessions, Key.WheelUp(1, 1)), Some(Event.SessionPickerUp))
    assertEquals(keyEventFor(app, sessions, Key.WheelDown(1, 1)), Some(Event.SessionPickerDown))
    val models = ChatState.initial.showModelPicker(sampleChoices)
    assertEquals(keyEventFor(app, models, Key.WheelUp(1, 1)), Some(Event.ModelPickerUp))
    assertEquals(keyEventFor(app, models, Key.WheelDown(1, 1)), Some(Event.ModelPickerDown))

  test("slash palette: wheel steps the completion; page/press are inert; typing still delegates"):
    val app = fullscreenApp
    val slash = ChatState.initial.copy(input = "/", cursor = 1, overlay = Overlay.SlashPalette(0))
    assertEquals(keyEventFor(app, slash, Key.WheelUp(1, 1)), Some(Event.SlashPaletteUp))
    assertEquals(keyEventFor(app, slash, Key.WheelDown(1, 1)), Some(Event.SlashPaletteDown))
    assertEquals(keyEventFor(app, slash, Key.PageUp), None)
    assertEquals(keyEventFor(app, slash, Key.MousePress(0, 1, 1)), None)
    assertEquals(keyEventFor(app, slash, Key.Char('x')), Some(Event.KeyChar('x')))

  test("left clicks drive selection in the body but stay inert in overlays and inline mode, and the wheel never dismisses the keybindings menu"):
    val app = fullscreenApp
    // Overlay.None in fullscreen: a left press/release now drives drag-selection.
    assertEquals(keyEventFor(app, ChatState.initial, Key.MousePress(0, 1, 1)), Some(Event.MouseDown(1, 1)))
    assertEquals(keyEventFor(app, ChatState.initial, Key.MouseRelease(0, 1, 1)), Some(Event.MouseUp(1, 1)))
    // Middle/right buttons stay inert even in fullscreen normal mode.
    assertEquals(keyEventFor(app, ChatState.initial, Key.MousePress(2, 1, 1)), None)
    // Inline mode leaves all mouse keys unbound so native selection keeps working.
    assertEquals(keyEventFor(appUI, ChatState.initial, Key.MousePress(0, 1, 1)), None)
    assertEquals(keyEventFor(appUI, ChatState.initial, Key.MouseRelease(0, 1, 1)), None)
    // The keybindings menu treats a stray wheel/click as inert, NOT a failed chord
    // that closes it; a real command key still dispatches.
    val kb = ChatState.initial.showKeyBindings
    assertEquals(keyEventFor(app, kb, Key.WheelUp(1, 1)), None)
    assertEquals(keyEventFor(app, kb, Key.MousePress(0, 1, 1)), None)
    assertEquals(keyEventFor(app, kb, Key.Char('c')), Some(Event.RunCommand("c")))
    // A genuine non-command key still dismisses (unchanged behavior).
    assertEquals(keyEventFor(app, kb, Key.Char('z')), Some(Event.HideOverlay))

  /* ---- Fullscreen drag-selection + clipboard copy (Stage B) ---- */

  // The selection highlight background (FrameBlue) as a plain Color, for asserting
  // which cells the frame painted without reaching into ChatApp's private style.
  private val SelBg: Color = Color.True(135, 206, 235)

  private def recordingApp(sink: String => Unit): ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      mode = DisplayMode.Fullscreen,
      copyToClipboard = sink
    )

  /** The fullscreen frame as styled lines (keeps span styles, unlike [[fsLines]]). */
  private def fsStyledLines(app: ChatApp, state: ChatState, width: Int, rows: Int): Vector[StyledLine] =
    val el = app.view(state, Viewport(width, rows)).fullscreen.getOrElse(fail("expected a fullscreen element"))
    Layout.lay(el, width)

  /** The 1-based screen column where display char `charIdx` of `line` begins. */
  private def screenCol(line: String, charIdx: Int): Int = Width.stringWidth(line.substring(0, charIdx)) + 1

  /** Fire a `Cmd.Fire` side effect (the copy), failing on any other shape. */
  private def fire(cmd: Cmd[Event]): Unit = cmd match
    case Cmd.Fire(effect) => effect()
    case other            => fail(s"expected Cmd.Fire, got $other")

  test("a drag-selection copies the exact multi-line, column-sliced text and shows a footer chip"):
    var copied: Option[String] = None
    val app = recordingApp(s => copied = Some(s))
    val state = ChatState.initial.copy(history = Vector(
      Entry.User("SELECTME alpha bravo"),
      Entry.Assistant(Vector(Block.shownAnswer("charlie delta echo\n\nfoxtrot golf hotel")))
    ))
    val frame = fsLines(app, state, 40, 20) // populates the scroll snapshot
    val rC = frame.indexWhere(_.contains("charlie"))
    val rF = frame.indexWhere(_.contains("foxtrot"))
    assert(rC >= 0 && rF > rC, frame.mkString("|"))
    // Press on the 'c' of charlie, release on the last 't' of foxtrot (inclusive).
    val downCol = screenCol(frame(rC), frame(rC).indexOf("charlie"))
    val upCol = screenCol(frame(rF), frame(rF).indexOf("foxtrot") + "foxtrot".length - 1)
    val (s1, _) = app.update(Event.MouseDown(downCol, rC + 1), state)
    assert(s1.selection.isDefined, "press must start a selection")
    val (s2, cmd) = app.update(Event.MouseUp(upCol, rF + 1), s1)
    fire(cmd)
    // The blank paragraph break between the two lines survives as an empty middle
    // line; the leading indent of the last line is content, the first line's is not.
    assertEquals(copied, Some("charlie delta echo\n\n  foxtrot"))
    assert(s2.selection.isDefined, "the selection stays highlighted after copy")
    assertEquals(s2.copied, Some("copied 3 lines"))
    assert(s2.notices.isEmpty, "copy feedback must not touch the sticky notices")
    // The chip renders in the footer, and the frame is still exactly rows lines
    // (the bottom stack height is unchanged, which is the whole point of the chip).
    val afterFrame = fsLines(app, s2, 40, 20)
    assertEquals(afterFrame.length, 20)
    assert(afterFrame.exists(_.contains("✓ copied 3 lines")), afterFrame.mkString("|"))
    // A subsequent click clears the selection and the chip together.
    val (s3, _) = app.update(Event.MouseDown(3, 5), s2)
    val (s4, _) = app.update(Event.MouseUp(3, 5), s3)
    assertEquals(s4.selection, None)
    assertEquals(s4.copied, None)
    assert(!fsLines(app, s4, 40, 20).exists(_.contains("✓ copied")), "the chip must clear with the selection")

  test("a drag-selection includes whole wide CJK glyphs, end-inclusive"):
    var copied: Option[String] = None
    val app = recordingApp(s => copied = Some(s))
    val state = ChatState.initial.copy(history = Vector(Entry.User("CJK 你好世界 done")))
    val frame = fsLines(app, state, 40, 20)
    val r = frame.indexWhere(_.contains("你好世界"))
    assert(r >= 0, frame.mkString("|"))
    val line = frame(r)
    val start = line.indexOf("你好世界")
    // Select 好世: press at the start cell of 好, release at the start cell of 世
    // (end-inclusive, so the whole two-cell 世 is taken).
    val (s1, _) = app.update(Event.MouseDown(screenCol(line, start + 1), r + 1), state)
    val (_, cmd) = app.update(Event.MouseUp(screenCol(line, start + 2), r + 1), s1)
    fire(cmd)
    assertEquals(copied, Some("好世"))

  test("a plain click (press and release on one cell) clears the selection and copies nothing"):
    var copied: Option[String] = None
    val app = recordingApp(s => copied = Some(s))
    val state = ChatState.initial.copy(history = Vector(Entry.User("hello there")))
    fsLines(app, state, 40, 20)
    val (s1, _) = app.update(Event.MouseDown(5, 5), state)
    assert(s1.selection.isDefined)
    val (s2, cmd) = app.update(Event.MouseUp(5, 5), s1)
    assertEquals(s2.selection, None)
    assertEquals(s2.copied, None)
    assertEquals(cmd, Cmd.none)
    assert(copied.isEmpty, "a plain click must not copy")

  test("a press on the bottom stack starts no selection and clears any prior one"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(
      history = Vector(Entry.User("hello there")),
      selection = Some(Selection(0, 0, 1, 1, 40))
    )
    val frame = fsLines(app, state, 40, 20)
    // The last `›` is the input box's (the committed user line above carries one too).
    val promptRow = frame.lastIndexWhere(_.contains("›")) + 1 // 1-based, in the bottom stack
    val (s1, cmd) = app.update(Event.MouseDown(3, promptRow), state)
    assertEquals(s1.selection, None)
    assertEquals(cmd, Cmd.none)

  test("dragging at the top screen row folds a one-line scroll-up (edge auto-scroll)"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(40), chatScroll = Some(10))
    fsLines(app, state, 40, 20)
    val (s1, _) = app.update(Event.MouseDown(3, 6), state)
    assert(s1.selection.isDefined)
    val (s2, _) = app.update(Event.MouseDragTo(3, 1), s1)
    assertEquals(s2.chatScroll, Some(9), "a drag on the first screen row scrolls up by one")

  test("switchedTo clears the drag-selection and its copy chip"):
    val events = List(SessionEvent.UserSubmitted("q"))
    val snapshot = SessionSnapshot(SessionSummary.from("s", None, events), events)
    val cleared = ChatState.initial
      .copy(selection = Some(Selection(0, 0, 1, 1, 40)), copied = Some("copied 2 lines"))
      .switchedTo(snapshot)
    assertEquals(cleared.selection, None)
    assertEquals(cleared.copied, None)

  test("a selection made at another width is not highlighted, and the next press replaces it"):
    val app = fullscreenApp
    val state = ChatState.initial.copy(history = rounds(6), selection = Some(Selection(4, 0, 5, 3, 80)))
    // Render at width 40: the width-80 selection must paint nothing.
    val styled = fsStyledLines(app, state, 40, 20)
    assert(
      !styled.exists(_.spans.exists(_.style.bgColor == SelBg)),
      "a selection made at a different width must not be highlighted"
    )
    // The next press starts a fresh selection stamped with the current width.
    val (s1, _) = app.update(Event.MouseDown(3, 5), state)
    s1.selection match
      case Some(sel) => assertEquals(sel.width, 40)
      case None      => fail("a press should start a fresh selection at the new width")

  test("the frame highlights exactly the selected cells and stays viewport.rows lines"):
    val app = fullscreenApp
    val state0 = ChatState.initial.copy(history = Vector(Entry.User("PICKME alpha bravo")))
    val frame = fsLines(app, state0, 40, 20) // learn the layout
    val r = frame.indexWhere(_.contains("PICKME"))
    assert(r >= 0, frame.mkString("|"))
    val line = frame(r)
    val startChar = line.indexOf("alpha")
    val endChar = startChar + "alpha".length - 1
    val startCell = Width.stringWidth(line.substring(0, startChar))
    val endCell = Width.stringWidth(line.substring(0, endChar))
    // top == 0 and no sticky row here, so content line == frame index.
    val sel = Selection(r, startCell, r, endCell, 40)
    val styled = fsStyledLines(app, state0.copy(selection = Some(sel)), 40, 20)
    assertEquals(styled.length, 20)
    val hl = styled(r).spans.filter(_.style.bgColor == SelBg).map(_.text).mkString
    assertEquals(hl, "alpha")
    // No other row carries the highlight.
    assertEquals(styled.zipWithIndex.count((l, _) => l.spans.exists(_.style.bgColor == SelBg)), 1)

  // ---- API-failure retry: working-line countdown and partial rewind ----------

  test("the working line shows the retry countdown during a backoff wait"):
    val s = ChatState.initial.copy(
      phase = Phase.Waiting,
      clockMs = 10_000,
      turnStartMs = 9_000,
      retry = Some(RetryState(2, 6, nextAtMs = 13_200))
    )
    val (_, live) = plainLines(s)
    assert(
      live.exists(l => l.contains("Retrying") && l.contains("(attempt 2/6 failed, next in 4s)")),
      live.mkString("|")
    )
    // Once the wait ends (RoundStart clears `retry`), the normal label returns.
    val (_, normal) = plainLines(s.roundStarted)
    assert(normal.exists(_.contains("Working…")), normal.mkString("|"))
    assert(!normal.exists(_.contains("Retrying")), normal.mkString("|"))

  test("a Retrying stream event rewinds the dead attempt's partial output"):
    val app = appUI
    def fold(state: ChatState, ev: StreamEvent): ChatState =
      app.update(Event.Inbound1(AgentEvent.Stream(Right(ev))), state)._1
    val waiting = ChatState.initial.copy(phase = Phase.Waiting)
    val partial = fold(fold(waiting, StreamEvent.RoundStart), StreamEvent.Delta("doomed partial"))
    assert(partial.streamingBlocks.nonEmpty)
    val retrying = fold(partial, StreamEvent.Retrying(1, 6, 4_000, "429 rate limited"))
    // The first round's partial rewinds the turn to the waiting spinner, with
    // the countdown armed; nothing was committed to the transcript.
    assertEquals(retrying.phase, Phase.Waiting)
    assert(retrying.retry.exists(r => r.attempt == 1 && r.maxAttempts == 6))
    assertEquals(retrying.history, Vector.empty)
