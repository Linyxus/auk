package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub}
import auk.tui.render.Width
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, UserCommand}
import auk.llm.endpoint.{ChatResponse, FinishReason, Message}
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}

class ChatAppViewSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    val events = UnboundedChannel[AgentEvent]()
    val commands = UnboundedChannel[UserCommand]()
    ChatApp(events.asReadable, commands, UnboundedChannel[Unit]())

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

  private def plainLines(state: ChatState, width: Int = 60): (Vector[String], Vector[String]) =
    val screen = appUI.view(state)
    val committed = screen.committed.flatMap(Layout.lay(_, width)).map(_.plain)
    val live = Layout.lay(screen.live, width).map(_.plain)
    (committed, live)

  private def panelLines(state: ChatState, width: Int = 60): Vector[String] =
    panelLinesFor(appUI, state, width)

  private def panelLinesFor(app: ChatApp, state: ChatState, width: Int = 60): Vector[String] =
    val lines = Layout.lay(app.view(state).live, width).map(_.plain)
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

  test("initial view: header is committed; hint, prompt, and footer are live") {
    val (committed, live) = plainLines(ChatState.initial)
    assert(committed.exists(_.contains("Auk")), committed.mkString("|"))
    assert(committed.exists(_.contains("a coding agent")))
    assert(live.exists(_.contains("Type a message and press Enter")), live.mkString("|"))
    assert(live.exists(_.contains("›")), "prompt arrow missing")
    assert(live.exists(_.contains("ctrl+c for commands")), "ctrl+c footer hint missing")
    assert(live.exists(_.contains("ctrl+q quit")), "footer missing")
  }

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

  test("key bindings overlay renders above the input box in the live region"):
    assert(panelLines(ChatState.initial).isEmpty)
    val screen = appUI.view(ChatState.initial.showKeyBindings)
    assert(screen.overlay.isEmpty)
    val live = Layout.lay(screen.live, 60).map(_.plain)
    val overlay = panelLines(ChatState.initial.showKeyBindings)
    assert(overlay.head.startsWith("┌"), overlay.mkString("|"))
    assert(overlay.last.startsWith("└"), overlay.mkString("|"))
    assert(overlay(1).contains("Commands"), overlay.mkString("|"))
    assert(overlay.exists(_.contains("c, q    exit")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("r") && line.contains("resume session")), overlay.mkString("|"))
    assert(overlay.exists(line => line.contains("n") && line.contains("new session")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("Enter")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("Ctrl+Q")), overlay.mkString("|"))
    assert(overlay.map(_.length).distinct.size == 1, overlay.mkString("|"))
    assert(live.indexWhere(_.startsWith("┌")) < live.indexWhere(_.contains("›")), live.mkString("|"))

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
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit]())

    val (resumeState, resumeCmd) = app.update(Event.RunCommand("r"), ChatState.initial.showKeyBindings)
    assertEquals(resumeState.overlay, Overlay.ResumeLoading("Loading sessions"))
    assertEquals(fireAndRead(resumeCmd, commands), UserCommand.ListSessions)

    val (newState, newCmd) = app.update(Event.RunCommand("n"), ChatState.initial.showKeyBindings)
    assertEquals(newState.overlay, Overlay.ResumeLoading("Starting new session"))
    assertEquals(fireAndRead(newCmd, commands), UserCommand.NewSession)

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
        keyCommands = Vector(ChatApp.Command(Vector("m", "n"), "mock command")(state => (state.copy(input = "ran"), Cmd.none)))
      )
    val state = ChatState.initial.showKeyBindings
    val overlay = panelLinesFor(customApp, state)
    assert(overlay.exists(_.contains("m, n    mock command")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("c, q    exit")), overlay.mkString("|"))

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
    val app = ChatApp(events.asReadable, commands, UnboundedChannel[Unit]())
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
    assertEquals(next.history, Vector(Entry.User("previous question"), Entry.Assistant(Vector(Block.Answer(Typewriter.shown("previous answer"))))))
    assertEquals(next.input, "")
    assertEquals(next.overlay, Overlay.None)
    assertEquals(appUI.view(next).committedEpoch, 1L)

  test("a submitted message commits a You entry; the hint disappears") {
    val state = ChatState.initial.submitted("hello there").copy(phase = Phase.Waiting)
    val (committed, live) = plainLines(state)
    assert(committed.exists(_.contains("You")), committed.mkString("|"))
    assert(committed.exists(_.contains("hello there")))
    assert(!live.exists(_.contains("Type a message")), "hint should be gone once history is non-empty")
    // Waiting phase shows the spinner label in the live region.
    assert(live.exists(_.contains("auk is thinking")), live.mkString("|"))
  }

  test("divider rule expands to the layout width") {
    val (_, live) = plainLines(ChatState.initial, width = 30)
    assert(live.exists(l => l.nonEmpty && l.forall(_ == '─') && l.length == 30), live.mkString("|"))
  }

  test("input prompt wraps inside the frame width") {
    val input = "abcdefghijklmnopqrstuvwxyz"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 12)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    def isRule(line: String): Boolean = line.nonEmpty && line.forall(_ == '─')
    val promptRows = live.drop(start).takeWhile(line => !isRule(line))
    val content = promptRows.map(_.drop(4)).mkString

    assert(promptRows.length > 1, promptRows.mkString("|"))
    assert(promptRows.forall(line => Width.stringWidth(line) <= 12), promptRows.mkString("|"))
    assert(promptRows.head.startsWith("  › "), promptRows.head)
    assert(promptRows.tail.forall(_.startsWith("    ")), promptRows.mkString("|"))
    assert(content.startsWith(input), content)
  }

  test("input prompt renders explicit newlines") {
    val input = "alpha\nbeta"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 40)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    def isRule(line: String): Boolean = line.nonEmpty && line.forall(_ == '─')
    val promptRows = live.drop(start).takeWhile(line => !isRule(line))

    assertEquals(promptRows.map(_.stripTrailing()), Vector("  › alpha", "    beta"))
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
    assert(live.exists(_.contains("▌")), s"cursor missing from live region: ${live.mkString("|")}")
    assert(live.exists(_.contains("h")), live.mkString("|"))
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
    val app = ChatApp(UnboundedChannel[AgentEvent]().asReadable, commands, UnboundedChannel[Unit](), modelChoices = choices)
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
    val (ok, _) = app.update(Event.Inbound1(AgentEvent.ModelSwitched("GLM 5.1", 200_000)), ChatState.initial)
    assertEquals(ok.modelName, "GLM 5.1")
    assertEquals(ok.contextWindow, 200_000)
    val live = Layout.lay(app.view(ok).live, 80).map(_.plain)
    assert(live.exists(_.contains("GLM 5.1")), live.mkString("|"))
  }

  /* ---- the eval_scala card ---- */

  private def evalTool(
      rawArgs: String,
      output: Option[String] = None,
      isError: Boolean = false,
      elapsedMs: Option[Long] = Some(0L)
  ): Block.Tool =
    Block.Tool("e1", "eval_scala", rawArgs, elapsedMs = elapsedMs, output = output, isError = isError)

  private def committedWith(block: Block): Vector[String] =
    val state = ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(block))))
    plainLines(state)._1

  test("eval_scala renders as a rounded card: code, rule, reply, then verdict"):
    val lines = committedWith(evalTool(
      """{"code":"val xs = (1 to 5).toList\nxs.sum"}""",
      output = Some("val xs: List[Int] = List(1, 2, 3, 4, 5)\nval res0: Int = 15\n")
    ))
    assert(lines.exists(_.contains("╭─ execution")), lines.mkString("|"))
    assert(lines.exists(_.contains("│ val xs = (1 to 5).toList")), lines.mkString("|"))
    assert(lines.exists(_.contains("│ xs.sum")), lines.mkString("|"))
    assert(lines.exists(_.contains("│ val xs: List[Int] = List(1, 2, 3, 4, 5)")), lines.mkString("|"))
    assert(lines.exists(_.contains("╰─ ✓")), lines.mkString("|"))
    // The card reads top to bottom: code, the ├─ rule, then the reply.
    val codeAt = lines.indexWhere(_.contains("val xs = (1 to 5).toList"))
    val ruleAt = lines.indexWhere(_.contains("├─"))
    val replyAt = lines.indexWhere(_.contains("val res0: Int = 15"))
    assert(codeAt >= 0 && codeAt < ruleAt && ruleAt < replyAt, lines.mkString("|"))

  test("a finished eval_scala card shows the time it took"):
    val lines = committedWith(evalTool(
      """{"code":"1 + 1"}""",
      output = Some("val res0: Int = 2\n"),
      elapsedMs = Some(400L)
    ))
    assert(lines.exists(_.contains("╰─ ✓ 0.4s")), lines.mkString("|"))

  test("eval_scala shows a placeholder body while the arguments are streaming"):
    val lines = committedWith(evalTool("""{"code":"val x = 1""")) // incomplete JSON
    assert(lines.exists(_.contains("╭─ execution")), lines.mkString("|"))
    assert(lines.exists(_.contains("│ ⋯")), lines.mkString("|"))
    assert(!lines.exists(_.contains("├─")), lines.mkString("|"))

  test("eval_scala caps a long REPL reply with a more-lines marker"):
    val long = (1 to 40).map(i => s"line-$i").mkString("\n")
    val lines = committedWith(evalTool("""{"code":"1 + 1"}""", output = Some(long)))
    assert(lines.exists(_.contains("line-12")), lines.mkString("|"))
    assert(!lines.exists(_.contains("line-13")), lines.mkString("|"))
    assert(lines.exists(_.contains("… +28 more lines")), lines.mkString("|"))

  test("a failed eval_scala shows the diagnostic and an ✗ verdict"):
    val lines = committedWith(evalTool(
      """{"code":"oops +"}""",
      output = Some("-- [E018] Syntax Error ---\nexpression expected"),
      isError = true
    ))
    assert(lines.exists(_.contains("expression expected")), lines.mkString("|"))
    assert(lines.exists(_.contains("╰─ ✗")), lines.mkString("|"))
