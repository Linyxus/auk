package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, UserCommand, Inbox}

/** The slash-command palette: typing `/` into an empty input opens a live,
  * filtered list of every named [[ChatApp.Command]]; Enter runs the selection,
  * reusing the very same `run` the Ctrl-C hotkey path invokes. */
class SlashCommandSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

  /** An app whose only command is a custom one, so dispatch is observable. */
  private def appWith(cmds: Vector[ChatApp.Command]): ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      keyCommands = cmds
    )

  private def keyEventFor(app: ChatApp, state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil
    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  private def keyEvent(state: ChatState, key: Key): Option[Event] =
    keyEventFor(appUI, state, key)

  /** The live region of `state`, rendered to plain text at `width`. */
  private def liveLines(app: ChatApp, state: ChatState, width: Int = 60): Vector[String] =
    Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)

  /** The popup's candidate rows among `lines`: `     ▸ /name …` for the selected
    * row (the anchor indent plus the bar's marker), `       /name …` otherwise. */
  private def popupRows(lines: Vector[String]): Vector[String] =
    lines.filter(l => l.startsWith("     ▸ /") || l.startsWith("       /"))

  /** A state with the slash palette open: `query` is set in the input box (as
    * `/query`), matching how the palette is actually driven after the refactor. */
  private def slashOpen(query: String = "", selected: Int = 0): ChatState =
    ChatState.initial.copy(input = "/" + query, cursor = query.length + 1, overlay = Overlay.SlashPalette(selected))

  private def noop: ChatState => (ChatState, Cmd[Event]) = s => (s, Cmd.none)

  // -- pure: slashMatches and Command.named -----------------------------------

  test("slashMatches returns every named command on an empty query, excluding unnamed ones"):
    val named   = ChatApp.Command("a", "alpha")(noop).named("exit", "quit")
    val named2  = ChatApp.Command("b", "beta")(noop).named("model")
    val unnamed = ChatApp.Command("c", "gamma")(noop)
    val cmds    = Vector(named, unnamed, named2)
    assertEquals(ChatApp.slashMatches(cmds, ""), Vector(named, named2))   // order preserved, unnamed dropped
    assertEquals(ChatApp.slashMatches(cmds, "   "), Vector(named, named2)) // blank query == all

  test("slashMatches filters by substring, case-insensitively, across all names"):
    val a = ChatApp.Command("a", "")(noop).named("exit", "quit")
    val b = ChatApp.Command("b", "")(noop).named("model")
    val cmds = Vector(a, b)
    assertEquals(ChatApp.slashMatches(cmds, "EX"), Vector(a))   // matches "exit"
    assertEquals(ChatApp.slashMatches(cmds, "uit"), Vector(a))  // matches the secondary name "quit"
    assertEquals(ChatApp.slashMatches(cmds, "od"), Vector(b))   // substring, not just prefix
    assertEquals(ChatApp.slashMatches(cmds, "zzz"), Vector.empty)

  test("Command.named lowercases the names and leaves keys untouched"):
    val c = ChatApp.Command(Vector("c", "q"), "exit")(noop).named("Exit", "QUIT")
    assertEquals(c.names, Vector("exit", "quit"))
    assertEquals(c.keys, Vector("c", "q"))

  // -- trigger: `/` on empty input opens the palette --------------------------

  test("`/` on an empty input inserts `/` and opens the palette; `/` mid-input inserts literally"):
    val (opened, c1) = appUI.update(Event.KeyChar('/'), ChatState.initial)
    assertEquals(opened.overlay, Overlay.SlashPalette(0))
    assertEquals(opened.input, "/")
    assertEquals(opened.cursor, 1)
    assertEquals(c1, Cmd.none)

    val typing = ChatState.initial.copy(input = "ab", cursor = 2)
    val (inserted, c2) = appUI.update(Event.KeyChar('/'), typing)
    assertEquals(inserted.overlay, Overlay.None)
    assertEquals(inserted.input, "ab/")
    assertEquals(c2, Cmd.none)

  test("a `/` keystroke decodes to KeyChar('/') via the normal input handler"):
    assertEquals(keyEvent(ChatState.initial, Key.Char('/')), Some(Event.KeyChar('/')))

  // -- key routing while the palette is open ----------------------------------

  test("the open palette routes navigation, run, complete, and cancel; typing/backspace go through normal events"):
    val s = slashOpen()
    // Up/Down/Enter/Tab/Esc are special-cased.
    assertEquals(keyEvent(s, Key.Up), Some(Event.SlashPaletteUp))
    assertEquals(keyEvent(s, Key.Down), Some(Event.SlashPaletteDown))
    assertEquals(keyEvent(s, Key.Enter), Some(Event.SlashSelected))
    assertEquals(keyEvent(s, Key.Tab), Some(Event.SlashComplete))
    assertEquals(keyEvent(s, Key.Esc), Some(Event.HideOverlay))
    // Typing and backspace delegate to the normal input handler (the typed text
    // lives in the input box).
    assertEquals(keyEvent(s, Key.Char('m')), Some(Event.KeyChar('m')))
    assertEquals(keyEvent(s, Key.Backspace), Some(Event.Backspace))
    assertEquals(keyEvent(s, Key.Delete), Some(Event.DeleteForward))

  // -- update: query editing and selection clamping ---------------------------

  test("typing extends the input query and resets the selection"):
    val (next, cmd) = appUI.update(Event.KeyChar('m'), slashOpen(query = "", selected = 3))
    assertEquals(next.overlay, Overlay.SlashPalette(0))
    assertEquals(next.input, "/m")
    assertEquals(next.cursor, 2)
    assertEquals(cmd, Cmd.none)

  test("backspace edits the input query, and backspacing past `/` closes the palette"):
    val (dropped, _) = appUI.update(Event.Backspace, slashOpen(query = "mo", selected = 0))
    assertEquals(dropped.overlay, Overlay.SlashPalette(0))
    assertEquals(dropped.input, "/m")
    val (closed, _) = appUI.update(Event.Backspace, slashOpen(query = "", selected = 0))
    assertEquals(closed.overlay, Overlay.None) // backspacing past the `/` exits
    assertEquals(closed.input, "") // the `/` itself is gone

  test("Up/Down move the selection, clamped to the filtered range"):
    // Empty query lists all default commands (>1), so Down advances and Up floors at 0.
    val (down, _) = appUI.update(Event.SlashPaletteDown, slashOpen(query = "", selected = 0))
    assertEquals(down.overlay, Overlay.SlashPalette(1))
    val (up, _) = appUI.update(Event.SlashPaletteUp, slashOpen(query = "", selected = 0))
    assertEquals(up.overlay, Overlay.SlashPalette(0))
    // A query that matches exactly one command clamps any Down back to 0.
    val (clamped, _) = appUI.update(Event.SlashPaletteDown, slashOpen(query = "model", selected = 0))
    assertEquals(clamped.overlay, Overlay.SlashPalette(0))

  // -- dispatch: running the selection reuses the command's `run` -------------

  test("selecting `/exit` quits and closes the overlay"):
    val (next, cmd) = appUI.update(Event.SlashSelected, slashOpen(query = "exit", selected = 0))
    assertEquals(next.overlay, Overlay.None)
    assertEquals(next.input, "") // input is cleared after dispatch
    cmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit, got $other")

  test("selecting a custom command runs its effect on the state"):
    val app = appWith(Vector(ChatApp.Command("x", "do the thing")(s => (s.copy(input = "ran"), Cmd.none)).named("run")))
    val (next, cmd) = app.update(Event.SlashSelected, slashOpen(query = "run", selected = 0))
    assertEquals(next.input, "ran")
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("a no-match selection just closes the palette"):
    val (next, cmd) = appUI.update(Event.SlashSelected, slashOpen(query = "zzz", selected = 0))
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  test("Enter on an empty query is a no-op: the palette stays open, nothing runs"):
    val (next, cmd) = appUI.update(Event.SlashSelected, slashOpen(query = "", selected = 0))
    assertEquals(next.overlay, Overlay.SlashPalette(0)) // unchanged, still open
    assertEquals(next.input, "/") // input untouched
    assertEquals(cmd, Cmd.none)

  test("slash dispatch shares the hotkey path's gating: `/resume` while busy is a no-op"):
    // resume.run gates on idle; invoked while not idle it only dismisses, exactly
    // as the Ctrl-C `r` hotkey would — no UserCommand is fired.
    val busy = slashOpen(query = "resume", selected = 0).copy(phase = Phase.Waiting)
    val (next, cmd) = appUI.update(Event.SlashSelected, busy)
    assertEquals(next.overlay, Overlay.None)
    assertEquals(cmd, Cmd.none)

  // -- Tab: complete the selected command name into the input box ----------------

  test("Tab completes the selected command name into the input without running"):
    val (next, cmd) = appUI.update(Event.SlashComplete, slashOpen(query = "ex", selected = 0))
    // The first match for "ex" is "exit"; completing inserts "/exit".
    assertEquals(next.input, "/exit")
    assertEquals(next.cursor, 5)
    assertEquals(next.overlay, Overlay.SlashPalette(0)) // palette stays open
    assertEquals(cmd, Cmd.none) // nothing runs

  // -- rendering --------------------------------------------------------------

  test("the popup lists the filtered commands docked right above the input box"):
    val lines = liveLines(appUI, slashOpen())
    val rows = popupRows(lines)
    assert(rows.exists(l => l.contains("/exit") && l.contains("exit")), lines.mkString("|"))
    assert(rows.exists(l => l.contains("/model") && l.contains("switch model")), lines.mkString("|"))
    assert(rows.exists(_.contains("/interrupt")), lines.mkString("|"))
    // The first row is selected by default: the full-row bar carries the marker.
    assert(rows.head.startsWith("     ▸ /exit"), rows.mkString("|"))
    // No panel chrome: no frame, no title, no key-hint footer.
    assert(!lines.exists(_.startsWith("┌")), lines.mkString("|"))
    assert(!lines.exists(_.contains("Commands")), lines.mkString("|"))
    assert(!lines.exists(_.contains("Tab complete")), lines.mkString("|"))
    // Docked: the last candidate row sits immediately above the input box frame.
    val frameTop = lines.indexWhere(_.startsWith("╭─"))
    assert(frameTop > 0, lines.mkString("|"))
    assertEquals(lines(frameTop - 1), rows.last, lines.mkString("|"))

  test("typing a query narrows the rendered rows and moves the selection bar"):
    val lines = liveLines(appUI, slashOpen(query = "mod"))
    val rows = popupRows(lines)
    assert(rows.exists(_.contains("/model")), rows.mkString("|"))
    assert(!rows.exists(_.contains("/exit")), rows.mkString("|"))
    assert(rows.exists(l => l.startsWith("     ▸ ") && l.contains("/model")), rows.mkString("|"))

  test("a non-matching query renders the compact empty notice"):
    val lines = liveLines(appUI, slashOpen(query = "zzz"))
    assert(lines.exists(_.contains("No commands match")), lines.mkString("|"))
    assert(!lines.exists(_.contains("/exit")), lines.mkString("|"))
    assert(!lines.exists(_.contains("Esc to cancel")), lines.mkString("|"))

  test("an overflowing list windows to ten rows with a right-edge scrollbar"):
    val cmds = (1 to 14).toVector.map(i => ChatApp.Command("x", s"command number $i")(noop).named(f"c$i%02d"))
    val app = appWith(cmds)
    val top = popupRows(liveLines(app, slashOpen()))
    assertEquals(top.length, 10)
    assert(top.head.contains("/c01") && top.last.contains("/c10"), top.mkString("|"))
    assert(top.forall(_.endsWith("▐")), top.mkString("|"))
    // Selecting past the window scrolls it: the tail arrives, the head is gone.
    val bottom = popupRows(liveLines(app, slashOpen(selected = 13)))
    assert(bottom.last.startsWith("     ▸ /c14"), bottom.mkString("|"))
    assert(!bottom.exists(_.contains("/c01")), bottom.mkString("|"))

  test("a short list shows no scrollbar and sizes to its content"):
    val lines = liveLines(appUI, slashOpen())
    val rows = popupRows(lines)
    assert(rows.nonEmpty && rows.forall(!_.endsWith("▐")), rows.mkString("|"))
    // Fit-to-content: rows end well short of the 60-column viewport.
    assert(rows.forall(_.length < 50), rows.map(_.length).mkString(","))


  // -- edge cases from Phase 5 -------------------------------------------------

  test("typing a space while the palette is open closes it but keeps the text"):
    // A space makes isSlashPrefix false → reconcileSlashPalette hides the overlay,
    // but the typed text (including the space) stays in the input for re-editing.
    val (next, cmd) = appUI.update(Event.KeyChar(' '), slashOpen(query = "ex", selected = 0))
    assertEquals(next.overlay, Overlay.None)
    assertEquals(next.input, "/ex ")
    assertEquals(cmd, Cmd.none)

  test("Esc closes the palette but keeps the typed text for re-editing"):
    val (next, _) = appUI.update(Event.HideOverlay, slashOpen(query = "mod", selected = 2))
    assertEquals(next.overlay, Overlay.None)
    assertEquals(next.input, "/mod")

  test("DeleteForward (Delete key) works while the palette is open"):
    // Cursor at start, Delete removes the `/`; reconcile closes the palette.
    val s = slashOpen(query = "exit").copy(cursor = 0)
    val (next, _) = appUI.update(Event.DeleteForward, s)
    assertEquals(next.input, "exit")
    assertEquals(next.overlay, Overlay.None)

  test("Ctrl+C now routes through normalKeyEvent while the palette is open"):
    // Before the refactor, Ctrl+C was swallowed by the palette. Now it delegates
    // to normalKeyEvent, which maps Ctrl+C to ShowKeyBindings.
    val s = slashOpen()
    assertEquals(keyEvent(s, Key.Ctrl('C')), Some(Event.ShowKeyBindings))

  test("a full Tab→Enter flow completes then dispatches"):
    // Type "ex", Tab completes to "/exit", Enter dispatches and quits.
    val typed = slashOpen(query = "ex")
    val (completed, _) = appUI.update(Event.SlashComplete, typed)
    assertEquals(completed.input, "/exit")
    assert(completed.slashPaletteOpen, "palette should still be open after Tab")
    val (dispatched, cmd) = appUI.update(Event.SlashSelected, completed)
    assertEquals(dispatched.input, "")
    assertEquals(dispatched.overlay, Overlay.None)
    cmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit, got $other")