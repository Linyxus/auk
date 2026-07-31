package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, LoopGenerationState, LoopGenerationView, LoopView, TeamMemberView, UserCommand}
import auk.workflow.TranscriptEvent

/** The refinement loops in the TUI: the one-line census that stands in for them in the
  * bottom chrome, the on-demand `ctrl+c l` window that lists them, and the fullscreen
  * loop transcript opened from it. */
class LoopPanelSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

  private def gen(n: Int, state: LoopGenerationState, metrics: (String, Double)*): LoopGenerationView =
    LoopGenerationView(n, state, metrics.toVector, if state == LoopGenerationState.Accepted then s"gen $n" else "")

  private def accepted(n: Int, p99: Double): LoopGenerationView =
    gen(n, LoopGenerationState.Accepted, "p99Ms" -> p99)

  private def abandoned(n: Int): LoopGenerationView = gen(n, LoopGenerationState.Abandoned)

  /** A loop mid-generation with a lineage behind it — the window's ordinary case. */
  private def running(
      id: String = "perf",
      generations: Vector[LoopGenerationView] = Vector(accepted(1, 90), abandoned(2), accepted(3, 70)),
      activity: String = "gen 4, attempt 1 — evaluating",
      label: String = "gen-4-eval"
  ): LoopView =
    LoopView(
      id = id,
      phase = "running (gen 4)",
      goal = "cut p99 latency",
      generations = generations :+ gen(generations.length + 1, LoopGenerationState.Running),
      activity = Some(activity),
      liveLabel = Some(label),
      parked = None,
      orphaned = false
    )

  private def parked(id: String = "old", reason: String = "budget exhausted"): LoopView =
    LoopView(id, s"parked: $reason", "cut p99 latency", Vector(accepted(1, 90)), None, None, Some(reason), false)

  private def orphaned(id: String = "stray"): LoopView =
    LoopView(id, "orphaned (dead session)", "cut p99 latency", Vector(accepted(1, 90)), None, None, None, true)

  private def loops(n: Int): Vector[LoopView] =
    (1 to n).toVector.map(i => running(id = f"l$i%02d"))

  private def member(id: String): TeamMemberView =
    TeamMemberView(id, s"$id desc", working = false, inputTokens = 0, outputTokens = 0)

  private def liveLines(app: ChatApp, state: ChatState, width: Int = 120): Vector[String] =
    Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)

  /** The census line, as the bottom chrome renders it (there is at most one). */
  private def census(app: ChatApp, state: ChatState, width: Int = 120): Option[String] =
    liveLines(app, state, width).find(_.contains(" loop"))

  /** The fullscreen loops window, as plain rows. */
  private def windowLines(
      app: ChatApp,
      state: ChatState,
      selected: Int = 0,
      width: Int = 100,
      rows: Int = 24
  ): Vector[String] =
    val open = state.copy(overlay = Overlay.Loops(selected))
    Layout.lay(app.view(open, Viewport(width, rows)).fullscreen.get, width).map(_.plain)

  /** The fullscreen loop-transcript frame, as plain rows. */
  private def fsLines(
      app: ChatApp,
      state: ChatState,
      loopId: String,
      offset: Int = 0,
      width: Int = 80,
      rows: Int = 24
  ): Vector[String] =
    val open = state.copy(overlay = Overlay.LoopTranscript(loopId, offset))
    Layout.lay(app.view(open, Viewport(width, rows)).fullscreen.get, width).map(_.plain)

  private def activity(app: ChatApp, st: ChatState, evs: TranscriptEvent*): ChatState =
    evs.foldLeft(st)((s, ev) => app.update(Event.Inbound1(AgentEvent.Activity(ev)), s)._1)

  private def keyEvent(app: ChatApp, state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil
    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  private def hasTick(sub: Sub[Event]): Boolean =
    sub match
      case Sub.Batch(ss)         => ss.exists(hasTick)
      case Sub.TimeEveryMs(_, _) => true
      case _                     => false

  // -- the census line ---------------------------------------------------------------

  test("a Loops snapshot folds into state and shows as ONE line between the footer and the subagents"):
    val app = appUI
    val withTeam = ChatState.initial.copy(team = Vector(member("scribe")))
    val (st, _) = app.update(Event.Inbound1(AgentEvent.Loops(Vector(running()))), withTeam)
    assertEquals(st.loops.map(_.id), Vector("perf"))
    val lines = liveLines(app, st)
    val footer = lines.indexWhere(_.contains("ctrl+c or / for commands"))
    val hint = lines.indexWhere(_.contains("ctrl+c l to view"))
    val teamFrame = lines.indexWhere(_.contains("╭─ subagents"))
    assert(footer >= 0 && hint > footer, lines.mkString("|"))
    assert(teamFrame > hint, lines.mkString("|"))
    // Exactly one line: the loops occupy a row, not a panel.
    assertEquals(lines.count(_.contains("ctrl+c l to view")), 1, lines.mkString("|"))

  test("the always-on panel is gone: the bottom chrome carries no framed rows, however many loops"):
    val app = appUI
    val lines = liveLines(app, ChatState.initial.copy(loops = loops(6), team = Vector(member("scribe"))))
    assert(!lines.exists(_.contains("╭─ loops")), lines.mkString("|"))
    // Six loops, one line: no per-loop row, no lineage strip, no headline metric.
    assertEquals(lines.count(_.contains("l0")), 1, lines.mkString("|"))
    assert(!lines.exists(_.contains("l02")), lines.mkString("|"))
    assert(!lines.exists(_.contains("✓1")), lines.mkString("|"))
    assert(!lines.exists(_.contains("p99Ms")), lines.mkString("|"))
    // The subagent panel is untouched by any of it.
    assert(lines.exists(_.contains("╭─ subagents")), lines.mkString("|"))

  test("no census line while there are no loops"):
    val lines = liveLines(appUI, ChatState.initial)
    assert(!lines.exists(_.contains("loop")), lines.mkString("|"))

  test("the census names the count, the loop worth naming, what it is doing, and the chord"):
    val app = appUI
    val line = census(app, ChatState.initial.copy(loops = Vector(running()))).getOrElse(fail("no census line"))
    assert(line.contains("1 loop ") && !line.contains("1 loops"), line)
    assert(line.contains("'perf'"), line)
    assert(line.contains("gen 4, attempt 1 — evaluating"), line)
    assert(line.contains("ctrl+c l to view"), line)

  test("several loops pluralise, and the detail follows the one actually running"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(parked(), running(), orphaned()))
    val line = census(app, st).getOrElse(fail("no census line"))
    assert(line.contains("3 loops"), line)
    assert(line.contains("'perf'") && !line.contains("'old'"), line)

  test("with nothing running the census still names the first loop and why it is not going"):
    val app = appUI
    val line = census(app, ChatState.initial.copy(loops = Vector(parked(), orphaned()))).getOrElse(fail("no census"))
    assert(line.contains("2 loops") && line.contains("'old'"), line)
    assert(line.contains("parked: budget exhausted"), line)

  test("the census is exactly one line, never wider than the terminal, at any width"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running(id = "a-rather-long-loop-name")))
    for width <- Vector(24, 30, 40, 56, 72, 100, 160) do
      val lines = liveLines(app, st, width)
      val hits = lines.filter(l => l.contains(" loop") && !l.contains("ctrl+c or /"))
      assertEquals(hits.length, 1, s"width $width: ${lines.mkString("|")}")
      assert(hits.head.length <= width, s"width $width overflowed: '${hits.head}'")
      // The census never goes; the hint and then the detail are what a narrow
      // terminal sheds.
      assert(hits.head.contains("1 loop"), s"width $width: '${hits.head}'")

  // -- the window --------------------------------------------------------------------

  test("ctrl+c l opens the loops window and Esc closes it"):
    val app = appUI
    val menu = ChatState.initial.copy(loops = loops(2)).showKeyBindings
    assertEquals(keyEvent(app, menu, Key.Char('l')), Some(Event.RunCommand("l")))
    val (open, cmd) = app.update(Event.RunCommand("l"), menu)
    assertEquals(open.overlay, Overlay.Loops(0))
    // Taking over the screen rides a full repaint, as every screen switch does.
    assertEquals(cmd, Cmd.batch(Cmd.none, Cmd.refresh))
    assertEquals(keyEvent(app, open, Key.Esc), Some(Event.HideOverlay))
    assertEquals(app.update(Event.HideOverlay, open)._1.overlay, Overlay.None)

  test("the window lists every loop with its lineage strip, headline metric and live stage"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running(), parked(), orphaned()))
    val lines = windowLines(app, st, width = 110)
    assert(lines.exists(_.contains("Loops · 3")), lines.mkString("|"))
    val row = lines.find(_.contains("perf")).getOrElse(fail("the running loop's row is missing"))
    assert(row.contains("✓1 ✗2 ✓3 ⋯4"), row)
    assert(row.contains("p99Ms 70 ↓"), row)
    assert(row.contains("gen 4, attempt 1 — evaluating"), row)
    // A parked loop and an orphaned one read differently: badge and tail both.
    val park = lines.find(_.contains("old")).getOrElse(fail("the parked loop's row is missing"))
    val stray = lines.find(_.contains("stray")).getOrElse(fail("the orphaned loop's row is missing"))
    assert(park.contains("×") && park.contains("parked: budget exhausted"), park)
    assert(stray.contains("⚠") && stray.contains("orphaned (dead session)"), stray)
    // The footer names the window's keys.
    assert(lines.exists(l => l.contains("↑/↓ select") && l.contains("Enter view") && l.contains("Esc close")), lines.mkString("|"))

  test("the trend arrow reports direction only, and is absent with nothing to compare"):
    val app = appUI
    def row(v: LoopView): String =
      windowLines(app, ChatState.initial.copy(loops = Vector(v)), width = 110).find(_.contains("perf")).getOrElse("")
    val rose = row(running(generations = Vector(accepted(1, 70), accepted(2, 90))))
    val first = row(running(generations = Vector(accepted(1, 70))))
    val flat = row(running(generations = Vector(accepted(1, 70), accepted(2, 70))))
    assert(rose.contains("p99Ms 90 ↑"), rose)
    assert(first.contains("p99Ms 70") && !first.contains("↓") && !first.contains("↑"), first)
    assert(flat.contains("p99Ms 70") && !flat.contains("↑"), flat)

  test("a long lineage is truncated from the left, keeping the newest generations"):
    val app = appUI
    val long = running(generations = (1 to 12).toVector.map(i => accepted(i, 100.0 - i)))
    val row = windowLines(app, ChatState.initial.copy(loops = Vector(long)), width = 110).find(_.contains("perf")).getOrElse("")
    assert(row.contains("✓12") && row.contains("⋯13"), row)
    assert(row.contains("…") && !row.contains("✓1 ✓2"), row)

  test("a narrow window sheds the metric and then the strip, never the name or the stage"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running()))
    // Middling: the metric is gone, the lineage — the thing a reader watches — is not.
    val mid = windowLines(app, st, width = 52).find(_.contains("perf")).getOrElse(fail("the loop's row is missing"))
    assert(mid.contains("✓1") && !mid.contains("p99Ms"), mid)
    assert(mid.contains("gen 4"), mid)
    // Narrower still: the strip goes too, and the name and the stage are what is left.
    val row = windowLines(app, st, width = 44).find(_.contains("perf")).getOrElse(fail("the loop's row is missing"))
    assert(!row.contains("✓1") && !row.contains("p99Ms"), row)
    assert(row.contains("gen 4"), row)

  test("↑/↓ move the selection, clamped at both ends, and the marker follows it"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(0))
    assertEquals(keyEvent(app, st, Key.Down), Some(Event.LoopsDown))
    assertEquals(keyEvent(app, st, Key.Up), Some(Event.LoopsUp))
    assertEquals(keyEvent(app, st, Key.WheelDown(0, 0)), Some(Event.LoopsDown))
    assertEquals(keyEvent(app, st, Key.Enter), Some(Event.LoopOpen))
    val (down, _) = app.update(Event.LoopsDown, st)
    assertEquals(down.overlay, Overlay.Loops(1))
    // Both edges clamp rather than wrapping or dangling.
    assertEquals(app.update(Event.LoopsUp, st)._1.overlay, Overlay.Loops(0))
    val bottom = st.copy(overlay = Overlay.Loops(2))
    assertEquals(app.update(Event.LoopsDown, bottom)._1.overlay, Overlay.Loops(2))
    // The marker sits on the selected row and nowhere else.
    val rows = windowLines(app, st, selected = 1).filter(_.contains("l0"))
    assertEquals(rows.count(_.contains("›")), 1, rows.mkString("|"))
    assert(rows.find(_.contains("›")).exists(_.contains("l02")), rows.mkString("|"))

  test("the body window follows the selection and the footer reports where it sits"):
    val app = appUI
    // Eight loops in a frame whose body holds fewer: the visible slice moves with
    // the selection and the range says so.
    val st = ChatState.initial.copy(loops = loops(8))
    val top = windowLines(app, st, selected = 0, rows = 12).filter(_.contains("l0"))
    assert(top.head.contains("l01"), top.mkString("|"))
    val bottom = windowLines(app, st, selected = 7, rows = 12).filter(_.contains("l0"))
    assert(bottom.last.contains("l08"), bottom.mkString("|"))
    assert(!bottom.exists(_.contains("l01")), bottom.mkString("|"))
    assert(windowLines(app, st, selected = 7, rows = 12).exists(_.contains(s"of 8")), "the footer reports the range")

  test("opening with no loops says so rather than refusing to open"):
    val app = appUI
    val (open, _) = app.update(Event.RunCommand("l"), ChatState.initial.showKeyBindings)
    assertEquals(open.overlay, Overlay.Loops(0))
    val lines = windowLines(app, ChatState.initial)
    assert(lines.exists(_.contains("Loops · 0")), lines.mkString("|"))
    assert(lines.exists(_.contains("No loops in this project")), lines.mkString("|"))
    assert(lines.exists(_.contains("Esc")), lines.mkString("|"))
    // Enter on an empty window is inert.
    assertEquals(app.update(Event.LoopOpen, open)._1.overlay, Overlay.Loops(0))

  test("a snapshot that drops loops clamps the open window's selection instead of dangling"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(2))
    val (fewer, _) = app.update(Event.Inbound1(AgentEvent.Loops(loops(1))), st)
    assertEquals(fewer.overlay, Overlay.Loops(0))
    // Enter after the shrink opens the loop that IS there, not a stale index.
    assertEquals(app.update(Event.LoopOpen, fewer)._1.overlay, Overlay.LoopTranscript("l01", 0))
    val (none, _) = app.update(Event.Inbound1(AgentEvent.Loops(Vector.empty)), st)
    assertEquals(none.overlay, Overlay.Loops(0))
    // A snapshot landing while the window is closed leaves the overlay alone.
    val closed = ChatState.initial.copy(loops = loops(3))
    assertEquals(app.update(Event.Inbound1(AgentEvent.Loops(loops(1))), closed)._1.overlay, Overlay.None)

  test("↓ on a fresh line goes straight to the subagents — loops are not in the focus chain"):
    val app = appUI
    val both = ChatState.initial.copy(loops = loops(2), team = Vector(member("scribe")), inputHistory = Vector("a"), histNav = 1)
    assertEquals(app.update(Event.HistoryNext, both)._1.teamSel, Some(0))
    // With loops but no roster, ↓ has nothing to enter.
    val loopsOnly = both.copy(team = Vector.empty)
    assertEquals(app.update(Event.HistoryNext, loopsOnly)._1.teamSel, None)

  test("↑ off the subagent grid returns to the input even with loops live"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(2), team = Vector(member("scribe")), teamSel = Some(0))
    assertEquals(app.update(Event.TeamMove(0, -1), st)._1.teamSel, None)

  test("the spinner clock runs for a live loop and stops for a parked or orphaned one"):
    val app = appUI
    assert(hasTick(app.subscriptions(ChatState.initial.copy(loops = Vector(running())))))
    assert(!hasTick(app.subscriptions(ChatState.initial.copy(loops = Vector(parked(), orphaned())))))

  // -- the transcript overlay --------------------------------------------------------

  test("Enter opens the selected loop's transcript and Esc steps back to the window"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(1))
    val (opened, _) = app.update(Event.LoopOpen, st)
    assertEquals(opened.overlay, Overlay.LoopTranscript("l02", 0))
    val (closed, _) = app.update(Event.LoopTranscriptBack, opened)
    assertEquals(closed.overlay, Overlay.Loops(1))
    // A loop that vanished while the transcript was open lands the window at its top.
    val gone = opened.copy(loops = loops(1))
    assertEquals(app.update(Event.LoopTranscriptBack, gone)._1.overlay, Overlay.Loops(0))

  test("the overlay takes the member transcript's scroll keys and clamps both edges"):
    val app = appUI
    val open = ChatState.initial.copy(loops = loops(1), overlay = Overlay.LoopTranscript("l01", 0))
    assertEquals(keyEvent(app, open, Key.Up), Some(Event.LoopTranscriptScroll(1)))
    assertEquals(keyEvent(app, open, Key.End), Some(Event.LoopTranscriptFollow))
    assertEquals(keyEvent(app, open, Key.Esc), Some(Event.LoopTranscriptBack))
    // A tall transcript, rendered once so the update loop has the content geometry it
    // clamps the scroll against.
    val active = activity(app, open, (1 to 30).map(i => TranscriptEvent.Said("l01", "gen-4-eval", s"row $i\n\n"))*)
    fsLines(app, active, "l01")
    val (scrolled, _) = app.update(Event.LoopTranscriptScroll(3), active)
    assertEquals(scrolled.overlay, Overlay.LoopTranscript("l01", 3))
    assertEquals(app.update(Event.LoopTranscriptFollow, scrolled)._1.overlay, Overlay.LoopTranscript("l01", 0))
    assertEquals(app.update(Event.LoopTranscriptScroll(-5), active)._1.overlay, Overlay.LoopTranscript("l01", 0))
    val maxed = app.update(Event.LoopTranscriptScroll(999), active)._1.overlay.asInstanceOf[Overlay.LoopTranscript].offset
    assert(maxed > 3, s"expected scrollable content, offset capped at $maxed")

  test("the overlay reads the live agent's transcript and follows the loop from worker to evaluator"):
    val app = appUI
    val working = running(activity = "gen 4, attempt 1 — working", label = "gen-4-worker")
    val st = ChatState.initial.copy(loops = Vector(working))
    val fed = activity(
      app,
      st,
      TranscriptEvent.Said("perf", "gen-4-worker", "measuring the hot path"),
      TranscriptEvent.Said("perf", "gen-4-eval", "reading the diff")
    )
    val onWorker = fsLines(app, fed, "perf")
    assert(onWorker.exists(_.contains("measuring the hot path")), onWorker.mkString("|"))
    assert(!onWorker.exists(_.contains("reading the diff")), onWorker.mkString("|"))
    // The overlay stores only the loop id, so the same open view moves with the stage.
    val evaluating = fed.copy(loops = Vector(running()))
    val onEval = fsLines(app, evaluating, "perf")
    assert(onEval.exists(_.contains("reading the diff")), onEval.mkString("|"))

  test("the overlay header names the loop, its goal, its stage and everything it has measured"):
    val app = appUI
    val v = running(generations = Vector(accepted(1, 90), gen(2, LoopGenerationState.Accepted, "allocMb" -> 12.0, "p99Ms" -> 70.0)))
    val header = fsLines(app, ChatState.initial.copy(loops = Vector(v)), "perf", width = 110)(1)
    assert(header.contains("perf") && header.contains("cut p99 latency"), header)
    assert(header.contains("gen 4, attempt 1 — evaluating"), header)
    assert(header.contains("allocMb 12 · p99Ms 70"), header)

  test("a loop with no agent running says so rather than showing an empty transcript"):
    val app = appUI
    val lines = fsLines(app, ChatState.initial.copy(loops = Vector(parked())), "old")
    assert(lines.exists(_.contains("no agent running for this loop")), lines.mkString("|"))
    assert(lines.exists(_.contains("parked: budget exhausted")), lines.mkString("|"))

  test("a loop that has gone leaves the overlay with a way back"):
    val app = appUI
    val lines = fsLines(app, ChatState.initial.copy(loops = loops(1)), "vanished")
    assert(lines.exists(_.contains("This loop is gone")), lines.mkString("|"))
    assert(lines.exists(_.contains("Esc")), lines.mkString("|"))

  test("the working tail names the stage instead of a token count"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running()))
    val fed = activity(app, st, TranscriptEvent.Said("perf", "gen-4-eval", "reading the diff"))
    val lines = fsLines(app, fed, "perf")
    assert(lines.exists(l => l.contains("Working") && l.contains("gen 4, attempt 1 — evaluating")), lines.mkString("|"))
