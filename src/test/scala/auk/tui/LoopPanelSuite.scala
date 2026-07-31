package auk.tui

import auk.tui.app.{Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, LoopGenerationState, LoopGenerationView, LoopView, TeamMemberView, UserCommand}
import auk.workflow.TranscriptEvent

/** The refinement-loop panel: the row rendering below the prompt box, the focus chain
  * it shares with the subagent panel, and the fullscreen loop transcript. */
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

  /** A loop mid-generation with a lineage behind it — the panel's ordinary case. */
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

  // -- the panel --------------------------------------------------------------------

  test("a Loops snapshot folds into state and the panel docks between the footer and the subagents"):
    val app = appUI
    val withTeam = ChatState.initial.copy(team = Vector(member("scribe")))
    val (st, _) = app.update(Event.Inbound1(AgentEvent.Loops(Vector(running()))), withTeam)
    assertEquals(st.loops.map(_.id), Vector("perf"))
    val lines = liveLines(app, st)
    val footer = lines.indexWhere(_.contains("ctrl+c or / for commands"))
    val loopFrame = lines.indexWhere(_.contains("╭─ loops"))
    val loopRow = lines.indexWhere(_.contains("perf"))
    val teamFrame = lines.indexWhere(_.contains("╭─ subagents"))
    assert(footer >= 0 && loopFrame > footer, lines.mkString("|"))
    assert(loopRow > loopFrame && teamFrame > loopRow, lines.mkString("|"))

  test("no panel renders while there are no loops"):
    val lines = liveLines(appUI, ChatState.initial)
    assert(!lines.exists(_.contains("loops")), lines.mkString("|"))

  test("a running loop shows its lineage strip, its headline metric with a trend, and its stage"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running()))
    val row = liveLines(app, st).find(_.contains("perf")).getOrElse(fail("the loop's row is missing"))
    // Every generation number it spent, accepted and abandoned alike, then the one running.
    assert(row.contains("✓1 ✗2 ✓3 ⋯4"), row)
    // The newest acceptance measured 70 against the 90 before it: the number fell.
    assert(row.contains("p99Ms 70 ↓"), row)
    assert(row.contains("gen 4, attempt 1 — evaluating"), row)

  test("the trend arrow reports direction only, and is absent with nothing to compare"):
    val app = appUI
    val rose = running(generations = Vector(accepted(1, 70), accepted(2, 90)))
    val first = running(generations = Vector(accepted(1, 70)))
    val flat = running(generations = Vector(accepted(1, 70), accepted(2, 70)))
    def row(v: LoopView): String =
      liveLines(app, ChatState.initial.copy(loops = Vector(v))).find(_.contains("perf")).getOrElse("")
    assert(row(rose).contains("p99Ms 90 ↑"), row(rose))
    assert(row(first).contains("p99Ms 70") && !row(first).contains("↓") && !row(first).contains("↑"), row(first))
    assert(row(flat).contains("p99Ms 70") && !row(flat).contains("↑"), row(flat))

  test("a parked loop and an orphaned one read differently from a running one"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(parked(), orphaned()))
    val lines = liveLines(app, st)
    val park = lines.find(_.contains("old")).getOrElse(fail("the parked loop's row is missing"))
    val stray = lines.find(_.contains("stray")).getOrElse(fail("the orphaned loop's row is missing"))
    assert(park.contains("×") && park.contains("parked: budget exhausted"), park)
    assert(stray.contains("⚠") && stray.contains("orphaned (dead session)"), stray)
    // Neither counts as running in the frame's meta; both are waiting to be picked up.
    assert(lines.exists(_.contains("2 waiting")), lines.mkString("|"))

  test("the frame's meta counts what is running and names the mode's keys"):
    val app = appUI
    val ambient = ChatState.initial.copy(loops = Vector(running(), parked()))
    val top = liveLines(app, ambient).find(_.contains("╭─ loops")).getOrElse("")
    assert(top.contains("1 running") && top.contains("1 waiting") && top.contains("↓ browse"), top)
    val focused = ambient.copy(loopSel = Some(0))
    val topFocused = liveLines(app, focused).find(_.contains("╭─ loops")).getOrElse("")
    assert(topFocused.contains("enter open") && topFocused.contains("esc back"), topFocused)

  test("a long lineage is truncated from the left, keeping the newest generations"):
    val app = appUI
    val long = running(generations = (1 to 12).toVector.map(i => accepted(i, 100.0 - i)))
    val row = liveLines(app, ChatState.initial.copy(loops = Vector(long))).find(_.contains("perf")).getOrElse("")
    assert(row.contains("✓12") && row.contains("⋯13"), row)
    assert(row.contains("…") && !row.contains("✓1 ✓2"), row)

  test("a narrow terminal sheds the metric and then the strip, never the name or the stage"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running()))
    val row = liveLines(app, st, width = 46).find(_.contains("perf")).getOrElse(fail("the loop's row is missing"))
    assert(!row.contains("✓1") && !row.contains("p99Ms"), row)
    assert(row.contains("gen 4"), row)

  test("the selected row renders in one inverted style and the overflow window follows it"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(6), loopSel = Some(0))
    val shown = liveLines(app, st).filter(l => l.contains("l0"))
    // Capped at four rows, showing from the top while the selection is there.
    assertEquals(shown.length, 4)
    assert(shown.head.contains("l01") && shown.last.contains("l04"), shown.mkString("|"))
    val scrolled = st.copy(loopSel = Some(5), loopScroll = 2)
    val after = liveLines(app, scrolled).filter(_.contains("l0"))
    assert(after.head.contains("l03") && after.last.contains("l06"), after.mkString("|"))
    assert(liveLines(app, scrolled).exists(_.contains("3-6/6")), "the frame reports where the window sits")

  // -- focus and keys ---------------------------------------------------------------

  test("↓ on a fresh line focuses the loop panel first; without loops it still finds the subagents"):
    val app = appUI
    val both = ChatState.initial.copy(loops = loops(2), team = Vector(member("scribe")), inputHistory = Vector("a"), histNav = 1)
    val (focused, _) = app.update(Event.HistoryNext, both)
    assertEquals(focused.loopSel, Some(0))
    assertEquals(focused.teamSel, None)
    val teamOnly = both.copy(loops = Vector.empty)
    assertEquals(app.update(Event.HistoryNext, teamOnly)._1.teamSel, Some(0))
    val neither = both.copy(loops = Vector.empty, team = Vector.empty)
    assertEquals(app.update(Event.HistoryNext, neither)._1.loopSel, None)

  test("focused panel keys: ↑/↓ move, Enter opens, Esc exits, typing falls through"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(2), loopSel = Some(0))
    assertEquals(keyEvent(app, st, Key.Down), Some(Event.LoopMove(1)))
    assertEquals(keyEvent(app, st, Key.Up), Some(Event.LoopMove(-1)))
    assertEquals(keyEvent(app, st, Key.Enter), Some(Event.LoopOpen))
    assertEquals(keyEvent(app, st, Key.Esc), Some(Event.LoopExit))
    assertEquals(keyEvent(app, st, Key.Char('x')), Some(Event.KeyChar('x')))

  test("an editing key drops the panel focus so typing resumes without an explicit Esc"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(2), loopSel = Some(1))
    val (typed, _) = app.update(Event.KeyChar('h'), st)
    assertEquals(typed.loopSel, None)
    assertEquals(typed.input, "h")

  test("the focus chain runs input → loops → subagents and back again"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(2), team = Vector(member("scribe")), loopSel = Some(0))
    // ↓ steps within the loop panel, then hands focus down to the subagents.
    val (second, _) = app.update(Event.LoopMove(1), st)
    assertEquals(second.loopSel, Some(1))
    val (handed, _) = app.update(Event.LoopMove(1), second)
    assertEquals(handed.loopSel, None)
    assertEquals(handed.teamSel, Some(0))
    // ↑ off the top of the subagent grid comes back to the loop panel's LAST row.
    val (back, _) = app.update(Event.TeamMove(0, -1), handed)
    assertEquals(back.teamSel, None)
    assertEquals(back.loopSel, Some(1))
    // ↑ off the top of the loop panel returns to the input box.
    val (top, _) = app.update(Event.LoopMove(-1), back.copy(loopSel = Some(0)))
    assertEquals(top.loopSel, None)
    assertEquals(top.teamSel, None)

  test("without a subagent panel below, ↓ from the last loop stays put"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(2), loopSel = Some(1))
    assertEquals(app.update(Event.LoopMove(1), st)._1.loopSel, Some(1))

  test("↑ off the subagent grid returns to the input when there are no loops"):
    val app = appUI
    val st = ChatState.initial.copy(team = Vector(member("scribe")), teamSel = Some(0))
    val (left, _) = app.update(Event.TeamMove(0, -1), st)
    assertEquals(left.teamSel, None)
    assertEquals(left.loopSel, None)

  test("a snapshot that drops loops clamps a browsing selection instead of dangling"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), loopSel = Some(2))
    val (fewer, _) = app.update(Event.Inbound1(AgentEvent.Loops(loops(1))), st)
    assertEquals(fewer.loopSel, Some(0))
    val (none, _) = app.update(Event.Inbound1(AgentEvent.Loops(Vector.empty)), st)
    assertEquals(none.loopSel, None)

  test("the spinner clock runs for a live loop and stops for a parked or orphaned one"):
    val app = appUI
    assert(hasTick(app.subscriptions(ChatState.initial.copy(loops = Vector(running())))))
    assert(!hasTick(app.subscriptions(ChatState.initial.copy(loops = Vector(parked(), orphaned())))))

  // -- the transcript overlay --------------------------------------------------------

  test("Enter opens the selected loop's transcript and Esc restores the panel focus"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), loopSel = Some(1))
    val (opened, _) = app.update(Event.LoopOpen, st)
    assertEquals(opened.overlay, Overlay.LoopTranscript("l02", 0))
    val (closed, _) = app.update(Event.LoopTranscriptBack, opened)
    assertEquals(closed.overlay, Overlay.None)
    assertEquals(closed.loopSel, Some(1))

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
