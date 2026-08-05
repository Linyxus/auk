package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, LoopGenerationState, LoopGenerationView, LoopStage, LoopView, TeamMemberView, UserCommand}
import auk.workflow.{Forest, OrchestrationEvent, RunStatus, Transcript, TranscriptEvent}

/** The refinement loops in the TUI: their segment of the ONE activity line that stands
  * in for everything running in the background, the on-demand `ctrl+c l` window that
  * lists them, and the fullscreen loop transcript opened from it. */
class LoopPanelSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

  /** The same app driving the alt-screen chat, whose bottom stack carries the same
    * activity line. */
  private def appFullscreen: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      mode = DisplayMode.Fullscreen
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
      orphaned = false,
      // Only the session driving a loop ever reports it as running: a loop read off
      // disk is parked or orphaned, never running.
      held = true
    )

  /** A parked loop. `held` is the whole question about one: a loop THIS session drove
    * to a stop is still its business, while one parked by somebody else is a stranger
    * the window may list and the activity line must ignore — which is the common case
    * below, so it is the default. */
  private def parked(id: String = "old", reason: String = "budget exhausted", held: Boolean = false): LoopView =
    LoopView(id, s"parked: $reason", "cut p99 latency", Vector(accepted(1, 90)), None, None, Some(reason), false, held)

  /** An orphan is always somebody else's: orphaned is the phase of a loop found on
    * disk with nobody driving it. */
  private def orphaned(id: String = "stray"): LoopView =
    LoopView(id, "orphaned (dead session)", "cut p99 latency", Vector(accepted(1, 90)), None, None, None, true, false)

  private def loops(n: Int): Vector[LoopView] =
    (1 to n).toVector.map(i => running(id = f"l$i%02d"))

  /** A generation with a spend on it. Every figure below is stated rather than derived:
    * the host adds a loop and its generations up before the view leaves it, and a panel
    * that did its own arithmetic would be the bug these tests are watching for. */
  private def priced(g: LoopGenerationView, input: Long, output: Long): LoopGenerationView =
    g.copy(inputTokens = input, outputTokens = output)

  private def spent(v: LoopView, input: Long, output: Long): LoopView =
    v.copy(inputTokens = input, outputTokens = output)

  /** A loop mid-step, with the run behind that step counted as far as it has got. The
    * prose and the structure say the same thing, as the host sends them. */
  private def onStage(v: LoopView, step: String, input: Long, output: Long): LoopView =
    v.copy(activity = Some(s"gen 4, attempt 1 — $step"), stage = Some(LoopStage(4, 1, step, input, output)))

  /** A lineage that has been counting: 12.3k spent, then 9.1k thrown away, then 4.2k so
    * far on the generation in flight. */
  private def countedGens: Vector[LoopGenerationView] = Vector(
    priced(accepted(1, 90), 9_000, 3_300),
    priced(abandoned(2), 6_000, 3_100),
    priced(gen(3, LoopGenerationState.Running), 3_000, 1_200)
  )

  private def counted: LoopView =
    spent(running().copy(generations = countedGens), 18_000, 7_600)

  private def member(id: String): TeamMemberView =
    TeamMemberView(id, s"$id desc", working = false, inputTokens = 0, outputTokens = 0)

  /** A one-node workflow run settled (or paused) into `status` by its terminal event —
    * the other half of the activity line. */
  private def run(runId: String, status: RunStatus): (String, Forest) =
    import OrchestrationEvent.*
    val started = Forest.empty.update(NodeDeclared(runId, "n", None, Nil)).update(NodeStarted(runId, "n", "go"))
    val settled = status match
      case RunStatus.Running => started
      case RunStatus.Paused  => started.update(WorkflowPaused(runId))
      case RunStatus.Done    => started.update(WorkflowFinished(runId, true, "ok"))
      case RunStatus.Failed  => started.update(WorkflowFinished(runId, false, "boom"))
    runId -> settled

  private def liveLines(app: ChatApp, state: ChatState, width: Int = 120): Vector[String] =
    Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)

  /** The activity line, as the bottom chrome renders it (there is at most one). */
  private def activityLine(app: ChatApp, state: ChatState, width: Int = 120): Option[String] =
    liveLines(app, state, width).find(l => l.contains(" loop") || l.contains(" workflow"))

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

  // -- the activity line -------------------------------------------------------------

  test("a Loops snapshot folds into state and shows as ONE line above the input box"):
    val app = appUI
    val withTeam = ChatState.initial.copy(team = Vector(member("scribe")))
    val (st, _) = app.update(Event.Inbound1(AgentEvent.Loops(Vector(running()))), withTeam)
    assertEquals(st.loops.map(_.id), Vector("perf"))
    val lines = liveLines(app, st)
    val hint = lines.indexWhere(_.contains("ctrl+c l to view"))
    val footer = lines.indexWhere(_.contains("ctrl+c or / for commands"))
    val teamFrame = lines.indexWhere(_.contains("╭─ subagents"))
    // The loops share the workflow line's slot, above the prompt and the footer.
    assert(hint >= 0 && footer > hint, lines.mkString("|"))
    assert(teamFrame > footer, lines.mkString("|"))
    // Exactly one line: the loops occupy a segment of a row, not a panel.
    assertEquals(lines.count(_.contains("ctrl+c l to view")), 1, lines.mkString("|"))

  test("the always-on panel is gone: the bottom chrome carries no framed rows, however many loops"):
    val app = appUI
    val lines = liveLines(app, ChatState.initial.copy(loops = loops(6), team = Vector(member("scribe"))))
    assert(!lines.exists(_.contains("╭─ loops")), lines.mkString("|"))
    // Six loops, one line, and not one of them named: no per-loop row, no lineage
    // strip, no headline metric — the window has all of it.
    assertEquals(lines.count(_.contains("6 loops running")), 1, lines.mkString("|"))
    assert(!lines.exists(_.contains("l0")), lines.mkString("|"))
    assert(!lines.exists(_.contains("✓1")), lines.mkString("|"))
    assert(!lines.exists(_.contains("p99Ms")), lines.mkString("|"))
    // The subagent panel is untouched by any of it.
    assert(lines.exists(_.contains("╭─ subagents")), lines.mkString("|"))

  test("no line at all while there is nothing running"):
    val lines = liveLines(appUI, ChatState.initial)
    assert(!lines.exists(_.contains("loop")), lines.mkString("|"))
    assert(!lines.exists(_.contains("workflow")), lines.mkString("|"))

  test("loops found on disk contribute nothing: a project holding only those shows NO line"):
    val app = appUI
    // The user's own ask: opening a session on a project full of loops nobody is
    // driving must be as quiet as opening one on a project with none. None of these
    // is held — this session has touched none of them — which is what keeps the line
    // away however many the project has.
    val st = ChatState.initial.copy(loops = Vector(parked(), orphaned(), parked(id = "older", reason = "user requested")))
    val lines = liveLines(app, st)
    assertEquals(activityLine(app, st), None, lines.mkString("|"))
    assert(!lines.exists(_.contains("loop")), lines.mkString("|"))
    assert(!lines.exists(_.contains("ctrl+c l to view")), lines.mkString("|"))
    // Same for the fullscreen chat's bottom stack, which carries the same line.
    val fs = Layout.lay(appFullscreen.view(st, Viewport(120, 30)).fullscreen.getOrElse(fail("no frame")), 120)
      .map(_.plain)
    assert(!fs.exists(_.contains(" loop")), fs.mkString("|"))

  test("loops alone read as the loop tide, the running count, and the loop chord"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running()))
    val line = activityLine(app, st).getOrElse(fail("no activity line"))
    assertEquals(line.trim, "⣀ 1 loop running · ctrl+c l to view")
    // No workflow vocabulary anywhere: there are no workflows to open.
    assert(!line.contains("ctrl+c w"), line)
    assert(!line.contains("dashboard"), line)
    // The tide glyph animates off the render clock, as the old loop line's did.
    assertNotEquals(activityLine(app, st.copy(clockMs = 0L)), activityLine(app, st.copy(clockMs = 300L)))

  test("a loop this session drove stays on the line once it parks, under a static glyph"):
    val app = appUI
    // Parking is where a loop the user asked for ends up, and the line it was on is
    // where they last saw it: it stays, said plainly, until the session is over.
    val st = ChatState.initial.copy(loops = Vector(parked(held = true)))
    val line = activityLine(app, st).getOrElse(fail("no activity line"))
    assertEquals(line.trim, "◆ 1 loop parked · ctrl+c l to view")
    // Nothing is running, so nothing animates — the tide is for live work.
    assertEquals(activityLine(app, st.copy(clockMs = 0L)), activityLine(app, st.copy(clockMs = 300L)))
    // A loop parked by somebody else, sitting in the same project, is still not counted.
    val strangers = st.copy(loops = st.loops ++ Vector(parked(id = "other"), orphaned()))
    assertEquals(activityLine(app, strangers).map(_.trim), Some("◆ 1 loop parked · ctrl+c l to view"))

  test("running and held-parked loops read as two segments, the noun carried by the first"):
    val app = appUI
    def line(vs: LoopView*): String =
      activityLine(app, ChatState.initial.copy(loops = vs.toVector)).getOrElse(fail("no activity line"))
    assertEquals(
      line(running("a"), running("b"), parked(held = true), orphaned()).trim,
      "⣀ 2 loops running · 1 parked · ctrl+c l to view"
    )
    // The count decides the noun once, on the leading segment; the second inherits it.
    assertEquals(
      line(running("a"), parked(held = true), parked(id = "p2", held = true)).trim,
      "⣀ 1 loop running · 2 parked · ctrl+c l to view"
    )
    // Beside a workflow the hint names both chords, as it does for a running loop.
    val withWorkflow = ChatState.initial.copy(
      activeWorkflows = Vector(run("r1", RunStatus.Running)),
      loops = Vector(parked(held = true))
    )
    assertEquals(
      activityLine(app, withWorkflow).map(_.trim.drop(2)),
      Some("1 workflow running · 1 loop parked · ctrl+c w / ctrl+c l to view · ctrl+c w o opens the live dashboard")
    )

  test("the line carries counts only — no loop is named and no stage is quoted"):
    val app = appUI
    val line = activityLine(app, ChatState.initial.copy(loops = Vector(running()))).getOrElse(fail("no line"))
    assert(!line.contains("'perf'"), line)
    assert(!line.contains("evaluating"), line)

  test("workflows with no live loop read exactly as they always did"):
    val app = appUI
    // The parked and orphaned loops beside them change nothing.
    val st = ChatState.initial.copy(
      activeWorkflows = Vector(run("r1", RunStatus.Running)),
      loops = Vector(parked(), orphaned())
    )
    val line = activityLine(app, st).getOrElse(fail("no activity line"))
    assertEquals(line.trim.drop(2), "1 workflow running · ctrl+c w to view · ctrl+c w o opens the live dashboard")
    assert(!line.contains("ctrl+c l"), line)

  test("workflows and loops share the one line, with a hint naming both chords"):
    val app = appUI
    val st = ChatState.initial.copy(
      activeWorkflows = Vector(run("r1", RunStatus.Running), run("r2", RunStatus.Running), run("r3", RunStatus.Failed)),
      loops = Vector(running(), parked())
    )
    val line = activityLine(app, st).getOrElse(fail("no activity line"))
    assertEquals(
      line.trim.drop(2),
      "2 workflows running · 1 failed · 1 loop running · ctrl+c w / ctrl+c l to view · ctrl+c w o opens the live dashboard"
    )
    // The workflow spinner wins the glyph while a run is going; the loop-only tide
    // does not get a look in.
    assertNotEquals(activityLine(app, st.copy(clockMs = 0L)), activityLine(app, st.copy(clockMs = 500L)))
    assertEquals(liveLines(app, st).count(_.contains("loop running")), 1, liveLines(app, st).mkString("|"))

  test("the loop count says one loop, N loops, and counts only the ones running"):
    val app = appUI
    def line(vs: LoopView*): String =
      activityLine(app, ChatState.initial.copy(loops = vs.toVector)).getOrElse(fail("no activity line"))
    assert(line(running()).contains("1 loop running · "), line(running()))
    assert(line(running("a"), running("b")).contains("2 loops running"), line(running("a"), running("b")))
    val mixed = line(parked(), running(), orphaned(), parked(id = "older"))
    assert(mixed.contains("1 loop running") && !mixed.contains("4 loops"), mixed)

  test("the loop-only line is exactly one row, never wider than the terminal, at any width"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(running(id = "a-rather-long-loop-name")))
    for width <- Vector(24, 30, 40, 56, 72, 100, 160) do
      val lines = liveLines(app, st, width)
      val hits = lines.filter(l => l.contains(" loop") && !l.contains("ctrl+c or /"))
      assertEquals(hits.length, 1, s"width $width: ${lines.mkString("|")}")
      assert(hits.head.length <= width, s"width $width overflowed: '${hits.head}'")
      // The census never goes; the hint is what a narrow terminal sheds.
      assert(hits.head.contains("1 loop running"), s"width $width: '${hits.head}'")

  test("a narrow terminal sheds the dashboard hint, then the chords, never the census"):
    val app = appUI
    val st = ChatState.initial.copy(activeWorkflows = Vector(run("r1", RunStatus.Running)), loops = Vector(running()))
    def at(width: Int): String =
      liveLines(app, st, width).find(_.contains(" loop running")).getOrElse(fail(s"no activity line at width $width"))
    assert(at(160).contains("ctrl+c w / ctrl+c l to view") && at(160).contains("opens the live dashboard"), at(160))
    // The dashboard goes first…
    assert(at(100).contains("ctrl+c w / ctrl+c l to view") && !at(100).contains("dashboard"), at(100))
    // …then the chords, and the census is what is left.
    assert(!at(56).contains("ctrl+c"), at(56))
    assert(at(56).contains("1 workflow running · 1 loop running"), at(56))
    for width <- Vector(56, 72, 100, 160) do
      val hits = liveLines(app, st, width).filter(_.contains(" loop running"))
      assertEquals(hits.length, 1, s"width $width")
      assert(hits.head.length <= width, s"width $width overflowed: '${hits.head}'")

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

  test("the window row carries what the loop has spent, as one figure in its own column"):
    val app = appUI
    def rowOf(v: LoopView): String =
      windowLines(app, ChatState.initial.copy(loops = Vector(v)), width = 110)
        .find(_.contains("perf"))
        .getOrElse(fail("the loop's row is missing"))
    // Input and output as one number, not two: 9k asked and 3.3k answered is 12.3k spent.
    val row = rowOf(spent(running(), 9_000, 3_300))
    assert(row.contains("12.3k"), row)
    assert(!row.contains("9.0k") && !row.contains("3.3k"), row)
    // A column of its own, between the metric and the stage — not a suffix on either.
    assert(row.indexOf("12.3k") > row.indexOf("p99Ms"), row)
    assert(row.indexOf("12.3k") < row.indexOf("gen 4"), row)
    // A loop from before any of this was counted shows a blank there rather than a `0`,
    // and the column stays open so the rows still line up beside it.
    val bare = rowOf(running())
    assert(!bare.contains("12.3k") && !bare.contains(" 0 "), bare)
    assertEquals(bare.indexOf("gen 4"), row.indexOf("gen 4"), s"'$bare' / '$row'")

  test("the spend column sheds before the headline metric does"):
    val app = appUI
    val st = ChatState.initial.copy(loops = Vector(spent(running(), 9_000, 3_300)))
    def at(width: Int): String =
      windowLines(app, st, width = width).find(_.contains("perf")).getOrElse(fail(s"no row at width $width"))
    assert(at(110).contains("12.3k") && at(110).contains("p99Ms 70"), at(110))
    // The metric is the decision and the spend is the receipt, so the receipt goes first
    // — and the lineage and the stage, which neither of them can replace, stay.
    assert(!at(82).contains("12.3k"), at(82))
    assert(at(82).contains("p99Ms 70") && at(82).contains("✓1") && at(82).contains("gen 4"), at(82))

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

  // -- `o`: the window's loop, in the browser ------------------------------------------

  test("`o` opens the dashboard on the SELECTED loop once its URL is known"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(1))
    assertEquals(keyEvent(app, st, Key.Char('o')), Some(Event.LoopOpenDashboard))
    // The page's key legend advertises the binding, in the workflow menu's words.
    assert(windowLines(app, st, selected = 1).exists(_.contains("o dashboard")), "the footer names the key")
    val ready = st.copy(dashboardUrl = Some("http://localhost:7777"))
    // The fragment names the loop under the cursor — not the first, not the last.
    assertEquals(ready.loopDashboardTarget("l02"), Some("http://localhost:7777/#loop/l02"))
    assertEquals(ready.selectedLoopId, Some("l02"))
    val (opened, cmd) = app.update(Event.LoopOpenDashboard, ready)
    // Opening a window changes nothing about the session — not even the intent, which
    // is for the case where there was no URL to open.
    assertEquals(opened, ready)
    assertEquals(opened.pendingLoopDashboard, None)
    cmd match
      case Cmd.Fire(_) => ()
      case other       => fail(s"expected Cmd.Fire, got $other")

  test("`o` with no URL yet asks the host for a dashboard and remembers the loop"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(2))
    // A window full of loops nobody is driving has never started a server, so the key
    // is a request rather than a no-op: it fires the capability and holds the loop.
    var asked = 0
    val asking = ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      requestDashboard = () => asked += 1
    )
    val (waiting, cmd) = asking.update(Event.LoopOpenDashboard, st)
    assertEquals(waiting.pendingLoopDashboard, Some("l03"))
    cmd match
      case Cmd.Fire(eff) => eff()
      case other         => fail(s"expected Cmd.Fire, got $other")
    assertEquals(asked, 1)
    // The URL lands as its own event, and THAT is what opens the browser — at the loop
    // the key was pressed on, with the intent spent so it can never open twice.
    val (ready, open) = app.update(Event.Inbound1(AgentEvent.Dashboard("http://localhost:7777/")), waiting)
    assertEquals(ready.dashboardUrl, Some("http://localhost:7777/"))
    assertEquals(ready.pendingLoopDashboard, None)
    assertEquals(ready.loopDashboardTarget("l03"), Some("http://localhost:7777/#loop/l03"))
    open match
      case Cmd.Fire(_) => ()
      case other       => fail(s"expected Cmd.Fire, got $other")

  test("a second `o` before the URL arrives moves the intent rather than queueing a window"):
    val app = appUI
    val st = ChatState.initial.copy(loops = loops(3), overlay = Overlay.Loops(0))
    val (first, _) = app.update(Event.LoopOpenDashboard, st)
    assertEquals(first.pendingLoopDashboard, Some("l01"))
    val (moved, _) = app.update(Event.LoopsDown, first)
    val (second, _) = app.update(Event.LoopOpenDashboard, moved)
    assertEquals(second.pendingLoopDashboard, Some("l02"))
    // One URL, one window: the loop asked for last is the one that opens.
    val (ready, open) = app.update(Event.Inbound1(AgentEvent.Dashboard("http://localhost:7777")), second)
    assertEquals(ready.loopDashboardTarget("l02"), Some("http://localhost:7777/#loop/l02"))
    open match
      case Cmd.Fire(_) => ()
      case other       => fail(s"expected Cmd.Fire, got $other")

  test("a dashboard nobody asked to see opens no window"):
    val app = appUI
    // The server comes up for its own reasons — a workflow started, say — while the
    // loops window is merely open. Storing the URL is the whole of the response.
    val st = ChatState.initial.copy(loops = loops(2), overlay = Overlay.Loops(0))
    val (ready, cmd) = app.update(Event.Inbound1(AgentEvent.Dashboard("http://localhost:7777")), st)
    assertEquals(ready.dashboardUrl, Some("http://localhost:7777"))
    assertEquals(cmd, Cmd.none)
    assert(ready.notices.isEmpty, ready.notices.mkString("|"))

  test("`o` is inert with no loop under the cursor, and with no dashboard to be had"):
    val app = appUI
    // An empty window: nothing to focus, so nothing is asked for and nothing is held.
    val empty = ChatState.initial.copy(overlay = Overlay.Loops(0))
    assertEquals(empty.selectedLoopId, None)
    assertEquals(app.update(Event.LoopOpenDashboard, empty), (empty, Cmd.none))
    // AUK_NO_DASHBOARD makes `requestDashboard` a no-op, so no URL ever arrives. The
    // intent is left standing and comes due never — it opens nothing by itself, which
    // is what keeps a disabled dashboard from leaking a window into a later state.
    val disabled = ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )
    val st = ChatState.initial.copy(loops = loops(1), overlay = Overlay.Loops(0))
    val (waiting, cmd) = disabled.update(Event.LoopOpenDashboard, st)
    assertEquals(waiting.pendingLoopDashboard, Some("l01"))
    cmd match
      case Cmd.Fire(eff) => eff() // the default capability: does nothing, raises nothing
      case other         => fail(s"expected Cmd.Fire, got $other")
    assertEquals(waiting.loopDashboardTarget("l01"), None)
    // Every other event still passes through without opening anything.
    assertEquals(disabled.update(Event.Inbound1(AgentEvent.Loops(loops(1))), waiting)._2, Cmd.none)

  test("the workflow page's `o` is untouched by the loops one"):
    val app = appUI
    val forest = Forest.empty.update(OrchestrationEvent.NodeDeclared("r", "a", None, Nil))
    val wf = ChatState.initial
      .copy(activeWorkflows = Vector("r" -> forest), overlay = Overlay.WorkflowList(0), dashboardUrl = Some("http://localhost:7777"))
    assertEquals(keyEvent(app, wf, Key.Char('o')), Some(Event.WorkflowOpenDashboard))
    // Still the bare run fragment, with no `loop/` segment in it.
    assertEquals(wf.dashboardTarget, Some("http://localhost:7777/#r"))

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
    // Including one this session parked itself: it keeps its place on the line, but a
    // stopped loop has nothing to animate and does not hold the clock awake.
    assert(!hasTick(app.subscriptions(ChatState.initial.copy(loops = Vector(parked(held = true))))))

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

  test("a loop agent's running eval carries both clocks, like every other transcript's"):
    // The loop's view renders through the same pipeline the team panel uses, so
    // this is the third surface reading one definition rather than a third copy.
    val app = appUI
    val called = Transcript.empty.update(TranscriptEvent.ToolCalled("perf", "gen-4-eval", "c1", "eval_scala", """{"code":"1 + 1"}"""))
    val progress = Map("replElapsedMs" -> "12400", "replPhase" -> "running", "replPhaseMs" -> "900")
    val t = called.update(TranscriptEvent.ToolProgressed("perf", "gen-4-eval", "c1", progress), Some(1_000L))
    def rowAt(clockMs: Long): String =
      val st = ChatState.initial.copy(
        loops = Vector(running()),
        transcripts = Map(("perf", "gen-4-eval") -> t),
        clockMs = clockMs
      )
      val fs = fsLines(app, st, "perf")
      fs.find(_.contains("Executing code")).getOrElse(fail(s"no eval row: ${fs.mkString("|")}"))
    assert(rowAt(1_400L).contains("Executing code (12.8s) · Running (1.3s)"), rowAt(1_400L))
    // And it ticks between the worker's reports.
    assert(rowAt(2_400L).contains("Executing code (13.8s) · Running (2.3s)"), rowAt(2_400L))

  test("the overlay header names the loop, its goal, its stage and everything it has measured"):
    val app = appUI
    val v = running(generations = Vector(accepted(1, 90), gen(2, LoopGenerationState.Accepted, "allocMb" -> 12.0, "p99Ms" -> 70.0)))
    val header = fsLines(app, ChatState.initial.copy(loops = Vector(v)), "perf", width = 110)(1)
    assert(header.contains("perf") && header.contains("cut p99 latency"), header)
    assert(header.contains("gen 4, attempt 1 — evaluating"), header)
    assert(header.contains("allocMb 12 · p99Ms 70"), header)

  test("the header prices the agent on screen by the job it is doing, and the loop by everything"):
    val app = appUI
    def header(v: LoopView, width: Int = 120): String =
      fsLines(app, ChatState.initial.copy(loops = Vector(v)), v.id, width = width)(1)
    val base = spent(running(), 40_000, 5_000)
    // 3k asked and 400 answered is 3.4k spent on the run behind this step alone.
    val working = header(onStage(base, "working", 3_000, 400))
    assert(working.contains("worker 3.4k tokens"), working)
    val evaluating = header(onStage(base, "evaluating", 3_000, 400))
    assert(evaluating.contains("evaluator 3.4k tokens"), evaluating)
    assert(evaluating.contains("45.0k total"), evaluating)
    // A check is the loop's own Scala running in the gate worker — it asks no model
    // anything — so there is no figure, and no role left standing over an empty one.
    val checking = header(onStage(base, "checking", 0, 0))
    assert(!checking.contains("worker") && !checking.contains("evaluator"), checking)
    assert(checking.contains("45.0k total"), checking)
    // Between generations there is no agent to hang a figure on; the loop's own total
    // is not tied to one and stays.
    val between = header(base.copy(stage = None))
    assert(!between.contains("tokens"), between)
    assert(between.contains("45.0k total"), between)
    // A loop nobody has counted says nothing at all rather than owning up to zero.
    assert(!header(running()).contains("total"), header(running()))

  test("a narrow header drops the two figures before the stage or the measurements"):
    val app = appUI
    val v = onStage(spent(running(), 40_000, 5_000), "evaluating", 3_000, 400)
    def header(width: Int): String =
      fsLines(app, ChatState.initial.copy(loops = Vector(v)), "perf", width = width)(1)
    assert(header(100).contains("evaluator 3.4k tokens") && header(100).contains("45.0k total"), header(100))
    // The total goes first — the loops window carries that same number a keystroke away.
    assert(!header(84).contains("45.0k total") && header(84).contains("evaluator 3.4k tokens"), header(84))
    // Then the live figure, and what the header was for before any of this existed is
    // what a narrow terminal is left with.
    val narrow = header(70)
    assert(!narrow.contains("tokens") && !narrow.contains("total"), narrow)
    assert(narrow.contains("gen 4, attempt 1 — evaluating") && narrow.contains("p99Ms 70"), narrow)
    // The loop's own name survives all of it, at every width: it is what the reader
    // opened, and a right side long enough to crowd it off the bar sheds instead. The
    // goal after it is prose, and prose was always the first thing the bar cut.
    for w <- Vector(60, 70, 76, 84, 90, 100, 120) do
      val h = header(w)
      assert(h.contains("perf") && !h.contains("per…"), s"width $w: '$h'")

  test("a band under the header prices the lineage, keeping the newest generations"):
    val app = appUI
    val lines = fsLines(app, ChatState.initial.copy(loops = Vector(counted)), "perf", width = 120)
    // Directly under the header bar, which sits under the frame's top pad row.
    assert(lines(1).contains("perf"), lines(1))
    assert(lines(2).contains("✓1 12.3k ✗2 9.1k ⋯3 4.2k"), lines.take(4).mkString("|"))
    // A generation that has spent nothing yet is its bare marker: the band is about
    // where the tokens went, and `⋯2 0` answers nothing.
    val fresh = spent(
      running().copy(generations = Vector(priced(accepted(1, 90), 9_000, 3_300), gen(2, LoopGenerationState.Running))),
      9_000,
      3_300
    )
    val band = fsLines(app, ChatState.initial.copy(loops = Vector(fresh)), "perf", width = 120)(2)
    assert(band.contains("✓1 12.3k ⋯2") && !band.contains("⋯2 0"), band)
    // Too long for the row, and it is the oldest generations that go — as in the strip.
    val long = spent(
      running().copy(generations = (1 to 12).toVector.map(i => priced(accepted(i, 100.0 - i), 9_000, 3_300))),
      108_000,
      39_600
    )
    val cut = fsLines(app, ChatState.initial.copy(loops = Vector(long)), "perf", width = 60)(2)
    assert(cut.contains("✓12 12.3k"), cut)
    assert(cut.contains("…") && !cut.contains("✓1 12.3k ✓2"), cut)

  test("a loop that never counted a token gets no band, and the body keeps that row"):
    val app = appUI
    val plain = fsLines(app, ChatState.initial.copy(loops = Vector(running())), "perf", width = 120)
    assert(!plain(2).contains("✓1"), plain.take(4).mkString("|"))
    assert(!plain.exists(_.contains("✓1 0")), plain.mkString("|"))
    // The band is paid for out of the body, so a frame is the same height either way and
    // the row a missing band would have taken goes back to the transcript.
    val banded = fsLines(app, ChatState.initial.copy(loops = Vector(counted)), "perf", width = 120)
    assertEquals(plain.length, banded.length)
    assertEquals(plain.map(_.length).distinct, banded.map(_.length).distinct)

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
