package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import auk.tui.render.{Color, Style}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, TeamMemberView, UserCommand}
import auk.workflow.{Transcript, TranscriptEvent}

/** The subagent (team) panel: grid rendering below the prompt box, ↓-on-a-fresh-
  * line focus entry, arrow navigation with the capped scroll window, the
  * tide badge, and the fullscreen member transcript. */
class TeamPanelSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

  private def member(id: String, working: Boolean = false, out: Long = 0, retired: Boolean = false): TeamMemberView =
    TeamMemberView(id, s"$id desc", working, inputTokens = 0, outputTokens = out, retired = retired)

  private def roster(n: Int): Vector[TeamMemberView] =
    (1 to n).toVector.map(i => member(f"m$i%02d"))

  private def liveLines(app: ChatApp, state: ChatState, width: Int = 120): Vector[String] =
    Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)

  /** The fullscreen member-transcript frame, as plain rows. */
  private def fsLines(
      app: ChatApp,
      state: ChatState,
      memberId: String,
      offset: Int = 0,
      width: Int = 80,
      rows: Int = 24
  ): Vector[String] =
    val open = state.copy(overlay = Overlay.TeamTranscript(memberId, offset))
    Layout.lay(app.view(open, Viewport(width, rows)).fullscreen.get, width).map(_.plain)

  /** The dim left bar the chat puts on reasoning and tool lines. */
  private val Bar = "│"

  /** Where the header bar lands, below the frame's top padding row. */
  private val HeaderRow = 1

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
      case Sub.Batch(ss)          => ss.exists(hasTick)
      case Sub.TimeEveryMs(_, _)  => true
      case _                      => false

  test("a Team snapshot folds into state and the panel docks below the footer"):
    val app = appUI
    val (st, _) = app.update(Event.Inbound1(AgentEvent.Team(roster(3))), ChatState.initial)
    assertEquals(st.team.map(_.id), Vector("m01", "m02", "m03"))
    val lines = liveLines(app, st)
    val prompt = lines.indexWhere(_.contains("›"))
    val footer = lines.indexWhere(_.contains("ctrl+c or / for commands"))
    val rule = lines.indexWhere(l => l.contains("╭─ subagents"))
    val panel = lines.indexWhere(l => l.contains("m01"))
    assert(prompt >= 0 && footer > prompt && rule > footer && panel > rule, lines.mkString("|"))
    // The titled frame closes the panel: its bottom edge is the live stack's last line.
    val bottom = lines.lastOption.getOrElse("")
    assert(bottom.contains("╰") && bottom.contains("╯"), bottom)

  test("no panel renders while the team is empty"):
    val lines = liveLines(appUI, ChatState.initial)
    assert(!lines.exists(_.contains("○")), lines.mkString("|"))
    assert(!lines.exists(_.contains("subagents")), lines.mkString("|"))

  test("the column count follows the terminal width"):
    val app = appUI
    val st = ChatState.initial.copy(team = roster(3))
    // 120 cols fit three ~38-col cells on one line.
    val wide = liveLines(app, st, width = 120)
    assert(wide.exists(l => l.contains("m01") && l.contains("m02") && l.contains("m03")), wide.mkString("|"))
    // 40 cols fit one column: each member on its own line.
    val narrow = liveLines(app, st, width = 40)
    assert(narrow.exists(l => l.contains("m01") && !l.contains("m02")), narrow.mkString("|"))
    assert(narrow.exists(l => l.contains("m02") && !l.contains("m01")), narrow.mkString("|"))

  test("↓ on a fresh line focuses the panel; mid-history it keeps recalling; no members keeps it a no-op"):
    val app = appUI
    val base = ChatState.initial.copy(team = roster(3), inputHistory = Vector("a", "b"), histNav = 2)
    val (focused, _) = app.update(Event.HistoryNext, base)
    assertEquals(focused.teamSel, Some(0))
    // While recalling history (histNav below the fresh line) ↓ steps history.
    val recalling = base.copy(histNav = 0, input = "a")
    val (stepped, _) = app.update(Event.HistoryNext, recalling)
    assertEquals(stepped.teamSel, None)
    assertEquals(stepped.histNav, 1)
    // Without members the fresh-line ↓ stays inert.
    val bare = ChatState.initial.copy(inputHistory = Vector("a"), histNav = 1)
    val (same, _) = app.update(Event.HistoryNext, bare)
    assertEquals(same.teamSel, None)

  test("focused panel keys: arrows move the grid, Enter opens, Esc exits, typing falls through"):
    val app = appUI
    val st = ChatState.initial.copy(team = roster(2), teamSel = Some(0))
    assertEquals(keyEvent(app, st, Key.Down), Some(Event.TeamMove(0, 1)))
    assertEquals(keyEvent(app, st, Key.Up), Some(Event.TeamMove(0, -1)))
    assertEquals(keyEvent(app, st, Key.Left), Some(Event.TeamMove(-1, 0)))
    assertEquals(keyEvent(app, st, Key.Right), Some(Event.TeamMove(1, 0)))
    assertEquals(keyEvent(app, st, Key.Enter), Some(Event.TeamOpen))
    assertEquals(keyEvent(app, st, Key.Esc), Some(Event.TeamExit))
    assertEquals(keyEvent(app, st, Key.Char('x')), Some(Event.KeyChar('x')))

  test("typing while focused drops the focus and edits the input"):
    val app = appUI
    val st = ChatState.initial.copy(team = roster(2), teamSel = Some(1))
    val (typed, _) = app.update(Event.KeyChar('x'), st)
    assertEquals(typed.teamSel, None)
    assertEquals(typed.input, "x")

  test("grid moves use the rendered column count; ↑ from the top row exits"):
    val app = appUI
    val st = ChatState.initial.copy(team = roster(6), teamSel = Some(0))
    // Render at 100 cols → 2 columns; the geometry is recorded for the update loop.
    liveLines(app, st, width = 100)
    assertEquals(app.update(Event.TeamMove(0, 1), st)._1.teamSel, Some(2))
    assertEquals(app.update(Event.TeamMove(1, 0), st)._1.teamSel, Some(1))
    // ↓ on the last row clamps to the last member.
    assertEquals(app.update(Event.TeamMove(0, 1), st.copy(teamSel = Some(5)))._1.teamSel, Some(5))
    // ↑ anywhere on the top row returns focus to the input.
    val (exited, _) = app.update(Event.TeamMove(0, -1), st.copy(teamSel = Some(1)))
    assertEquals(exited.teamSel, None)

  test("overflow caps the panel and the focused selection scrolls through it"):
    val app = appUI
    val unfocused = ChatState.initial.copy(team = roster(12))
    val lines = liveLines(app, unfocused, width = 40)
    assert(lines.exists(_.contains("m04")), lines.mkString("|"))
    assert(!lines.exists(_.contains("m05")), lines.mkString("|"))
    assert(lines.exists(_.contains("+8 more")), lines.mkString("|"))
    // Focused: stepping below the window advances the scroll row.
    var st = unfocused.copy(teamSel = Some(0))
    liveLines(app, st, width = 40) // records 1 column
    for _ <- 1 to 4 do st = app.update(Event.TeamMove(0, 1), st)._1
    assertEquals(st.teamSel, Some(4))
    assertEquals(st.teamScroll, 1)
    val focused = liveLines(app, st, width = 40)
    assert(!focused.exists(_.contains("m01")), focused.mkString("|"))
    assert(focused.exists(_.contains("m05")), focused.mkString("|"))
    assert(focused.exists(_.contains("2-5/12")), focused.mkString("|"))

  test("Enter opens the fullscreen member transcript; Esc returns with the focus restored"):
    val app = appUI
    val st = ChatState.initial.copy(team = roster(3), teamSel = Some(1))
    val (opened, _) = app.update(Event.TeamOpen, st)
    assertEquals(opened.overlay, Overlay.TeamTranscript("m02", 0))
    val screen = app.view(opened, Viewport(80, 24))
    assert(screen.fullscreen.isDefined)
    val fs = Layout.lay(screen.fullscreen.get, 80).map(_.plain)
    assertEquals(fs.length, 24)
    assert(fs.exists(l => l.contains("m02") && l.contains("m02 desc")), fs.mkString("|"))
    assert(fs.exists(_.contains("(no activity yet)")), fs.mkString("|"))
    val (closed, _) = app.update(Event.TeamTranscriptBack, opened)
    assertEquals(closed.overlay, Overlay.None)
    assertEquals(closed.teamSel, Some(1))

  test("a screen switch rides a full repaint; scrolling within one does not"):
    def hasRefresh(cmd: Cmd[Event]): Boolean =
      cmd match
        case Cmd.Refresh   => true
        case Cmd.Batch(cs) => cs.exists(hasRefresh)
        case _             => false
    val app = appUI
    val st = ChatState.initial.copy(team = roster(3), teamSel = Some(1))
    val (opened, openCmd) = app.update(Event.TeamOpen, st)
    assert(hasRefresh(openCmd))
    val (scrolled, scrollCmd) = app.update(Event.TeamTranscriptScroll(3), opened)
    assert(!hasRefresh(scrollCmd))
    val (_, backCmd) = app.update(Event.TeamTranscriptBack, scrolled)
    assert(hasRefresh(backCmd))

  test("member transcript keys: scroll, follow, back"):
    val app = appUI
    val open = ChatState.initial.copy(team = roster(1), overlay = Overlay.TeamTranscript("m01", 0))
    assertEquals(keyEvent(app, open, Key.Up), Some(Event.TeamTranscriptScroll(1)))
    assertEquals(keyEvent(app, open, Key.End), Some(Event.TeamTranscriptFollow))
    assertEquals(keyEvent(app, open, Key.Esc), Some(Event.TeamTranscriptBack))
    // A tall transcript, rendered once so the update loop has the content
    // geometry it clamps the scroll against.
    val active = activity(app, open, (1 to 30).map(i => TranscriptEvent.Said("team", "m01", s"row $i\n\n"))*)
    fsLines(app, active, "m01")
    val (scrolled, _) = app.update(Event.TeamTranscriptScroll(3), active)
    assertEquals(scrolled.overlay, Overlay.TeamTranscript("m01", 3))
    val (followed, _) = app.update(Event.TeamTranscriptFollow, scrolled)
    assertEquals(followed.overlay, Overlay.TeamTranscript("m01", 0))
    // The offset floors at zero and caps at the top: one step back from far
    // past the top moves it — no invisible overscroll to unwind.
    assertEquals(app.update(Event.TeamTranscriptScroll(-5), active)._1.overlay, Overlay.TeamTranscript("m01", 0))
    val maxed = app.update(Event.TeamTranscriptScroll(999), active)._1.overlay.asInstanceOf[Overlay.TeamTranscript].offset
    assert(maxed > 3, s"expected scrollable content, offset capped at $maxed")
    assertEquals(
      app.update(Event.TeamTranscriptScroll(-1), app.update(Event.TeamTranscriptScroll(999), active)._1)._1.overlay,
      Overlay.TeamTranscript("m01", maxed - 1)
    )

  test("member activity folds under (team, id): the cell shows the tool, the transcript view its rows"):
    val app = appUI
    var st = ChatState.initial.copy(team = Vector(member("m01", working = true)))
    st = app.update(Event.Inbound1(AgentEvent.Activity(TranscriptEvent.ToolCalled("team", "m01", "c1", "grep", "NodeProcess"))), st)._1
    assert(st.transcripts.contains(("team", "m01")))
    val cell = liveLines(app, st).find(_.contains("m01")).getOrElse("")
    assert(cell.contains("▸ grep"), cell)
    // The body now speaks the main chat's language: a running tool is a bar line
    // carrying the chat's own label for it, with no timing (team items have none).
    val fs = fsLines(app, st, "m01")
    assert(fs.exists(_.contains(s"$Bar grep")), fs.mkString("|"))
    assert(!fs.exists(_.contains("0.0s")), fs.mkString("|"))

  test("a member's tool call is labelled the way the main chat labels it"):
    val app = appUI
    var st = ChatState.initial.copy(team = Vector(member("m01")))
    st = app.update(Event.Inbound1(AgentEvent.Activity(
      TranscriptEvent.ToolCalled("team", "m01", "c1", "read", """{"path":"Foo.scala"}"""))), st)._1
    assert(fsLines(app, st, "m01").exists(_.contains(s"$Bar Reading Foo.scala")), fsLines(app, st, "m01").mkString("|"))

  // -- a live eval's clocks on a member's row ----------------------------------

  /** A member mid-eval: one open `eval_scala` call that has reported `progress`,
    * anchored at `at`, rendered against `clockMs`. */
  private def evalMemberState(
      progress: Map[String, String],
      at: Option[Long] = None,
      tool: String = "eval_scala",
      returned: Boolean = false,
      clockMs: Long = 1_400L
  ): ChatState =
    val called = Transcript.empty.update(TranscriptEvent.ToolCalled("team", "m01", "c1", tool, """{"code":"1 + 1"}"""))
    val reported = if progress.isEmpty then called else called.update(TranscriptEvent.ToolProgressed("team", "m01", "c1", progress), at)
    val t = if returned then reported.update(TranscriptEvent.ToolReturned("team", "m01", "c1", "val res0: Int = 2", false), at) else reported
    ChatState.initial.copy(
      team = Vector(member("m01", working = true)),
      transcripts = Map(("team", "m01") -> t),
      clockMs = clockMs
    )

  private def compiling(elapsedMs: String, phaseMs: String): Map[String, String] =
    Map("replElapsedMs" -> elapsedMs, "replPhase" -> "compiling", "replPhaseMs" -> phaseMs)

  /** The row a tool call renders on in the member's transcript. */
  private def evalRow(app: ChatApp, st: ChatState): String =
    val fs = fsLines(app, st, "m01")
    fs.find(_.contains("Executing code")).orElse(fs.find(_.contains("Reading")))
      .getOrElse(fail(s"no tool row: ${fs.mkString("|")}"))

  test("a member's running eval shows both clocks, exactly as the lead's own row does"):
    val app = appUI
    val row = evalRow(app, evalMemberState(compiling("12400", "900"), at = Some(1_000L)))
    assert(row.contains("Executing code (12.8s) · Compiling (1.3s)"), row)

  test("a member's eval row ticks between reports, cached block and all"):
    // The SAME state rendered at two frames: between reports the transcript item
    // is reference-identical, so the per-item block cache hits and the row is
    // rebuilt from a cached block. The clocks must still advance — they are read
    // at render time, not baked into what is cached.
    val app = appUI
    val st = evalMemberState(compiling("12400", "900"), at = Some(1_000L), clockMs = 1_000L)
    val onArrival = evalRow(app, st)
    assert(onArrival.contains("Executing code (12.4s) · Compiling (0.9s)"), onArrival)
    val later = evalRow(app, st.copy(clockMs = 2_500L))
    assert(later.contains("Executing code (13.9s) · Compiling (2.4s)"), later)

  test("a member's returned eval carries no clocks, whatever its last report said"):
    val app = appUI
    val fs = fsLines(app, evalMemberState(compiling("12400", "900"), at = Some(1_000L), returned = true), "m01")
    assert(!fs.exists(_.contains("Compiling")), fs.mkString("|"))
    assert(!fs.exists(_.contains("12.8s")), fs.mkString("|"))

  test("a member's non-eval tool row is untouched by a progress report"):
    val app = appUI
    val plain = evalRow(app, evalMemberState(Map.empty, tool = "read"))
    val reporting = evalRow(app, evalMemberState(compiling("12400", "900"), at = Some(1_000L), tool = "read"))
    assertEquals(reporting, plain)

  test("a member's eval reads as it always did until a report carries a clock"):
    val app = appUI
    val bare = evalRow(app, evalMemberState(Map.empty))
    assert(!bare.contains("s)"), bare)
    assertEquals(evalRow(app, evalMemberState(Map("workerPid" -> "4242"), at = Some(1_000L))), bare)

  // -- the member transcript body renders through the main chat's pipeline ------

  test("a message from the lead renders as the chat's grey prompt box, untagged"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Received("team", "m01", "lead", "please do it"))
    val fs = fsLines(app, st, "m01")
    val boxed = fs.indexWhere(_.contains("› please do it"))
    assert(boxed > 0, fs.mkString("|"))
    assert(fs(boxed - 1).contains("╭─"), fs.mkString("|"))
    assert(fs(boxed + 1).contains("╰─"), fs.mkString("|"))
    // The lead is the implied sender here, so it goes unlabelled.
    assert(!fs.exists(_.contains("from lead")), fs.mkString("|"))

  test("a message from another member carries a dim sender line inside the box"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Received("team", "m01", "m02", "handing this over"))
    val fs = fsLines(app, st, "m01")
    val tag = fs.indexWhere(_.contains("from m02"))
    assert(tag > 0, fs.mkString("|"))
    assert(fs(tag - 1).contains("╭─"), fs.mkString("|"))
    // The sender line sits inside the box, directly above the message.
    assert(fs(tag + 1).contains("› handing this over"), fs.mkString("|"))

  test("a member's prose renders as markdown, not as flat rows"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Said("team", "m01", "# Heading\n\nsome **bold** prose"))
    val fs = fsLines(app, st, "m01")
    assert(fs.exists(_.contains("Heading")), fs.mkString("|"))
    // The markdown renderer strips the emphasis markers rather than printing them.
    assert(fs.exists(l => l.contains("bold") && !l.contains("**")), fs.mkString("|"))

  test("quiet work folds to one summary line, with no duration to report"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Thought("team", "m01", "let me look"),
      TranscriptEvent.ToolCalled("team", "m01", "c1", "eval_scala", """{"code":"1"}"""),
      TranscriptEvent.ToolReturned("team", "m01", "c1", "res0: Int = 1", false))
    val fs = fsLines(app, st, "m01")
    val summary = fs.find(_.contains("✻")).getOrElse("")
    assert(summary.contains("Thought"), fs.mkString("|"))
    assert(summary.contains("executed a code snippet"), fs.mkString("|"))
    // No timings exist for team items, so the summary must not claim any.
    assert(!summary.contains("for 0.0s"), summary)
    // The folded reasoning text itself is hidden, exactly as in the main chat.
    assert(!fs.exists(_.contains("let me look")), fs.mkString("|"))
    // ...but the pointer to the unfolded view does not follow it here: `ctrl+c o`
    // shows THIS conversation, not a member's.
    assert(!summary.contains("ctrl+c o"), summary)

  test("reasoning still being written shows as the live sliding window"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01", working = true))),
      TranscriptEvent.Thought("team", "m01", "first I check the parser, "),
      TranscriptEvent.Thought("team", "m01", "then the config loader"))
    val fs = fsLines(app, st, "m01")
    assert(fs.exists(_.contains("thinking ▸")), fs.mkString("|"))
    assert(fs.exists(_.contains("then the config loader")), fs.mkString("|"))
    // It is live, not folded away.
    assert(!fs.exists(_.contains("✻ Thought")), fs.mkString("|"))

  test("open reasoning settles the moment prose lands or the member idles"):
    val app = appUI
    val thinking = activity(appUI, ChatState.initial.copy(team = Vector(member("m01", working = true))),
      TranscriptEvent.Thought("team", "m01", "weighing the options"))
    // Prose after it: the thought is no longer the tail, so it folds.
    val spoke = activity(app, thinking, TranscriptEvent.Said("team", "m01", "here goes"))
    val afterProse = fsLines(app, spoke, "m01")
    assert(afterProse.exists(_.contains("✻ Thought")), afterProse.mkString("|"))
    assert(!afterProse.exists(_.contains("weighing the options")), afterProse.mkString("|"))
    // The member idling folds it too, with the transcript otherwise untouched —
    // the same item, so this only holds because the open tail is never cached.
    val idled = thinking.copy(team = Vector(member("m01")))
    val afterIdle = fsLines(app, idled, "m01")
    assert(afterIdle.exists(_.contains("✻ Thought")), afterIdle.mkString("|"))
    assert(!afterIdle.exists(_.contains("thinking ▸")), afterIdle.mkString("|"))

  test("a working member gets the animated Working… tail with its token count"):
    val app = appUI
    val busy = activity(appUI, ChatState.initial.copy(team = Vector(member("m01", working = true, out = 1200))),
      TranscriptEvent.Said("team", "m01", "on it"))
    val fs = fsLines(app, busy, "m01")
    assert(fs.exists(l => l.contains("Working…") && l.contains("(1.2k tokens)")), fs.mkString("|"))
    // It is the live tail: an idle member shows no such line.
    val idle = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Said("team", "m01", "done"))
    assert(!fsLines(app, idle, "m01").exists(_.contains("Working…")), fsLines(app, idle, "m01").mkString("|"))

  test("the transcript frame is exactly viewport.rows lines, scrolls, and reports its range"):
    val app = appUI
    // Separate paragraphs: consecutive prose deltas merge into one run, and a
    // markdown paragraph reflows, so single newlines would not grow the body.
    val many = (1 to 60).map(i => TranscriptEvent.Said("team", "m01", s"line $i\n\n"))
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))), many*)
    val tail = fsLines(app, st, "m01")
    assertEquals(tail.length, 24)
    assert(tail(HeaderRow).contains("m01 — m01 desc"), tail(HeaderRow))
    // Bottom-anchored: the newest line is visible, the oldest scrolled off.
    assert(tail.exists(_.contains("line 60")), tail.mkString("|"))
    assert(!tail.exists(_.trim == "line 1"), tail.mkString("|"))
    assert(tail.last.contains("of"), tail.last)
    // Scrolling up moves the window back and the range with it.
    val scrolled = fsLines(app, st, "m01", offset = 10)
    assertEquals(scrolled.length, 24)
    assertNotEquals(scrolled.last, tail.last)
    assert(!scrolled.exists(_.contains("line 60")), scrolled.mkString("|"))

  test("the member transcript is inset by the gutter, bars included"):
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Received("team", "m01", "lead", "please do it"))
    val fs = fsLines(app, st, "m01")
    assert(fs.head.isBlank, s"a padding row belongs above the header: '${fs.head}'")
    assert(fs(HeaderRow).startsWith("   ○ m01 — m01 desc"), fs(HeaderRow))
    assert(fs.last.startsWith("   ↑/↓"), fs.last)
    assert(fs(HeaderRow + 1).isBlank, s"a padding row belongs under the header: '${fs(HeaderRow + 1)}'")
    assert(fs(fs.length - 2).isBlank, s"a padding row belongs above the footer: '${fs(fs.length - 2)}'")
    // The round box gets symmetric margins rather than sitting on the edge.
    val top = fs.find(_.contains("╭─")).getOrElse(fail(fs.mkString("|")))
    assertEquals(top.take(3), "   ", top)
    assertEquals(top.takeRight(3), "   ", top)

  test("the member transcript's gutters carry no overlay backdrop"):
    // The body is the chat's own look on the plain background: if the frame's
    // panel colour leaked into the gutters it would frame the chat in dark bars.
    val app = appUI
    val st = activity(appUI, ChatState.initial.copy(team = Vector(member("m01"))),
      TranscriptEvent.Said("team", "m01", "plain background please"))
    val open = st.copy(overlay = Overlay.TeamTranscript("m01", 0))
    val rendered = Layout.lay(app.view(open, Viewport(80, 24)).fullscreen.get, 80).map(_.render)
    val bodyRows = rendered.slice(HeaderRow + 2, rendered.length - 2)
    val overlayBg = Style(bg = Color.Indexed(236)).setSequence.filter(_.isDigit)
    assert(bodyRows.forall(!_.contains("48;5;236")), "no overlay background may appear in the body")
    assert(overlayBg.contains("236"), "guard: the overlay backdrop is still indexed 236")
    // The bars themselves keep their chrome.
    assert(rendered(HeaderRow).contains("48;5;236"), "the header bar keeps its band")
    // The top pad above it is plain, like the body.
    assert(!rendered.head.contains("48;5;236"), "the row above the header stays unstyled")

  test("a transcript with no activity says so; a vanished member degrades"):
    val app = appUI
    val quiet = fsLines(app, ChatState.initial.copy(team = Vector(member("m01"))), "m01")
    assert(quiet.exists(_.contains("(no activity yet)")), quiet.mkString("|"))
    assertEquals(quiet.length, 24)
    val gone = fsLines(app, ChatState.initial.copy(team = Vector(member("m01"))), "ghost")
    assert(gone.exists(_.contains("This team member is gone")), gone.mkString("|"))
    assertEquals(gone.length, 24)

  test("the latest-action cell falls back to the description, and prose shows its last line"):
    val app = appUI
    var st = ChatState.initial.copy(team = Vector(member("m01"), member("m02")))
    val fresh = liveLines(app, st).find(_.contains("m01")).getOrElse("")
    assert(fresh.contains("m01 desc"), fresh)
    st = app.update(Event.Inbound1(AgentEvent.Activity(TranscriptEvent.Said("team", "m02", "first line\nthe last line"))), st)._1
    val prose = liveLines(app, st).find(_.contains("m02")).getOrElse("")
    assert(prose.contains("the last line"), prose)

  test("a member handed a message shows it in the cell until it produces output"):
    val app = appUI
    var st = ChatState.initial.copy(team = Vector(member("m01", working = true)))
    st = app.update(Event.Inbound1(AgentEvent.Activity(
      TranscriptEvent.Received("team", "m01", "lead", "please do it\nwith care"))), st)._1
    val handed = liveLines(app, st).find(_.contains("m01")).getOrElse("")
    assert(handed.contains("› please do it"), handed)
    // Its own output takes the cell back over as soon as any arrives.
    st = app.update(Event.Inbound1(AgentEvent.Activity(TranscriptEvent.Said("team", "m01", "on it"))), st)._1
    assert(liveLines(app, st).find(_.contains("m01")).getOrElse("").contains("on it"))

  test("token usage renders right-aligned in the cell"):
    val app = appUI
    val st = ChatState.initial.copy(team = Vector(member("m01", out = 12_345)))
    val cell = liveLines(app, st).find(_.contains("m01")).getOrElse("")
    assert(cell.contains("12.3k"), cell)

  test("the working badge's tide flows with the clock; an idle badge is still"):
    val app = appUI
    val st = ChatState.initial.copy(team = Vector(member("poet", working = true), member("critic")))
    def line(clock: Long): String =
      liveLines(app, st.copy(clockMs = clock)).find(_.contains("poet")).getOrElse("")
    // The tide swells: low water at frame 0, full crest at frame 4 (600ms).
    assert(line(0).contains("⣀"), line(0))
    assert(line(600).contains("⣿"), line(600))
    // Idle members never animate: without the working member no tide glyph shows.
    val idleOnly = ChatState.initial.copy(team = Vector(member("critic")))
    val glyphs = "⣀⣄⣦⣷⣿⣾⣴⣠"
    assert(!liveLines(app, idleOnly.copy(clockMs = 600)).exists(l => glyphs.exists(l.contains(_))))

  test("the ambient panel drops retired cells; browsing shows them, marked and still"):
    val app = appUI
    val st = ChatState.initial.copy(
      team = Vector(member("poet", working = true), member("critic", retired = true)),
      clockMs = 600
    )
    // Ambient: only members still in play, with the tally as the retired one's
    // single trace (the meta needs the room, so this is the wide render).
    val ambient = liveLines(app, st, width = 120)
    assert(ambient.exists(_.contains("poet")), ambient.mkString("|"))
    assert(!ambient.exists(_.contains("critic")), ambient.mkString("|"))
    assert(ambient.exists(l => l.contains("1 working") && l.contains("1 retired")), ambient.mkString("|"))
    // Browsing: the cell is back, one column so it is a line of its own — badge ×,
    // ordinal 2 (its place in the roster), and the tide left with the working member.
    val cell = liveLines(app, st.copy(teamSel = Some(0)), width = 40).find(_.contains("critic")).getOrElse("")
    assert(cell.contains("2 × critic"), cell)
    val glyphs = "⣀⣄⣦⣷⣿⣾⣴⣠"
    assert(!glyphs.exists(cell.contains(_)), cell)
    // Nothing about a retired member animates, so it must not drive the clock.
    assert(!hasTick(app.subscriptions(ChatState.initial.copy(team = Vector(member("critic", retired = true))))))
    // Enter from browse opens its transcript, headed retired, with no Working… tail.
    val (opened, _) = app.update(Event.TeamOpen, st.copy(teamSel = Some(1)))
    assertEquals(opened.overlay, Overlay.TeamTranscript("critic", 0))
    val fs = fsLines(app, activity(app, st, TranscriptEvent.Said("team", "critic", "all done")), "critic")
    assert(fs(HeaderRow).contains("× critic — critic desc"), fs(HeaderRow))
    assert(fs(HeaderRow).contains("retired"), fs(HeaderRow))
    assert(!fs.exists(_.contains("Working…")), fs.mkString("|"))
    assert(fs.exists(_.contains("all done")), fs.mkString("|"))

  test("an all-retired team renders no ambient panel, and ↓ brings the browse grid up from nothing"):
    val app = appUI
    val st = ChatState.initial.copy(
      team = Vector(member("scribe", retired = true), member("critic", retired = true)),
      inputHistory = Vector("a"),
      histNav = 1
    )
    // Nothing left in play, so the panel goes entirely — no frame, no tally, no
    // placeholder — exactly as for a team that was never created.
    val ambient = liveLines(app, st, width = 120)
    assert(!ambient.exists(_.contains("subagents")), ambient.mkString("|"))
    assert(!ambient.exists(_.contains("retired")), ambient.mkString("|"))
    assert(!ambient.exists(l => l.contains("scribe") || l.contains("critic")), ambient.mkString("|"))
    // ↓ on the fresh input line still focuses the panel, which is what raises the
    // browse grid: both retired members, and Enter into a transcript from there.
    val (focused, _) = app.update(Event.HistoryNext, st)
    assertEquals(focused.teamSel, Some(0))
    val browsing = liveLines(app, focused, width = 40)
    assert(browsing.exists(_.contains("╭─ subagents")), browsing.mkString("|"))
    assert(browsing.exists(_.contains("scribe")), browsing.mkString("|"))
    assert(browsing.exists(_.contains("critic")), browsing.mkString("|"))
    assertEquals(app.update(Event.TeamOpen, focused)._1.overlay, Overlay.TeamTranscript("scribe", 0))
    // Leaving browse — typing or Esc — takes the whole panel away again.
    for left <- List(app.update(Event.KeyChar('x'), focused)._1, app.update(Event.TeamExit, focused)._1) do
      assertEquals(left.teamSel, None)
      val gone = liveLines(app, left, width = 120)
      assert(!gone.exists(_.contains("subagents")), gone.mkString("|"))

  test("browse sorts retired members last, and ordinals are grid positions"):
    val app = appUI
    // Retired FIRST in creation order: roster order and display order disagree, so
    // this is the case that can tell them apart.
    val st = ChatState.initial.copy(
      team = Vector(member("scribe", retired = true), member("poet")),
      teamSel = Some(1)
    )
    val lines = liveLines(app, st, width = 40)
    val poetRow = lines.indexWhere(_.contains("poet"))
    val scribeRow = lines.indexWhere(_.contains("scribe"))
    assert(poetRow >= 0 && scribeRow > poetRow, lines.mkString("|"))
    // Ordinals count the grid, not the roster: the live member is 1 though it was
    // created second, and the retired one is 2 though it holds roster index 0.
    assert(lines(poetRow).contains("1 ○ poet"), lines(poetRow))
    assert(lines(scribeRow).contains("2 × scribe"), lines(scribeRow))

  test("ambient and browse give a live member the same ordinal across a retired one"):
    val app = appUI
    // Created live, retired, live: the retired member sits between them in creation
    // order, and must not shift the live members' numbering in either mode.
    val st = ChatState.initial.copy(
      team = Vector(member("poet"), member("scribe", retired = true), member("critic"))
    )
    val ambient = liveLines(app, st, width = 40)
    assert(ambient.exists(_.contains("1 ○ poet")), ambient.mkString("|"))
    assert(ambient.exists(_.contains("2 ○ critic")), ambient.mkString("|"))
    assert(!ambient.exists(_.contains("scribe")), ambient.mkString("|"))
    val browsing = liveLines(app, st.copy(teamSel = Some(0)), width = 40)
    assert(browsing.exists(_.contains("1 ○ poet")), browsing.mkString("|"))
    assert(browsing.exists(_.contains("2 ○ critic")), browsing.mkString("|"))
    assert(browsing.exists(_.contains("3 × scribe")), browsing.mkString("|"))

  test("↓ focuses the first member in play, not roster zero"):
    val app = appUI
    val st = ChatState.initial.copy(
      team = Vector(member("scribe", retired = true), member("poet")),
      inputHistory = Vector("a"),
      histNav = 1
    )
    // Roster 0 has retired, so browse opens on the live member behind it.
    val (focused, _) = app.update(Event.HistoryNext, st)
    assertEquals(focused.teamSel, Some(1))
    assertEquals(focused.teamSel.flatMap(focused.team.lift).map(_.id), Some("poet"))

  test("arrow navigation steps display order across the retired boundary"):
    val app = appUI
    // Created retired, live, live, retired — so the grid reads a, b, r1, r2 while
    // the roster reads r1, a, b, r2.
    val st = ChatState.initial.copy(
      team = Vector(
        member("r1", retired = true),
        member("a"),
        member("b"),
        member("r2", retired = true)
      ),
      teamSel = Some(1)
    )
    liveLines(app, st, width = 100) // records 2 columns
    // → within the live run, and ↓ from the top row into the retired run below it.
    assertEquals(app.update(Event.TeamMove(1, 0), st)._1.teamSel, Some(2))
    assertEquals(app.update(Event.TeamMove(0, 1), st)._1.teamSel, Some(0))
    // → off the last live cell crosses the boundary; ← comes back over it.
    assertEquals(app.update(Event.TeamMove(1, 0), st.copy(teamSel = Some(2)))._1.teamSel, Some(0))
    assertEquals(app.update(Event.TeamMove(-1, 0), st.copy(teamSel = Some(0)))._1.teamSel, Some(2))
    // ↑ exits from the top display row, whatever roster index the selection holds.
    assertEquals(app.update(Event.TeamMove(0, -1), st)._1.teamSel, None)

  test("a member retiring mid-browse keeps the selection on the same member"):
    val app = appUI
    val st = ChatState.initial.copy(team = Vector(member("poet"), member("critic")), teamSel = Some(1))
    // "poet" retires while the user browses "critic": the grid reorders under the
    // cursor, so a selection naming a slot would slide onto poet.
    val reordered = Vector(member("poet", retired = true), member("critic"))
    val (folded, _) = app.update(Event.Inbound1(AgentEvent.Team(reordered)), st)
    assertEquals(folded.teamSel.flatMap(folded.team.lift).map(_.id), Some("critic"))
    // Enter still opens the member the cursor was on.
    assertEquals(app.update(Event.TeamOpen, folded)._1.overlay, Overlay.TeamTranscript("critic", 0))
    // And that member now leads the grid, with poet's cell after it.
    val lines = liveLines(app, folded, width = 40)
    val criticRow = lines.indexWhere(_.contains("critic"))
    val poetRow = lines.indexWhere(_.contains("poet"))
    assert(criticRow >= 0 && poetRow > criticRow, lines.mkString("|"))
    assert(lines(criticRow).contains("1 ○ critic"), lines(criticRow))
    assert(lines(poetRow).contains("2 × poet"), lines(poetRow))

  test("dropping focus from a retired cell falls back to the clean ambient panel"):
    val app = appUI
    val st = ChatState.initial.copy(
      team = Vector(member("poet"), member("critic", retired = true)),
      teamSel = Some(1)
    )
    assert(liveLines(app, st, width = 40).exists(_.contains("critic")))
    // Typing and Esc both drop the focus; the cell the selection sat on simply
    // leaves the ambient render with it.
    for dropped <- List(app.update(Event.KeyChar('x'), st)._1, app.update(Event.TeamExit, st)._1) do
      assertEquals(dropped.teamSel, None)
      val ambient = liveLines(app, dropped, width = 40)
      assert(ambient.exists(_.contains("poet")), ambient.mkString("|"))
      assert(!ambient.exists(_.contains("critic")), ambient.mkString("|"))

  test("the overflow tally counts what the mode actually shows"):
    val app = appUI
    val live = (1 to 9).toVector.map(i => member(f"m$i%02d"))
    val gone = (10 to 12).toVector.map(i => member(f"m$i%02d", retired = true))
    val st = ChatState.initial.copy(team = live ++ gone)
    // Ambient at one column: nine live members, four rows of them shown.
    val ambient = liveLines(app, st, width = 40)
    assert(ambient.exists(_.contains("+5 more")), ambient.mkString("|"))
    // Focused, the whole roster is in play and the range says so.
    val browsing = liveLines(app, st.copy(teamSel = Some(0)), width = 40)
    assert(browsing.exists(_.contains("1-4/12")), browsing.mkString("|"))

  test("the animation clock ticks while a member works, even with the agent idle"):
    val app = appUI
    val idle = ChatState.initial.copy(team = Vector(member("m01")))
    assert(!hasTick(app.subscriptions(idle)))
    val busy = ChatState.initial.copy(team = Vector(member("m01", working = true)))
    assert(hasTick(app.subscriptions(busy)))
    // Also with its transcript open, which is what animates the Working… tail.
    assert(hasTick(app.subscriptions(busy.copy(overlay = Overlay.TeamTranscript("m01", 0)))))

  test("the Working… tail animates with the render clock"):
    val app = appUI
    val busy = activity(appUI, ChatState.initial.copy(team = Vector(member("m01", working = true))),
      TranscriptEvent.Said("team", "m01", "on it"))
    // Plain text is stable; the shimmer is in the styling, so compare rendered.
    def tail(clock: Long): String =
      val open = busy.copy(clockMs = clock, overlay = Overlay.TeamTranscript("m01", 0))
      Layout.lay(app.view(open, Viewport(80, 24)).fullscreen.get, 80)
        .find(_.plain.contains("Working…")).map(_.render).getOrElse("")
    assertNotEquals(tail(0), tail(300), "the shimmer must sweep with the clock")

  test("a roster snapshot clamps a live selection"):
    val st = ChatState.initial.copy(team = roster(6), teamSel = Some(5))
    val shrunk = st.applyTeam(roster(2))
    assertEquals(shrunk.teamSel, Some(1))
    assertEquals(shrunk.applyTeam(Vector.empty).teamSel, None)
