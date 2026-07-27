package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, TeamMemberView, UserCommand}
import auk.workflow.TranscriptEvent

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

  private def member(id: String, working: Boolean = false, out: Long = 0): TeamMemberView =
    TeamMemberView(id, s"$id desc", working, inputTokens = 0, outputTokens = out)

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
    val rule = lines.indexWhere(l => l.contains("subagents") && l.contains("─"))
    val panel = lines.indexWhere(l => l.contains("001") && l.contains("m01"))
    assert(prompt >= 0 && footer > prompt && rule > footer && panel > rule, lines.mkString("|"))

  test("no panel renders while the team is empty"):
    val lines = liveLines(appUI, ChatState.initial)
    assert(!lines.exists(_.contains("001")), lines.mkString("|"))
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
    assert(lines.exists(_.contains("004")), lines.mkString("|"))
    assert(!lines.exists(_.contains("005")), lines.mkString("|"))
    assert(lines.exists(_.contains("+8 more")), lines.mkString("|"))
    // Focused: stepping below the window advances the scroll row.
    var st = unfocused.copy(teamSel = Some(0))
    liveLines(app, st, width = 40) // records 1 column
    for _ <- 1 to 4 do st = app.update(Event.TeamMove(0, 1), st)._1
    assertEquals(st.teamSel, Some(4))
    assertEquals(st.teamScroll, 1)
    val focused = liveLines(app, st, width = 40)
    assert(!focused.exists(_.contains("001")), focused.mkString("|"))
    assert(focused.exists(_.contains("005")), focused.mkString("|"))
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
    val (scrolled, _) = app.update(Event.TeamTranscriptScroll(3), open)
    assertEquals(scrolled.overlay, Overlay.TeamTranscript("m01", 3))
    val (followed, _) = app.update(Event.TeamTranscriptFollow, scrolled)
    assertEquals(followed.overlay, Overlay.TeamTranscript("m01", 0))
    // The offset floors at zero.
    assertEquals(app.update(Event.TeamTranscriptScroll(-5), open)._1.overlay, Overlay.TeamTranscript("m01", 0))

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
    assert(tail.head.contains("m01 — m01 desc"), tail.head)
    // Bottom-anchored: the newest line is visible, the oldest scrolled off.
    assert(tail.exists(_.contains("line 60")), tail.mkString("|"))
    assert(!tail.exists(_.trim == "line 1"), tail.mkString("|"))
    assert(tail.last.contains("of"), tail.last)
    // Scrolling up moves the window back and the range with it.
    val scrolled = fsLines(app, st, "m01", offset = 10)
    assertEquals(scrolled.length, 24)
    assertNotEquals(scrolled.last, tail.last)
    assert(!scrolled.exists(_.contains("line 60")), scrolled.mkString("|"))

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
    def tail(clock: Long): String =
      fsLines(app, busy.copy(clockMs = clock), "m01").find(_.contains("Working…")).getOrElse("")
    assertNotEquals(tail(0), tail(300), "the spinner must advance with the clock")

  test("a roster snapshot clamps a live selection"):
    val st = ChatState.initial.copy(team = roster(6), teamSel = Some(5))
    val shrunk = st.applyTeam(roster(2))
    assertEquals(shrunk.teamSel, Some(1))
    assertEquals(shrunk.applyTeam(Vector.empty).teamSel, None)
