package auk.tui

import gears.async.UnboundedChannel

import auk.tui.app.{Cmd, Key, Layout, Sub, Viewport}
import auk.tui.render.Width
import auk.agent.{AgentEvent, UserCommand}
import auk.workflow.{Forest, NodeStatus, OrchestrationEvent, RunStatus, Transcript, TranscriptEvent, TranscriptItem}
import auk.session.{SessionSnapshot, SessionSummary}

/** TUI workflows: folding orchestration events into `ChatState.activeWorkflows`
  * (keyed by run id), the compact per-status census that stands in for them in
  * the live region, and the `ctrl+c w` overlays — the list menu and the per-run
  * detail forest — that render the live forest on demand. */
class WorkflowForestSuite extends munit.FunSuite:

  import OrchestrationEvent.*

  private def app: ChatApp =
    ChatApp(UnboundedChannel[AgentEvent]().asReadable, UnboundedChannel[UserCommand](), UnboundedChannel[Unit](), UnboundedChannel[auk.agent.Inbox]())

  /** Render the committed transcript (history). */
  private def render(block: Block, width: Int = 80): Vector[String] =
    val state = ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(block))))
    app.view(state, Viewport(width, 30)).committed.flatMap(Layout.lay(_, width)).map(_.plain)

  /** Render the live region (where the notice lives). */
  private def renderLive(state: ChatState, width: Int = 80): Vector[String] =
    Layout.lay(app.view(state, Viewport(width, 30)).live, width).map(_.plain)

  /** Render a fullscreen workflow view (the three views take over the whole screen
    * via the alternate buffer, so they land in `Screen.fullscreen`, not `.live`). */
  private def fullscreenLines(state: ChatState, width: Int, rows: Int): Vector[String] =
    app.view(state, Viewport(width, rows)).fullscreen match
      case Some(el) => Layout.lay(el, width).map(_.plain)
      case None     => Vector.empty

  /** Render the per-run detail view (`ctrl+c w` → Enter) for one run. */
  private def detailFor(runId: String, forest: Forest, width: Int = 80): Vector[String] =
    fullscreenLines(ChatState.initial.copy(activeWorkflows = Vector(runId -> forest), overlay = Overlay.WorkflowDetail(runId, 0)), width, 24)

  /** Render the workflow list view (the `ctrl+c w` menu) over a set of runs. */
  private def listFor(workflows: Vector[(String, Forest)], width: Int = 80): Vector[String] =
    fullscreenLines(ChatState.initial.copy(activeWorkflows = workflows, overlay = Overlay.WorkflowList(0)), width, 24)

  /** Render the per-run detail view with transcripts at a given viewport (wide
    * enough for the two-pane preview when `width` is large). */
  private def detailLines(
      runId: String, forest: Forest, transcripts: Map[(String, String), Transcript],
      cursor: Int = 0, width: Int = 120, rows: Int = 30
  ): Vector[String] =
    fullscreenLines(
      ChatState.initial.copy(
        activeWorkflows = Vector(runId -> forest),
        transcripts = transcripts,
        overlay = Overlay.WorkflowDetail(runId, cursor)
      ),
      width, rows
    )

  /** Render the full transcript view for one node (offset 0 = tail-following). */
  private def transcriptLines(
      runId: String, nodeId: String, forest: Forest, t: Transcript,
      offset: Int = 0, width: Int = 100, rows: Int = 30
  ): Vector[String] =
    fullscreenLines(
      ChatState.initial.copy(
        activeWorkflows = Vector(runId -> forest),
        transcripts = Map((runId, nodeId) -> t),
        overlay = Overlay.WorkflowTranscript(runId, nodeId, offset)
      ),
      width, rows
    )

  /** The [[Event]] a key maps to under `state`'s active overlay. */
  private def keyEvent(state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil
    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  test("Forest folds group / node / progress / finish events in declaration order"):
    val f = Forest.empty
      .update(GroupDeclared("r", "g1", "hunt", "find bugs", None))
      .update(NodeDeclared("r", "a", Some("g1"), Nil))
      .update(NodeStarted("r", "a", "task A"))
      .update(NodeProgress("r", "a", 10, 20, Some("eval_scala")))
      .update(NodeDeclared("r", "b", Some("g1"), List("a")))
      .update(NodeFinished("r", "a", true, "ok"))
    assertEquals(f.groups.map(_.name), Vector("hunt"))
    assertEquals(f.nodes.map(_.id), Vector("a", "b"))
    assertEquals(f.nodes.head.status, NodeStatus.Done)
    assertEquals(f.nodes.head.outputTokens, 20L)
    assertEquals(f.nodes(1).deps, List("a"))
    assertEquals(f.nodes(1).status, NodeStatus.Pending)

  test("applyOrchestration folds events into activeWorkflows keyed by runId"):
    val updated = ChatState.initial
      .applyOrchestration(GroupDeclared("e1", "g1", "hunt", "d", None))
      .applyOrchestration(NodeDeclared("e1", "a", Some("g1"), Nil))
      .applyOrchestration(NodeStarted("e1", "a", "task A"))
    val forest = updated.activeWorkflows.collectFirst { case (id, f) if id == "e1" => f }
    assert(forest.exists(_.groups.map(_.name).contains("hunt")), updated.activeWorkflows.toString)
    assert(forest.exists(_.nodes.exists(n => n.id == "a" && n.status == NodeStatus.Running)), updated.activeWorkflows.toString)

  test("distinct runIds fold into separate activeWorkflows entries"):
    val updated = ChatState.initial
      .applyOrchestration(GroupDeclared("e1", "g1", "alpha", "d", None))
      .applyOrchestration(GroupDeclared("e2", "g1", "beta", "d", None))
    assertEquals(updated.activeWorkflows.map(_._1), Vector("e1", "e2"))
    assert(updated.activeWorkflows.find(_._1 == "e1").exists(_._2.groups.map(_.name) == Vector("alpha")))
    assert(updated.activeWorkflows.find(_._1 == "e2").exists(_._2.groups.map(_.name) == Vector("beta")))

  test("WorkflowFinished retains the run, settling its status (both remain)"):
    val updated = ChatState.initial
      .applyOrchestration(NodeDeclared("e1", "a", None, Nil))
      .applyOrchestration(NodeDeclared("e2", "b", None, Nil))
      .applyOrchestration(WorkflowFinished("e1", true, "done"))
    // Finished runs no longer vanish — e1 stays (settled Done) so its transcript
    // is readable right after it finishes; e2 is still running.
    assertEquals(updated.activeWorkflows.map(_._1), Vector("e1", "e2"))
    assertEquals(updated.activeWorkflows.collectFirst { case (id, f) if id == "e1" => f.status }, Some(auk.workflow.RunStatus.Done))
    assertEquals(updated.activeWorkflows.collectFirst { case (id, f) if id == "e2" => f.status }, Some(auk.workflow.RunStatus.Running))

  test("the detail overlay renders the forest grouped by group, with status glyphs"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "hunt", "find bugs", None))
      .update(NodeDeclared("e1", "alpha", Some("g1"), Nil))
      .update(NodeStarted("e1", "alpha", "task A"))
      .update(NodeFinished("e1", "alpha", true, "ok"))
      .update(NodeDeclared("e1", "beta", Some("g1"), Nil))
      .update(NodeStarted("e1", "beta", "task B"))
    val lines = detailFor("e1", forest)
    assert(lines.exists(_.contains("hunt")), lines.mkString("|"))
    assert(lines.exists(l => l.contains("✓") && l.contains("alpha")), lines.mkString("|"))
    assert(lines.exists(_.contains("beta")), lines.mkString("|"))

  test("the list overlay labels each run with its id and settled/total progress"):
    val forest = Forest.empty
      .update(NodeDeclared("wf-7-1", "a", None, Nil))
      .update(NodeFinished("wf-7-1", "a", true, "ok"))
      .update(NodeDeclared("wf-7-1", "b", None, Nil))
    val lines = listFor(Vector("wf-7-1" -> forest))
    assert(lines.exists(l => l.contains("wf-7-1") && l.contains("1/2")), lines.mkString("|"))

  test("a failed node renders an ✗ glyph in the detail overlay"):
    val forest = Forest.empty
      .update(NodeDeclared("e1", "x", None, Nil))
      .update(NodeStarted("e1", "x", "go"))
      .update(NodeFinished("e1", "x", false, "boom"))
    val lines = detailFor("e1", forest)
    assert(lines.exists(l => l.contains("✗") && l.contains("x")), lines.mkString("|"))

  test("active workflows ⇒ a compact notice, not an inline forest"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "hunt", "find bugs", None))
      .update(NodeDeclared("e1", "alpha", Some("g1"), Nil))
      .update(NodeStarted("e1", "alpha", "task A"))
    val lines = renderLive(ChatState.initial.copy(activeWorkflows = Vector("e1" -> forest)))
    // The notice names the census, the menu hint, and the dashboard chord — all
    // on one line at the default width.
    assert(
      lines.exists(l =>
        l.contains("1 workflow running") && l.contains("ctrl+c w to view") && l.contains("ctrl+c w o opens the live dashboard")
      ),
      lines.mkString("|")
    )
    // ...but the forest itself (group markers / names) is not inline.
    assert(!lines.exists(_.contains("▸")), lines.mkString("|"))
    assert(!lines.exists(_.contains("hunt")), lines.mkString("|"))

  test("no active workflows ⇒ no notice"):
    val lines = renderLive(ChatState.initial)
    assert(!lines.exists(_.contains("ctrl+c w to view")), lines.mkString("|"))

  // -- the notice's per-status census -------------------------------------------

  /** A one-node run settled (or paused) into `status` by its terminal event. */
  private def runIn(runId: String, status: RunStatus): (String, Forest) =
    val started = Forest.empty.update(NodeDeclared(runId, "n", None, Nil)).update(NodeStarted(runId, "n", "go"))
    val settled = status match
      case RunStatus.Running => started
      case RunStatus.Paused  => started.update(WorkflowPaused(runId))
      case RunStatus.Done    => started.update(WorkflowFinished(runId, true, "ok"))
      case RunStatus.Failed  => started.update(WorkflowFinished(runId, false, "boom"))
    runId -> settled

  /** The notice line (the one carrying the menu hint), if the live region has one. */
  private def noticeLine(state: ChatState): Option[String] =
    renderLive(state).find(_.contains("ctrl+c w to view"))

  test("the notice counts each status in turn, and its spinner animates while a run is live"):
    val mixed = ChatState.initial.copy(
      activeWorkflows = Vector(runIn("r1", RunStatus.Running), runIn("r2", RunStatus.Done), runIn("r3", RunStatus.Failed))
    )
    val line = noticeLine(mixed).getOrElse(fail("expected a workflow notice"))
    // The leading segment names the noun; the later ones inherit it.
    assert(line.contains("1 workflow running · 1 done · 1 failed"), line)
    // The noun follows its own segment's count.
    val twoLive = mixed.copy(activeWorkflows = mixed.activeWorkflows :+ runIn("r4", RunStatus.Running))
    assert(noticeLine(twoLive).exists(_.contains("2 workflows running · 1 done · 1 failed")), noticeLine(twoLive).toString)
    // A live run keeps the braille spinner sweeping off the render clock.
    assertNotEquals(noticeLine(mixed.copy(clockMs = 0L)), noticeLine(mixed.copy(clockMs = 500L)))

  test("a paused-only set gets a static ◆, not a spinner"):
    val paused = ChatState.initial.copy(activeWorkflows = Vector(runIn("r1", RunStatus.Paused)))
    val line = noticeLine(paused).getOrElse(fail("expected a workflow notice"))
    assert(line.contains("◆") && line.contains("1 workflow paused"), line)
    // Nothing is live, so the line is the same frame at any clock.
    assertEquals(noticeLine(paused.copy(clockMs = 0L)), noticeLine(paused.copy(clockMs = 500L)))

  test("retained runs that have all settled ⇒ no notice at all"):
    // activeWorkflows keeps settled runs so their transcripts stay readable, but
    // the live stack collapses — they are reachable through `ctrl+c w`.
    val settled = ChatState.initial.copy(
      activeWorkflows = Vector(runIn("r1", RunStatus.Done), runIn("r2", RunStatus.Failed))
    )
    assertEquals(noticeLine(settled), None)

  test("a committed eval folds to a summary line, never a workflow forest"):
    val block = Block.Tool("e1", "eval_scala", """{"code":"1 + 1"}""", elapsedMs = Some(0L), output = Some("val res0: Int = 2\n"))
    val lines = render(block)
    assert(lines.exists(_.contains("✻ Executed a code snippet")), lines.mkString("|"))
    assert(!lines.exists(_.contains("▸")), lines.mkString("|")) // no group markers

  // -- queued status -----------------------------------------------------------

  test("a node moves Pending → Queued → Running → Done across its events"):
    val f0 = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    assertEquals(f0.nodes.head.status, NodeStatus.Pending)
    val f1 = f0.update(NodeQueued("r", "a"))
    assertEquals(f1.nodes.head.status, NodeStatus.Queued)
    val f2 = f1.update(NodeStarted("r", "a", "go"))
    assertEquals(f2.nodes.head.status, NodeStatus.Running)
    val f3 = f2.update(NodeFinished("r", "a", true, "ok"))
    assertEquals(f3.nodes.head.status, NodeStatus.Done)

  test("a queued event for a not-yet-declared node still creates it (upsert)"):
    val f = Forest.empty.update(NodeQueued("r", "z"))
    assertEquals(f.nodes.map(_.id), Vector("z"))
    assertEquals(f.nodes.head.status, NodeStatus.Queued)

  test("the detail overlay renders a distinct ◌ glyph for a queued node"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "fan", "fan out", None))
      .update(NodeDeclared("e1", "qnode", Some("g1"), Nil))
      .update(NodeQueued("e1", "qnode"))
    val lines = detailFor("e1", forest)
    val nodeLine = lines.find(_.contains("qnode")).getOrElse("")
    assert(nodeLine.contains("◌"), s"queued node should show ◌, got: '$nodeLine' | all: ${lines.mkString("|")}")
    // The queued glyph is not the pending dot's role nor a terminal glyph.
    assert(!nodeLine.contains("✓") && !nodeLine.contains("✗"), nodeLine)

  test("queued and running nodes render with different glyphs side by side"):
    val forest = Forest.empty
      .update(NodeDeclared("e1", "runner", None, Nil))
      .update(NodeStarted("e1", "runner", "go"))
      .update(NodeDeclared("e1", "waiter", None, Nil))
      .update(NodeQueued("e1", "waiter"))
    val lines = detailFor("e1", forest)
    val waiterLine = lines.find(_.contains("waiter")).getOrElse("")
    val runnerLine = lines.find(_.contains("runner")).getOrElse("")
    assert(waiterLine.contains("◌"), s"waiter: '$waiterLine'")
    assert(!runnerLine.contains("◌"), s"runner must not share the queued glyph: '$runnerLine'")

  // -- transcript folding (Activity events) -----------------------------------

  private def saidTexts(t: Transcript): Vector[String] =
    t.items.collect { case TranscriptItem.Said(x) => x }

  test("Activity events accumulate per (run, node): prose/thoughts grow, a tool call fills its output"):
    val s = ChatState.initial
      .applyActivity(TranscriptEvent.Said("r", "n", "hello "))
      .applyActivity(TranscriptEvent.Said("r", "n", "world"))
      .applyActivity(TranscriptEvent.Thought("r", "n", "hmm "))
      .applyActivity(TranscriptEvent.Thought("r", "n", "ok"))
      .applyActivity(TranscriptEvent.ToolCalled("r", "n", "c1", "read", "{}"))
      .applyActivity(TranscriptEvent.ToolReturned("r", "n", "c1", "result", isError = false))
    val t = s.transcripts(("r", "n"))
    assertEquals(saidTexts(t), Vector("hello world"))
    assertEquals(t.items.collect { case TranscriptItem.Thought(x) => x }, Vector("hmm ok"))
    assertEquals(
      t.items.collectFirst { case tc: TranscriptItem.ToolCall => (tc.tool, tc.output, tc.isError) },
      Some(("read", Some("result"), false))
    )

  test("Activity deltas for different nodes and runs do not cross-contaminate"):
    val s = ChatState.initial
      .applyActivity(TranscriptEvent.Said("r1", "a", "A1"))
      .applyActivity(TranscriptEvent.Said("r1", "b", "B1"))
      .applyActivity(TranscriptEvent.Said("r2", "a", "A2"))
    assertEquals(s.transcripts.size, 3)
    assertEquals(saidTexts(s.transcripts(("r1", "a"))), Vector("A1"))
    assertEquals(saidTexts(s.transcripts(("r1", "b"))), Vector("B1"))
    assertEquals(saidTexts(s.transcripts(("r2", "a"))), Vector("A2"))

  test("re-admitting an interrupted node drops that node's transcript, leaving others"):
    val s0 = ChatState.initial
      .applyOrchestration(NodeDeclared("r", "a", None, Nil))
      .applyOrchestration(NodeDeclared("r", "b", None, Nil))
      .applyOrchestration(NodeStarted("r", "a", "task a"))
      .applyActivity(TranscriptEvent.Said("r", "a", "first attempt"))
      .applyActivity(TranscriptEvent.Said("r", "b", "sibling"))
      .applyOrchestration(NodeInterrupted("r", "a"))
    // A resumed run re-runs `a` from scratch: its stale transcript is discarded.
    val s1 = s0.applyOrchestration(NodeStarted("r", "a", "task a again"))
    assert(!s1.transcripts.contains(("r", "a")), s1.transcripts.keys.toString)
    assert(s1.transcripts.contains(("r", "b")))

  test("WorkflowFinished settles Failed on ok=false and ignores an unknown run"):
    val settled = ChatState.initial
      .applyOrchestration(NodeDeclared("e1", "a", None, Nil))
      .applyOrchestration(WorkflowFinished("e1", false, "boom"))
    assertEquals(settled.activeWorkflows.collectFirst { case (id, f) if id == "e1" => f.status }, Some(RunStatus.Failed))
    // A finish for a run we never tracked creates no phantom settled forest.
    val phantom = ChatState.initial.applyOrchestration(WorkflowFinished("ghost", true, "done"))
    assertEquals(phantom.activeWorkflows, Vector.empty)

  test("a 6th settled run evicts the oldest settled run and its transcripts; a running run is never evicted"):
    val withRunning = ChatState.initial
      .applyOrchestration(NodeDeclared("r0", "n", None, Nil))
      .applyOrchestration(NodeStarted("r0", "n", "go")) // stays running throughout
    val filled = (1 to 6).foldLeft(withRunning) { (st, i) =>
      st.applyOrchestration(NodeDeclared(s"r$i", "n", None, Nil))
        .applyActivity(TranscriptEvent.Said(s"r$i", "n", s"work $i"))
        .applyOrchestration(WorkflowFinished(s"r$i", true, "ok"))
    }
    // Six settled runs exceed the cap of 5: the oldest settled (r1) is evicted with
    // its transcript; the older-but-running r0 survives.
    assertEquals(filled.activeWorkflows.map(_._1), Vector("r0", "r2", "r3", "r4", "r5", "r6"))
    assert(!filled.transcripts.contains(("r1", "n")), filled.transcripts.keys.toString)
    assert(filled.transcripts.contains(("r2", "n")))

  test("switchedTo clears accumulated transcripts and workflows"):
    val snapshot = SessionSnapshot(SessionSummary.from("s", None, Nil), Nil)
    val loaded = ChatState.initial
      .applyActivity(TranscriptEvent.Said("r", "n", "hello"))
      .applyOrchestration(NodeDeclared("r", "n", None, Nil))
      .switchedTo(snapshot)
    assertEquals(loaded.transcripts, Map.empty[(String, String), Transcript])
    assertEquals(loaded.activeWorkflows, Vector.empty)

  // -- node cursor + transcript navigation ------------------------------------

  test("displayNodes orders declared groups first (in order), ungrouped last"):
    val forest = Forest.empty
      .update(GroupDeclared("r", "g1", "one", "", None))
      .update(GroupDeclared("r", "g2", "two", "", None))
      .update(NodeDeclared("r", "free", None, Nil))        // ungrouped, declared first
      .update(NodeDeclared("r", "b2", Some("g2"), Nil))
      .update(NodeDeclared("r", "a1", Some("g1"), Nil))
      .update(NodeDeclared("r", "a1b", Some("g1"), Nil))
    // g1's nodes (declaration order), then g2's, then the ungrouped node last.
    assertEquals(ChatState.displayNodes(forest).map(_.id), Vector("a1", "a1b", "b2", "free"))

  private def detailState(runId: String, forest: Forest, cursor: Int): ChatState =
    ChatState.initial.copy(activeWorkflows = Vector(runId -> forest), overlay = Overlay.WorkflowDetail(runId, cursor))

  test("openSelectedWorkflow opens the detail with the cursor at 0"):
    val forest = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    val listOpen = ChatState.initial.copy(activeWorkflows = Vector("r" -> forest), overlay = Overlay.WorkflowList(0))
    assertEquals(listOpen.openSelectedWorkflow.overlay, Overlay.WorkflowDetail("r", 0))

  test("`o` on the workflow page opens the live dashboard once its URL is known"):
    val forest = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    val listOpen = ChatState.initial.copy(activeWorkflows = Vector("r" -> forest), overlay = Overlay.WorkflowList(0))
    assertEquals(keyEvent(listOpen, Key.Char('o')), Some(Event.WorkflowOpenDashboard))
    // The page's key legend advertises the binding.
    assert(listFor(Vector("r" -> forest)).exists(_.contains("o dashboard")))
    // The URL arrives as its own event — stored, never shown as a sticky notice.
    val (ready, _) = app.update(Event.Inbound1(AgentEvent.Dashboard("http://localhost:7777")), listOpen)
    assertEquals(ready.dashboardUrl, Some("http://localhost:7777"))
    assert(ready.notices.isEmpty, ready.notices.mkString("|"))
    // The browser lands on the run itself: the dashboard reads the hash fragment
    // as the run to select.
    assertEquals(ready.dashboardTarget, Some("http://localhost:7777/#r"))
    // With the URL known, `o` fires the browser opener (not executed here) and
    // leaves the state alone; before it arrives, `o` is inert.
    val (opened, openCmd) = app.update(Event.WorkflowOpenDashboard, ready)
    assertEquals(opened, ready)
    openCmd match
      case Cmd.Fire(_) => ()
      case other       => fail(s"expected Cmd.Fire, got $other")
    assertEquals(app.update(Event.WorkflowOpenDashboard, listOpen)._2, Cmd.none)

  test("the dashboard opens focused on the latest running run"):
    def runs(entries: (String, RunStatus)*): ChatState =
      ChatState.initial.copy(
        dashboardUrl = Some("http://localhost:7777"),
        activeWorkflows = entries.toVector.map((id, status) => runIn(id, status))
      )
    // Insertion order stands in for start order, so the last running run wins.
    assertEquals(runs("r1" -> RunStatus.Running, "r2" -> RunStatus.Done, "r3" -> RunStatus.Running).dashboardRun, Some("r3"))
    // A newer run that already settled never steals focus from a live one.
    assertEquals(runs("r1" -> RunStatus.Running, "r2" -> RunStatus.Done).dashboardRun, Some("r1"))
    // Nothing running: the most recent run overall, whatever became of it.
    assertEquals(runs("r1" -> RunStatus.Failed, "r2" -> RunStatus.Paused).dashboardRun, Some("r2"))
    assertEquals(runs("r1" -> RunStatus.Failed, "r2" -> RunStatus.Paused).dashboardTarget, Some("http://localhost:7777/#r2"))
    // No runs at all: the bare dashboard URL, unfocused.
    assertEquals(runs().dashboardRun, None)
    assertEquals(runs().dashboardTarget, Some("http://localhost:7777"))
    // A trailing slash on the server's URL doesn't double up before the fragment.
    val slashed = runs("r1" -> RunStatus.Running).copy(dashboardUrl = Some("http://localhost:7777/"))
    assertEquals(slashed.dashboardTarget, Some("http://localhost:7777/#r1"))
    // Still nothing to open before the dashboard server reports its URL.
    assertEquals(runs("r1" -> RunStatus.Running).copy(dashboardUrl = None).dashboardTarget, None)

  test("moveWorkflowCursor clamps at both ends; no-op on an empty forest"):
    val forest = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeDeclared("r", "b", None, Nil))
      .update(NodeDeclared("r", "c", None, Nil))
    val open = detailState("r", forest, 0)
    assertEquals(open.moveWorkflowCursor(-1).overlay, Overlay.WorkflowDetail("r", 0))
    assertEquals(open.moveWorkflowCursor(1).overlay, Overlay.WorkflowDetail("r", 1))
    assertEquals(open.moveWorkflowCursor(10).overlay, Overlay.WorkflowDetail("r", 2))
    assertEquals(detailState("r", Forest.empty, 0).moveWorkflowCursor(1).overlay, Overlay.WorkflowDetail("r", 0))

  test("openSelectedNode targets the display-order node (groups first); no-op on an empty forest"):
    val forest = Forest.empty
      .update(GroupDeclared("r", "g1", "phase one", "", None))
      .update(NodeDeclared("r", "u", None, Nil))        // ungrouped, declared first
      .update(NodeDeclared("r", "g", Some("g1"), Nil))  // grouped → displays first
    assertEquals(detailState("r", forest, 0).openSelectedNode.overlay, Overlay.WorkflowTranscript("r", "g", 0))
    assertEquals(detailState("r", forest, 1).openSelectedNode.overlay, Overlay.WorkflowTranscript("r", "u", 0))
    // out of range / empty forest: no-op
    assertEquals(detailState("r", Forest.empty, 0).openSelectedNode.overlay, Overlay.WorkflowDetail("r", 0))

  test("backToWorkflowDetail restores the cursor to the node's display index; vanished node → 0"):
    val forest = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeDeclared("r", "b", None, Nil))
      .update(NodeDeclared("r", "c", None, Nil))
    val onC = ChatState.initial.copy(activeWorkflows = Vector("r" -> forest), overlay = Overlay.WorkflowTranscript("r", "c", 5))
    assertEquals(onC.backToWorkflowDetail.overlay, Overlay.WorkflowDetail("r", 2))
    val vanished = onC.copy(overlay = Overlay.WorkflowTranscript("r", "zzz", 0))
    assertEquals(vanished.backToWorkflowDetail.overlay, Overlay.WorkflowDetail("r", 0))

  test("scrollTranscript moves the bottom-anchored offset (floored at 0); followTranscript re-pins to the tail"):
    val tail = ChatState.initial.copy(overlay = Overlay.WorkflowTranscript("r", "n", 0)) // offset 0 = following
    val up = tail.scrollTranscript(1) // one row older
    assertEquals(up.overlay, Overlay.WorkflowTranscript("r", "n", 1))
    val up3 = up.scrollTranscript(2)
    assertEquals(up3.overlay, Overlay.WorkflowTranscript("r", "n", 3))
    // stepping back past the tail floors the offset at 0
    assertEquals(up3.scrollTranscript(-10).overlay, Overlay.WorkflowTranscript("r", "n", 0))
    // follow snaps straight back to the tail
    assertEquals(up3.followTranscript.overlay, Overlay.WorkflowTranscript("r", "n", 0))

  // -- wrap helper ------------------------------------------------------------

  test("wrap: empty text yields a single empty line"):
    assertEquals(ChatApp.wrap("", 10), Vector(""))

  test("wrap: greedy split on spaces"):
    assertEquals(ChatApp.wrap("the quick brown fox", 9), Vector("the quick", "brown fox"))

  test("wrap: hard-breaks a token longer than the width"):
    assertEquals(ChatApp.wrap("abcdefghij", 4), Vector("abcd", "efgh", "ij"))

  test("wrap: preserves explicit newlines"):
    assertEquals(ChatApp.wrap("a\nb", 10), Vector("a", "b"))

  test("wrap: CJK counts as double width"):
    // Three CJK glyphs = 6 columns; at width 4 two fit per line.
    assertEquals(ChatApp.wrap("你好吗", 4), Vector("你好", "吗"))

  test("wrap: preserves leading indentation on every wrapped row"):
    // Regression (review Finding 2): printed code must keep its indentation.
    val rows = ChatApp.wrap("  alpha beta gamma delta", 12)
    assert(rows.length > 1, rows.toString)
    assert(rows.forall(_.startsWith("  ")), rows.toString)
    assert(rows.forall(_.length <= 12), rows.toString)

  test("wrap: clamps a runaway indent instead of collapsing the content column"):
    val rows = ChatApp.wrap((" " * 40) + "x y z", 12)
    // The 40-space indent is clamped to width - 8 = 4 spaces; content still fits.
    assert(rows.forall(_.length <= 12), rows.toString)
    assert(rows.head.startsWith("    x"), rows.head)     // exactly 4 spaces, then content
    assert(!rows.head.startsWith("     "), rows.head)    // not 5

  // -- transcript overlay rendering -------------------------------------------

  test("the transcript overlay renders prose, thoughts, and a completed tool call"):
    val t = Transcript.empty
      .update(TranscriptEvent.Thought("r", "n", "let me think"))
      .update(TranscriptEvent.Said("r", "n", "here is the answer"))
      .update(TranscriptEvent.ToolCalled("r", "n", "c1", "read", """{"path":"a.scala"}"""))
      .update(TranscriptEvent.ToolReturned("r", "n", "c1", "ok", isError = false))
    val forest = Forest.empty.update(NodeDeclared("r", "n", None, Nil)).update(NodeStarted("r", "n", "do the thing"))
    val lines = transcriptLines("r", "n", forest, t)
    assert(lines.exists(l => l.contains("r / n") && l.contains("running")), lines.mkString("|")) // header
    assert(lines.exists(_.contains("do the thing")), lines.mkString("|"))                        // prompt above
    assert(lines.exists(_.contains("let me think")), lines.mkString("|"))
    assert(lines.exists(_.contains("here is the answer")), lines.mkString("|"))
    assert(lines.exists(l => l.contains("▸") && l.contains("read") && l.contains("✓")), lines.mkString("|"))

  test("the transcript overlay shows an empty state and marks a failed tool with its output"):
    val forest = Forest.empty.update(NodeDeclared("r", "n", None, Nil))
    val empty = transcriptLines("r", "n", forest, Transcript.empty)
    assert(empty.exists(_.contains("(no activity yet)")), empty.mkString("|"))
    val t = Transcript.empty
      .update(TranscriptEvent.ToolCalled("r", "n", "c1", "bash", "{}"))
      .update(TranscriptEvent.ToolReturned("r", "n", "c1", "boom happened", isError = true))
    val failed = transcriptLines("r", "n", forest, t)
    assert(failed.exists(_.contains("✗")), failed.mkString("|"))         // ✗ verdict
    assert(failed.exists(_.contains("boom happened")), failed.mkString("|"))  // error output

  test("the transcript overlay follows the tail at offset 0: newest lines show, oldest scroll off"):
    val body = (1 to 40).map(i => s"line-$i").mkString("\n")
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "n", body))
    val forest = Forest.empty.update(NodeDeclared("r", "n", None, Nil))
    val lines = transcriptLines("r", "n", forest, t, offset = 0, width = 100, rows = 20)
    assert(lines.exists(_.contains("line-40")), lines.mkString("|"))
    assert(!lines.exists(_.contains("line-5")), "following should hide the early lines")
    assert(lines.exists(_.contains("of 40")), lines.mkString("|")) // range indicator

  test("the transcript overlay shows the top when the offset exceeds the content"):
    val body = (1 to 40).map(i => s"line-$i").mkString("\n")
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "n", body))
    val forest = Forest.empty.update(NodeDeclared("r", "n", None, Nil))
    // A huge offset clamps at render to the top of the transcript.
    val lines = transcriptLines("r", "n", forest, t, offset = 1000, width = 100, rows = 20)
    assert(lines.exists(_.contains("line-1")), lines.mkString("|"))
    assert(!lines.exists(_.contains("line-40")), "a top-pinned view keeps the tail hidden")

  test("the transcript overlay reveals exactly one older row on the first step up from the tail"):
    // Regression (review Finding 1): unpinning from the live tail must not teleport
    // to the top. With bottom-anchored offset, offset 1 shows one row above the tail.
    val body = (1 to 40).map(i => s"line-$i").mkString("\n")
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "n", body))
    val forest = Forest.empty.update(NodeDeclared("r", "n", None, Nil))
    val lines = transcriptLines("r", "n", forest, t, offset = 1, width = 100, rows = 20)
    // rows=20 → bodyRows=12; offset 1 shows lines 28..39 (line-40 just scrolled off).
    assert(lines.exists(_.contains("line-39")), lines.mkString("|"))
    assert(!lines.exists(_.contains("line-40")), "one step up hides only the newest row")
    assert(lines.exists(_.contains("line-28")), lines.mkString("|"))
    assert(!lines.exists(_.contains("line-1")), "one step up must not jump to the top")

  test("the transcript overlay falls back when the node or run is gone"):
    // Run present, node absent → the node-level fallback.
    val nodeGone = transcriptLines("r", "missing", Forest.empty.update(NodeDeclared("r", "x", None, Nil)), Transcript.empty)
    assert(nodeGone.exists(_.contains("Transcript no longer available")), nodeGone.mkString("|"))
    // Run evicted entirely → the workflow-finished fallback.
    val runGone = fullscreenLines(ChatState.initial.copy(overlay = Overlay.WorkflowTranscript("gone", "n", 0)), 100, 30)
    assert(runGone.exists(_.contains("This workflow has finished")), runGone.mkString("|"))

  // -- fullscreen padding geometry --------------------------------------------

  // Mirrors of ChatApp's private fullscreen geometry. Retuning one there means
  // retuning it here; the tests derive their viewport sizes from these rather
  // than hard-coding the resulting widths.
  private val Gutter = 3            // FsGutter
  private val MinGutterWidth = 20   // FsMinGutterWidth: narrower drops the gutters
  private val MinPaddedRows = 6     // FsMinPaddedRows: shorter drops the pad rows
  private val TwoPaneMinBody = 88   // TwoPaneMinWidth, measured against the inner width

  /** Where the header bar lands on a frame tall enough for the top padding row. */
  private val HeaderRow = 1

  private def busyForest: Forest =
    (1 to 40).foldLeft(Forest.empty)((f, i) => f.update(NodeDeclared("r", f"n$i%02d", None, Nil)))

  test("body content is inset by the gutter on both sides, the bars aligned with it"):
    val lines = listFor(Vector("r1" -> Forest.empty.update(NodeDeclared("r1", "a", None, Nil))), width = 72)
    val row = lines.find(_.contains("r1")).getOrElse(fail(s"no run row: ${lines.mkString("|")}"))
    assertEquals(row.take(Gutter), " " * Gutter, row)
    assert(row.drop(Gutter).startsWith("›"), row)
    // Header and footer text start at the same column as the body.
    assertEquals(lines(HeaderRow).take(Gutter), " " * Gutter, lines(HeaderRow))
    assert(lines(HeaderRow).drop(Gutter).startsWith("Workflows"), lines(HeaderRow))
    assert(lines.last.drop(Gutter).startsWith("↑/↓"), lines.last)
    // Nothing reaches either edge.
    assert(lines.forall(l => l.isEmpty || l.take(Gutter).isBlank), lines.mkString("|"))
    assert(lines.forall(l => l.isEmpty || l.takeRight(Gutter).isBlank), lines.mkString("|"))

  test("blank padding rows sit above the header, under it, and above the footer"):
    val lines = listFor(Vector("r1" -> Forest.empty.update(NodeDeclared("r1", "a", None, Nil))), width = 72)
    assert(lines.head.isBlank, s"row above the header must be padding: '${lines.head}'")
    assert(lines(HeaderRow).contains("Workflows"), s"the header follows it: '${lines(HeaderRow)}'")
    assert(lines(HeaderRow + 1).isBlank, s"row under the header must be padding: '${lines(HeaderRow + 1)}'")
    assert(lines(lines.length - 2).isBlank, s"row above the footer must be padding: '${lines(lines.length - 2)}'")
    // The footer alone stays flush with the frame's bottom edge.
    assert(lines.last.trim.nonEmpty, s"the footer is not padded away from the bottom: '${lines.last}'")

  test("the frame is exactly viewport.rows lines at every size, padding or not"):
    val wf = Vector("r1" -> busyForest)
    for (w, r) <- Vector((72, 24), (72, 12), (100, 6), (40, 5), (18, 4), (12, 3), (8, 2)) do
      val lines = fullscreenLines(ChatState.initial.copy(activeWorkflows = wf, overlay = Overlay.WorkflowList(0)), w, r)
      assertEquals(lines.length, r, s"at ${w}x$r: ${lines.mkString("|")}")
      assert(lines.forall(l => Width.stringWidth(l) <= w), s"at ${w}x$r a line overflowed: ${lines.mkString("|")}")

  /** The list view over enough runs to fill any body, at one viewport size. */
  private def listAt(width: Int, rows: Int): Vector[String] =
    fullscreenLines(
      ChatState.initial.copy(
        activeWorkflows = (1 to 40).toVector.map(i => f"r$i%02d" -> Forest.empty),
        overlay = Overlay.WorkflowList(0)
      ),
      width, rows
    )

  test("the gutter cutoff: kept at the threshold width, dropped one column below"):
    val padded = listAt(MinGutterWidth, 10)
    assertEquals(padded.length, 10)
    assert(padded.forall(l => Width.stringWidth(l) <= MinGutterWidth), padded.mkString("|"))
    assert(padded(HeaderRow).startsWith(" " * Gutter), s"gutter expected: '${padded(HeaderRow)}'")
    // One column narrower the gutters collapse rather than squeezing the content.
    val bare = listAt(MinGutterWidth - 1, 10)
    assertEquals(bare.length, 10)
    assert(bare.forall(l => Width.stringWidth(l) <= MinGutterWidth - 1), bare.mkString("|"))
    assert(bare(HeaderRow).startsWith("Workflows"), s"no gutter expected: '${bare(HeaderRow)}'")

  test("the padding rows drop one step at a time as the frame shrinks"):
    // Tallest: the title floats a row down and the body keeps its own padding.
    val all = listAt(72, MinPaddedRows + 1)
    assertEquals(all.length, MinPaddedRows + 1)
    assert(all.forall(l => Width.stringWidth(l) <= 72), all.mkString("|"))
    assert(all.head.isBlank, s"top pad expected: '${all.head}'")
    assert(all(HeaderRow).contains("Workflows"), all(HeaderRow))
    assert(all(HeaderRow + 1).isBlank, s"pad row under the header expected: '${all(HeaderRow + 1)}'")
    assert(all(all.length - 2).isBlank, s"pad row above the footer expected: '${all(all.length - 2)}'")

    // One row shorter: the TOP pad is the first to go, leaving today's layout —
    // the title back at row 0, the body still padded on both sides.
    val padded = listAt(72, MinPaddedRows)
    assertEquals(padded.length, MinPaddedRows)
    assert(padded.forall(l => Width.stringWidth(l) <= 72), padded.mkString("|"))
    assert(padded.head.contains("Workflows"), s"the title returns to the top edge: '${padded.head}'")
    assert(padded(1).isBlank, s"pad row under the header expected: '${padded(1)}'")
    assert(padded(padded.length - 2).isBlank, s"pad row above the footer expected: '${padded(padded.length - 2)}'")
    // Two content rows survive: 6 rows = header + pad + 2 body + pad + footer.
    assertEquals(padded.slice(2, 4).count(_.trim.nonEmpty), 2, padded.mkString("|"))

    // Shorter still and the body pads go too, so the rows go to content.
    val tight = listAt(72, MinPaddedRows - 1)
    assertEquals(tight.length, MinPaddedRows - 1)
    assert(tight.head.contains("Workflows"), tight.head)
    assert(tight(1).trim.nonEmpty, s"no pad row expected, content should follow the header: '${tight(1)}'")
    assert(tight.last.contains("↑/↓"), tight.last)

  test("the scroll range counts against the padded body height, not the raw frame"):
    // 24 rows = 1 top pad + 1 header + 1 pad + 19 body + 1 pad + 1 footer.
    val lines = fullscreenLines(
      ChatState.initial.copy(activeWorkflows = (1 to 40).toVector.map(i => f"r$i%02d" -> Forest.empty),
        overlay = Overlay.WorkflowList(0)), 72, 24)
    val body = lines.slice(HeaderRow + 2, lines.length - 2)
    assertEquals(body.length, 19)
    val listed = body.count(_.trim.nonEmpty)
    assert(lines.last.contains(s"1-$listed of 40"), s"footer range must match the ${listed} rendered rows: ${lines.last}")

  // -- two-pane detail preview + list tags ------------------------------------

  test("the detail overlay shows a two-pane transcript preview on a wide terminal"):
    val forest = Forest.empty
      .update(GroupDeclared("r", "g1", "hunt", "", None))
      .update(NodeDeclared("r", "alpha", Some("g1"), Nil))
      .update(NodeStarted("r", "alpha", "task alpha"))
      .update(NodeDeclared("r", "beta", Some("g1"), Nil))
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "alpha", "preview body text"))
    val lines = detailLines("r", forest, Map(("r", "alpha") -> t), cursor = 0, width = 120, rows = 30)
    assert(lines.exists(_.contains("alpha")), lines.mkString("|"))              // left forest pane
    assert(lines.exists(_.contains("preview body text")), lines.mkString("|")) // right preview pane
    assert(lines.exists(l => l.contains("alpha") && l.contains("running")), lines.mkString("|")) // preview header

  test("the two-pane split is gated on the padded body width, not the viewport"):
    val forest = Forest.empty
      .update(NodeDeclared("r", "solo", None, Nil))
      .update(NodeStarted("r", "solo", "go"))
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "solo", "preview marker"))
    def hasPreview(width: Int): Boolean =
      detailLines("r", forest, Map(("r", "solo") -> t), cursor = 0, width = width, rows = 24)
        .exists(_.contains("preview marker"))
    // The split needs TwoPaneMinWidth columns of BODY, so the viewport must carry
    // that plus both gutters — derived, so retuning the gutter moves this with it.
    val boundary = TwoPaneMinBody + 2 * Gutter
    assert(hasPreview(boundary), s"$boundary columns leaves exactly $TwoPaneMinBody for the split")
    assert(!hasPreview(boundary - 1), "one column short of a full-width split stays single-pane")

  test("the detail overlay degrades to a single forest pane on a narrow terminal"):
    val forest = Forest.empty
      .update(NodeDeclared("r", "solo", None, Nil))
      .update(NodeStarted("r", "solo", "go"))
    val t = Transcript.empty.update(TranscriptEvent.Said("r", "solo", "hidden preview"))
    val lines = detailLines("r", forest, Map(("r", "solo") -> t), cursor = 0, width = 70, rows = 24)
    assert(lines.exists(_.contains("solo")), lines.mkString("|"))
    assert(!lines.exists(_.contains("hidden preview")), "the narrow layout carries no preview")

  test("the list overlay tags settled runs done / failed and keeps them listed"):
    val done = Forest.empty.update(NodeDeclared("r1", "a", None, Nil)).update(WorkflowFinished("r1", true, "ok"))
    val failed = Forest.empty.update(NodeDeclared("r2", "a", None, Nil)).update(WorkflowFinished("r2", false, "boom"))
    val lines = listFor(Vector("r1" -> done, "r2" -> failed))
    assert(lines.exists(l => l.contains("r1") && l.contains("done")), lines.mkString("|"))
    assert(lines.exists(l => l.contains("r2") && l.contains("failed")), lines.mkString("|"))

  // -- fullscreen takeover ----------------------------------------------------

  test("workflow views take over the screen (Screen.fullscreen set, overlay empty); other overlays stay inline"):
    val forest = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    val base = ChatState.initial.copy(activeWorkflows = Vector("r" -> forest))
    val workflowOverlays = Vector(Overlay.WorkflowList(0), Overlay.WorkflowDetail("r", 0), Overlay.WorkflowTranscript("r", "a", 0))
    workflowOverlays.foreach: ov =>
      val screen = app.view(base.copy(overlay = ov), Viewport(100, 30))
      assert(screen.fullscreen.isDefined, s"expected fullscreen for $ov")
      assert(screen.overlay.isEmpty, s"expected no inline overlay for $ov")
    // A non-workflow overlay is an inline overlay, never fullscreen.
    assert(app.view(ChatState.initial.showKeyBindings, Viewport(100, 30)).fullscreen.isEmpty, "keybindings must stay inline")

  test("a fullscreen workflow view fills exactly the viewport height"):
    val forest = Forest.empty.update(NodeDeclared("r", "a", None, Nil)).update(NodeStarted("r", "a", "go"))
    val st = ChatState.initial.copy(activeWorkflows = Vector("r" -> forest), overlay = Overlay.WorkflowDetail("r", 0))
    assertEquals(fullscreenLines(st, 100, 24).length, 24)
    assertEquals(fullscreenLines(st, 60, 12).length, 12)

  // -- key dispatch -----------------------------------------------------------

  test("detail keys move the cursor, open a transcript, pause/resume, and go back"):
    val st = detailState("r", Forest.empty.update(NodeDeclared("r", "a", None, Nil)), 0)
    assertEquals(keyEvent(st, Key.Up), Some(Event.WorkflowCursorUp))
    assertEquals(keyEvent(st, Key.Down), Some(Event.WorkflowCursorDown))
    assertEquals(keyEvent(st, Key.Enter), Some(Event.WorkflowNodeOpen))
    assertEquals(keyEvent(st, Key.Char('p')), Some(Event.WorkflowPause))
    assertEquals(keyEvent(st, Key.Char('r')), Some(Event.WorkflowResume))
    assertEquals(keyEvent(st, Key.Esc), Some(Event.WorkflowBack))

  test("transcript keys scroll, follow (End / g / G), and go back"):
    val st = ChatState.initial.copy(overlay = Overlay.WorkflowTranscript("r", "n", 0))
    // The ±1 arrows route through the parameterized scroll event: Up reveals one
    // row older (offset +1), Down moves one row newer (offset -1).
    assertEquals(keyEvent(st, Key.Up), Some(Event.WorkflowTranscriptScroll(1)))
    assertEquals(keyEvent(st, Key.Down), Some(Event.WorkflowTranscriptScroll(-1)))
    assertEquals(keyEvent(st, Key.End), Some(Event.WorkflowFollow))
    assertEquals(keyEvent(st, Key.Char('g')), Some(Event.WorkflowFollow))
    assertEquals(keyEvent(st, Key.Char('G')), Some(Event.WorkflowFollow))
    assertEquals(keyEvent(st, Key.Esc), Some(Event.WorkflowBack))

  test("WorkflowBack steps transcript → detail, then detail → list"):
    val forest = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    val onTranscript = ChatState.initial.copy(
      activeWorkflows = Vector("r" -> forest),
      overlay = Overlay.WorkflowTranscript("r", "a", 0)
    )
    val (toDetail, _) = app.update(Event.WorkflowBack, onTranscript)
    assertEquals(toDetail.overlay, Overlay.WorkflowDetail("r", 0))
    val (toList, _) = app.update(Event.WorkflowBack, toDetail)
    assert(toList.overlay.isInstanceOf[Overlay.WorkflowList], toList.overlay.toString)
