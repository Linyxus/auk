package auk.tui

import gears.async.UnboundedChannel

import auk.tui.app.Layout
import auk.agent.{AgentEvent, UserCommand}
import auk.workflow.{Forest, NodeStatus, OrchestrationEvent}

/** Phase-3 TUI: folding orchestration events into the live workflow panel
  * (above the input box) and rendering it grouped by group with per-node status.
  * Background runs are keyed by run id in `ChatState.activeWorkflows`, no longer
  * attached to the eval card that launched them. */
class WorkflowForestSuite extends munit.FunSuite:

  import OrchestrationEvent.*

  private def app: ChatApp =
    ChatApp(UnboundedChannel[AgentEvent]().asReadable, UnboundedChannel[UserCommand](), UnboundedChannel[Unit](), UnboundedChannel[auk.agent.Inbox]())

  /** Render the committed transcript (history). */
  private def render(block: Block, width: Int = 80): Vector[String] =
    val state = ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(block))))
    app.view(state).committed.flatMap(Layout.lay(_, width)).map(_.plain)

  /** Render the live region (where the workflow panel lives). */
  private def renderLive(state: ChatState, width: Int = 80): Vector[String] =
    Layout.lay(app.view(state).live, width).map(_.plain)

  private def panelFor(runId: String, forest: Forest, width: Int = 80): Vector[String] =
    renderLive(ChatState.initial.copy(activeWorkflows = Vector(runId -> forest)), width)

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

  test("WorkflowFinished drops only its run from the panel; others remain"):
    val updated = ChatState.initial
      .applyOrchestration(NodeDeclared("e1", "a", None, Nil))
      .applyOrchestration(NodeDeclared("e2", "b", None, Nil))
      .applyOrchestration(WorkflowFinished("e1", true, "done"))
    assertEquals(updated.activeWorkflows.map(_._1), Vector("e2"))

  test("the workflow panel renders the forest grouped by group, with status glyphs"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "hunt", "find bugs", None))
      .update(NodeDeclared("e1", "alpha", Some("g1"), Nil))
      .update(NodeStarted("e1", "alpha", "task A"))
      .update(NodeFinished("e1", "alpha", true, "ok"))
      .update(NodeDeclared("e1", "beta", Some("g1"), Nil))
      .update(NodeStarted("e1", "beta", "task B"))
    val lines = panelFor("e1", forest)
    assert(lines.exists(_.contains("workflows")), lines.mkString("|"))
    assert(lines.exists(_.contains("hunt")), lines.mkString("|"))
    assert(lines.exists(l => l.contains("✓") && l.contains("alpha")), lines.mkString("|"))
    assert(lines.exists(_.contains("beta")), lines.mkString("|"))

  test("the panel labels each run with its id and settled/total progress"):
    val forest = Forest.empty
      .update(NodeDeclared("wf-7-1", "a", None, Nil))
      .update(NodeFinished("wf-7-1", "a", true, "ok"))
      .update(NodeDeclared("wf-7-1", "b", None, Nil))
    val lines = panelFor("wf-7-1", forest)
    assert(lines.exists(l => l.contains("wf-7-1") && l.contains("1/2")), lines.mkString("|"))

  test("a failed node renders an ✗ glyph in the panel"):
    val forest = Forest.empty
      .update(NodeDeclared("e1", "x", None, Nil))
      .update(NodeStarted("e1", "x", "go"))
      .update(NodeFinished("e1", "x", false, "boom"))
    val lines = panelFor("e1", forest)
    assert(lines.exists(l => l.contains("✗") && l.contains("x")), lines.mkString("|"))

  test("no active workflows ⇒ no panel"):
    val lines = renderLive(ChatState.initial)
    assert(!lines.exists(_.contains("workflows")), lines.mkString("|"))

  test("the eval card never renders a workflow forest"):
    val block = Block.Tool("e1", "eval_scala", """{"code":"1 + 1"}""", elapsedMs = Some(0L), output = Some("val res0: Int = 2\n"))
    val lines = render(block)
    assert(lines.exists(_.contains("╭─ execution")), lines.mkString("|"))
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

  test("the panel renders a distinct ◌ glyph for a queued node"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "fan", "fan out", None))
      .update(NodeDeclared("e1", "qnode", Some("g1"), Nil))
      .update(NodeQueued("e1", "qnode"))
    val lines = panelFor("e1", forest)
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
    val lines = panelFor("e1", forest)
    val waiterLine = lines.find(_.contains("waiter")).getOrElse("")
    val runnerLine = lines.find(_.contains("runner")).getOrElse("")
    assert(waiterLine.contains("◌"), s"waiter: '$waiterLine'")
    assert(!runnerLine.contains("◌"), s"runner must not share the queued glyph: '$runnerLine'")
