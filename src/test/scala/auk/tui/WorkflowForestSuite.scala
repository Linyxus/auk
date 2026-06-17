package auk.tui

import gears.async.UnboundedChannel

import auk.tui.app.Layout
import auk.agent.{AgentEvent, UserCommand}
import auk.workflow.{Forest, NodeStatus, OrchestrationEvent}

/** Phase-3 TUI: folding orchestration events into the eval card's agent forest
  * and rendering it grouped by group with per-node status. */
class WorkflowForestSuite extends munit.FunSuite:

  import OrchestrationEvent.*

  private def app: ChatApp =
    ChatApp(UnboundedChannel[AgentEvent]().asReadable, UnboundedChannel[UserCommand](), UnboundedChannel[Unit]())

  private def render(block: Block, width: Int = 80): Vector[String] =
    val state = ChatState.initial.copy(history = Vector(Entry.Assistant(Vector(block))))
    app.view(state).committed.flatMap(Layout.lay(_, width)).map(_.plain)

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

  test("applyOrchestration routes events to the eval tool whose id is the runId"):
    val streaming = ChatState.initial.copy(phase = Phase.Streaming(Vector(Block.Tool("e1", "eval_scala", "{}"))))
    val updated = streaming
      .applyOrchestration(GroupDeclared("e1", "g1", "hunt", "d", None))
      .applyOrchestration(NodeDeclared("e1", "a", Some("g1"), Nil))
      .applyOrchestration(NodeStarted("e1", "a", "task A"))
    updated.phase match
      case Phase.Streaming(blocks, _) =>
        val tool = blocks.collectFirst { case t: Block.Tool if t.id == "e1" => t }
        assert(tool.flatMap(_.forest).exists(_.groups.map(_.name).contains("hunt")), tool.toString)
        assert(tool.flatMap(_.forest).exists(_.nodes.exists(n => n.id == "a" && n.status == NodeStatus.Running)), tool.toString)
      case other => fail(s"expected streaming, got $other")

  test("events for an unrelated runId leave the tool block untouched"):
    val streaming = ChatState.initial.copy(phase = Phase.Streaming(Vector(Block.Tool("e1", "eval_scala", "{}"))))
    val updated = streaming.applyOrchestration(GroupDeclared("other", "g1", "hunt", "d", None))
    updated.phase match
      case Phase.Streaming(blocks, _) =>
        assert(blocks.collectFirst { case t: Block.Tool => t }.flatMap(_.forest).isEmpty)
      case other => fail(s"expected streaming, got $other")

  test("the eval card renders the forest grouped by group, with status glyphs"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "hunt", "find bugs", None))
      .update(NodeDeclared("e1", "alpha", Some("g1"), Nil))
      .update(NodeStarted("e1", "alpha", "task A"))
      .update(NodeFinished("e1", "alpha", true, "ok"))
      .update(NodeDeclared("e1", "beta", Some("g1"), Nil))
      .update(NodeStarted("e1", "beta", "task B"))
    val block = Block.Tool("e1", "eval_scala", """{"code":"wf.start[String](...)"}""", elapsedMs = Some(0L), forest = Some(forest))
    val lines = render(block)
    assert(lines.exists(_.contains("hunt")), lines.mkString("|"))
    assert(lines.exists(l => l.contains("✓") && l.contains("alpha")), lines.mkString("|"))
    assert(lines.exists(_.contains("beta")), lines.mkString("|"))

  test("a failed node renders an ✗ glyph"):
    val forest = Forest.empty
      .update(NodeDeclared("e1", "x", None, Nil))
      .update(NodeStarted("e1", "x", "go"))
      .update(NodeFinished("e1", "x", false, "boom"))
    val block = Block.Tool("e1", "eval_scala", """{"code":"x"}""", elapsedMs = Some(0L), forest = Some(forest))
    val lines = render(block)
    assert(lines.exists(l => l.contains("✗") && l.contains("x")), lines.mkString("|"))

  test("eval blocks without a forest render exactly as before"):
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

  test("the eval card renders a distinct ◌ glyph for a queued node"):
    val forest = Forest.empty
      .update(GroupDeclared("e1", "g1", "fan", "fan out", None))
      .update(NodeDeclared("e1", "qnode", Some("g1"), Nil))
      .update(NodeQueued("e1", "qnode"))
    val block = Block.Tool("e1", "eval_scala", """{"code":"x"}""", elapsedMs = Some(0L), forest = Some(forest))
    val lines = render(block)
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
    val block = Block.Tool("e1", "eval_scala", """{"code":"x"}""", elapsedMs = Some(0L), forest = Some(forest))
    val lines = render(block)
    val waiterLine = lines.find(_.contains("waiter")).getOrElse("")
    val runnerLine = lines.find(_.contains("runner")).getOrElse("")
    assert(waiterLine.contains("◌"), s"waiter: '$waiterLine'")
    assert(!runnerLine.contains("◌"), s"runner must not share the queued glyph: '$runnerLine'")
