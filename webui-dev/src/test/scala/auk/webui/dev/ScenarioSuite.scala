package auk.webui.dev

import auk.workflow.{Forest, NodeStatus, OrchestrationEvent, Transcript, TranscriptEvent, TranscriptItem, WireCodec, WireMessage}
import OrchestrationEvent.*

class ScenarioSuite extends munit.FunSuite:

  private def orch(s: Scenarios.Script): Vector[OrchestrationEvent] =
    s.collect { case (_, WireMessage.Event(e)) => e }
  private def activity(s: Scenarios.Script): Vector[TranscriptEvent] =
    s.collect { case (_, WireMessage.Activity(e)) => e }

  test("schedule preserves message order and delays, encoding each"):
    val msgs = Vector(
      0 -> WireMessage.Event(NodeDeclared("r", "a", None, Nil)),
      100 -> WireMessage.Activity(TranscriptEvent.Said("r", "a", "hi"))
    )
    val lines = Scenario.schedule(msgs, leadSnapshot = false)
    assertEquals(lines.map(_.delayMs), Vector(0, 100))
    assertEquals(WireCodec.decode(lines.head.data), Right(WireMessage.Event(NodeDeclared("r", "a", None, Nil))))
    assertEquals(WireCodec.decode(lines(1).data), Right(WireMessage.Activity(TranscriptEvent.Said("r", "a", "hi"))))

  test("schedule with leadSnapshot prepends a Snapshot frame at delay 0"):
    val lines = Scenario.schedule(Vector(0 -> WireMessage.Event(NodeDeclared("r", "a", None, Nil))), leadSnapshot = true)
    assertEquals(lines.head.delayMs, 0)
    WireCodec.decode(lines.head.data) match
      case Right(_: WireMessage.Snapshot) => ()
      case other                          => fail(s"expected a Snapshot first, got $other")

  test("every scheduled line of every scenario decodes back to a WireMessage"):
    Scenarios.names.foreach: name =>
      Scenario.schedule(Scenarios.byName(name), leadSnapshot = true).foreach: sl =>
        assert(WireCodec.decode(sl.data).isRight, s"$name: ${sl.data}")

  test("snapshotFrom folds forest events per runId and ignores transcript activity"):
    val msgs = Vector(
      0 -> WireMessage.Event(NodeDeclared("r", "a", None, Nil)),
      0 -> WireMessage.Activity(TranscriptEvent.Said("r", "a", "ignored in snapshot")),
      0 -> WireMessage.Event(NodeStarted("r", "a", "go"))
    )
    Scenario.snapshotFrom(msgs) match
      case WireMessage.Snapshot(List((rid, f))) =>
        assertEquals(rid, "r")
        assertEquals(f.nodes.head.status, NodeStatus.Running)
      case other => fail(s"expected a single-run snapshot, got $other")

  test("byName returns each named fixture and falls back to fanout for unknown names"):
    Scenarios.names.foreach(n => assert(Scenarios.byName(n).nonEmpty, n))
    assertEquals(Scenarios.byName("nonexistent"), Scenarios.byName("fanout"))

  test("the loop fixture has the writer/reviewer dependency chain"):
    val deps = orch(Scenarios.byName("loop")).collect { case NodeDeclared(_, id, _, d) => id -> d }.toMap
    assertEquals(deps.get("reviewer-1"), Some(List("writer-1")))
    assertEquals(deps.get("writer-2"), Some(List("reviewer-1")))
    assertEquals(deps.get("reviewer-3"), Some(List("writer-3")))

  test("the bigFanout fixture has queued and running nodes coexisting mid-run"):
    val evs = orch(Scenarios.byName("bigFanout"))
    val prefix = evs.takeWhile { case _: NodeFinished => false; case _ => true }
    val f = prefix.foldLeft(Forest.empty)((s, e) => s.update(e))
    assert(f.nodes.exists(_.status == NodeStatus.Running), "expected some running nodes")
    assert(f.nodes.exists(_.status == NodeStatus.Queued), "expected some queued nodes")

  test("the failures fixture produces a failed node and a log line"):
    val evs = orch(Scenarios.byName("failures"))
    assert(evs.exists { case NodeFinished(_, _, false, _) => true; case _ => false }, "expected a failure")
    assert(evs.exists { case _: Log => true; case _ => false }, "expected a log")

  test("every fixture ends with all nodes terminal (Done or Failed)"):
    Scenarios.names.foreach: name =>
      val f = orch(Scenarios.byName(name)).foldLeft(Forest.empty)((s, e) => s.update(e))
      val pending = f.nodes.filterNot(n => n.status == NodeStatus.Done || n.status == NodeStatus.Failed)
      assert(pending.isEmpty, s"$name left non-terminal nodes: ${pending.map(_.id)}")

  // -- transcript coverage -----------------------------------------------------

  test("every fixture emits transcript activity for its nodes"):
    Scenarios.names.foreach: name =>
      assert(activity(Scenarios.byName(name)).nonEmpty, s"$name emitted no transcript activity")

  test("every started node in a fixture has at least one transcript event"):
    Scenarios.names.foreach: name =>
      val script = Scenarios.byName(name)
      val started = orch(script).collect { case NodeStarted(_, id, _) => id }.toSet
      val withActivity = activity(script).map(_.nodeId).toSet
      val missing = started.diff(withActivity)
      assert(missing.isEmpty, s"$name: started nodes without transcript: $missing")

  test("folding a node's transcript yields a tool call whose output filled in"):
    val fanoutActivity = activity(Scenarios.byName("fanout")).filter(_.nodeId == "alpha")
    val t = fanoutActivity.foldLeft(Transcript.empty)((s, e) => s.update(e))
    val tools = t.items.collect { case tc: TranscriptItem.ToolCall => tc }
    assert(tools.nonEmpty, "expected a tool call in alpha's transcript")
    assert(tools.forall(_.output.isDefined), "every tool call should have its output filled in by the end")

  test("the failures fixture's bad node has an errored tool result"):
    val badActivity = activity(Scenarios.byName("failures")).filter(_.nodeId == "bad-node")
    val t = badActivity.foldLeft(Transcript.empty)((s, e) => s.update(e))
    assert(t.items.exists { case tc: TranscriptItem.ToolCall => tc.isError; case _ => false }, "expected an errored tool call")

  test("every fixture announces the workflow code (so the code tab has content)"):
    Scenarios.names.foreach: name =>
      val codes = orch(Scenarios.byName(name)).collect { case WorkflowCode(_, c) => c }
      assert(codes.nonEmpty && codes.forall(_.contains("wf.start")), s"$name: missing/odd workflow code: $codes")

  test("every eval_scala input is valid JSON with a string code field (UI can extract it)"):
    val inputs = Scenarios.names
      .flatMap(n => activity(Scenarios.byName(n)))
      .collect { case TranscriptEvent.ToolCalled(_, _, _, "eval_scala", input) => input }
    assert(inputs.nonEmpty, "expected some eval_scala calls")
    inputs.foreach: input =>
      val d = scala.scalajs.js.JSON.parse(input).asInstanceOf[scala.scalajs.js.Dynamic]
      assert(scala.scalajs.js.typeOf(d.code) == "string", s"eval_scala input lacks a code string: $input")
