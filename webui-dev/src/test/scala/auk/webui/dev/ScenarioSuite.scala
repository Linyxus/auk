package auk.webui.dev

import auk.workflow.{Forest, NodeStatus, OrchestrationEvent, WireCodec, WireMessage}
import OrchestrationEvent.*

class ScenarioSuite extends munit.FunSuite:

  test("schedule preserves event order and delays, wrapping each as an Event"):
    val evs = Vector(0 -> NodeDeclared("r", "a", None, Nil), 100 -> NodeStarted("r", "a", "go"))
    val lines = Scenario.schedule(evs, leadSnapshot = false)
    assertEquals(lines.map(_.delayMs), Vector(0, 100))
    assertEquals(WireCodec.decode(lines.head.data), Right(WireMessage.Event(NodeDeclared("r", "a", None, Nil))))

  test("schedule with leadSnapshot prepends a Snapshot frame at delay 0"):
    val lines = Scenario.schedule(Vector(0 -> NodeDeclared("r", "a", None, Nil)), leadSnapshot = true)
    assertEquals(lines.head.delayMs, 0)
    WireCodec.decode(lines.head.data) match
      case Right(_: WireMessage.Snapshot) => ()
      case other                          => fail(s"expected a Snapshot first, got $other")

  test("every scheduled line of every scenario decodes back to a WireMessage"):
    Scenarios.names.foreach: name =>
      Scenario.schedule(Scenarios.byName(name), leadSnapshot = true).foreach: sl =>
        assert(WireCodec.decode(sl.data).isRight, s"$name: ${sl.data}")

  test("snapshotFrom folds events into one forest per runId"):
    val evs = Vector(0 -> NodeDeclared("r", "a", None, Nil), 0 -> NodeStarted("r", "a", "go"))
    Scenario.snapshotFrom(evs) match
      case WireMessage.Snapshot(List((rid, f))) =>
        assertEquals(rid, "r")
        assertEquals(f.nodes.head.status, NodeStatus.Running)
      case other => fail(s"expected a single-run snapshot, got $other")

  test("byName returns each named fixture and falls back to fanout for unknown names"):
    Scenarios.names.foreach(n => assert(Scenarios.byName(n).nonEmpty, n))
    assertEquals(Scenarios.byName("nonexistent"), Scenarios.byName("fanout"))

  test("the loop fixture has the writer/reviewer dependency chain"):
    val deps = Scenarios.byName("loop").map(_._2).collect { case NodeDeclared(_, id, _, d) => id -> d }.toMap
    assertEquals(deps.get("reviewer-1"), Some(List("writer-1")))
    assertEquals(deps.get("writer-2"), Some(List("reviewer-1")))
    assertEquals(deps.get("reviewer-3"), Some(List("writer-3")))

  test("the bigFanout fixture has queued and running nodes coexisting mid-run"):
    val evs = Scenarios.byName("bigFanout").map(_._2)
    val prefix = evs.takeWhile { case _: NodeFinished => false; case _ => true }
    val f = prefix.foldLeft(Forest.empty)((s, e) => s.update(e))
    assert(f.nodes.exists(_.status == NodeStatus.Running), "expected some running nodes")
    assert(f.nodes.exists(_.status == NodeStatus.Queued), "expected some queued nodes")

  test("the failures fixture produces a failed node and a log line"):
    val evs = Scenarios.byName("failures").map(_._2)
    assert(evs.exists { case NodeFinished(_, _, false, _) => true; case _ => false }, "expected a failure")
    assert(evs.exists { case _: Log => true; case _ => false }, "expected a log")

  test("every fixture ends with all nodes terminal (Done or Failed)"):
    Scenarios.names.foreach: name =>
      val f = Scenarios.byName(name).map(_._2).foldLeft(Forest.empty)((s, e) => s.update(e))
      val pending = f.nodes.filterNot(n => n.status == NodeStatus.Done || n.status == NodeStatus.Failed)
      assert(pending.isEmpty, s"$name left non-terminal nodes: ${pending.map(_.id)}")
