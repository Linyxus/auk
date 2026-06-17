package auk.webui

import auk.workflow.{Forest, ForestNode, NodeStatus, OrchestrationEvent, WireMessage}
import OrchestrationEvent.*

class AppStateSuite extends munit.FunSuite:

  private def ev(e: OrchestrationEvent): WireMessage = WireMessage.Event(e)

  test("an Event for a new runId creates a fresh forest and selects it"):
    val s = AppState().reduce(ev(NodeDeclared("r1", "a", None, Nil)))
    assertEquals(s.order, Vector("r1"))
    assertEquals(s.selected, Some("r1"))
    assertEquals(s.forests("r1").nodes.map(_.id), Vector("a"))

  test("an Event for an existing runId folds into that forest"):
    val s = AppState()
      .reduce(ev(NodeDeclared("r1", "a", None, Nil)))
      .reduce(ev(NodeStarted("r1", "a", "go")))
    assertEquals(s.forests("r1").nodes.head.status, NodeStatus.Running)

  test("events for two runIds keep both in insertion order; first stays selected"):
    val s = AppState()
      .reduce(ev(NodeDeclared("r1", "a", None, Nil)))
      .reduce(ev(NodeDeclared("r2", "b", None, Nil)))
    assertEquals(s.order, Vector("r1", "r2"))
    assertEquals(s.selected, Some("r1"))

  test("order appends a runId exactly once across many events"):
    val s = List(NodeDeclared("r1", "a", None, Nil), NodeQueued("r1", "a"), NodeStarted("r1", "a", "go"))
      .foldLeft(AppState())((st, e) => st.reduce(ev(e)))
    assertEquals(s.order, Vector("r1"))

  test("a Snapshot replaces the entire forest set"):
    val before = AppState().reduce(ev(NodeDeclared("old", "x", None, Nil)))
    val snap = WireMessage.Snapshot(List("r1" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))))
    val s = before.reduce(snap)
    assertEquals(s.order, Vector("r1"))
    assert(!s.forests.contains("old"))
    assertEquals(s.forests("r1").nodes.head.status, NodeStatus.Done)

  test("a Snapshot preserves the current selection when that run still exists"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    assertEquals(AppState(selected = Some("r2")).reduce(snap).selected, Some("r2"))

  test("a Snapshot falls back to the first run when the selected run vanished"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    assertEquals(AppState(selected = Some("gone")).reduce(snap).selected, Some("r1"))

  test("a Snapshot with no runs clears the selection"):
    assertEquals(AppState(selected = Some("r1")).reduce(WireMessage.Snapshot(Nil)).selected, None)

  test("withConn updates only the connection status"):
    assertEquals(AppState().withConn(ConnStatus.Open).conn, ConnStatus.Open)

  test("select switches to a known run"):
    val s = AppState(forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty), order = Vector("r1", "r2"), selected = Some("r1"))
    assertEquals(s.select("r2").selected, Some("r2"))

  test("select ignores an unknown run id"):
    val s = AppState(forests = Map("r1" -> Forest.empty), selected = Some("r1"))
    assertEquals(s.select("nope").selected, Some("r1"))
