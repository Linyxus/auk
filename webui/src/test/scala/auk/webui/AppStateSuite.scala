package auk.webui

import auk.workflow.{Forest, ForestNode, NodeStatus, OrchestrationEvent, TranscriptEvent, TranscriptItem, WireMessage}
import OrchestrationEvent.*

class AppStateSuite extends munit.FunSuite:

  private def ev(e: OrchestrationEvent): WireMessage = WireMessage.Event(e)
  private def act(e: TranscriptEvent): WireMessage = WireMessage.Activity(e)

  // -- run selection & forest folding -----------------------------------------

  test("an Event for a new runId creates a fresh forest and selects the run"):
    val s = AppState().reduce(ev(NodeDeclared("r1", "a", None, Nil)))
    assertEquals(s.order, Vector("r1"))
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.focus, Focus.Unfocused)
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
    assertEquals(s.selectedRun, Some("r1"))

  test("order appends a runId exactly once across many events"):
    val s = List(NodeDeclared("r1", "a", None, Nil), NodeQueued("r1", "a"), NodeStarted("r1", "a", "go"))
      .foldLeft(AppState())((st, e) => st.reduce(ev(e)))
    assertEquals(s.order, Vector("r1"))

  // -- snapshots ---------------------------------------------------------------

  test("a Snapshot replaces the entire forest set"):
    val before = AppState().reduce(ev(NodeDeclared("old", "x", None, Nil)))
    val snap = WireMessage.Snapshot(List("r1" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))))
    val s = before.reduce(snap)
    assertEquals(s.order, Vector("r1"))
    assert(!s.forests.contains("old"))
    assertEquals(s.forests("r1").nodes.head.status, NodeStatus.Done)

  test("a Snapshot preserves the selected run when it still exists"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    assertEquals(AppState(selectedRun = Some("r2")).reduce(snap).selectedRun, Some("r2"))

  test("a Snapshot falls back to the first run when the selected run vanished"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    assertEquals(AppState(selectedRun = Some("gone")).reduce(snap).selectedRun, Some("r1"))

  test("a Snapshot with no runs clears the selection and focus"):
    val s = AppState(selectedRun = Some("r1"), focus = Focus.Node("a")).reduce(WireMessage.Snapshot(Nil))
    assertEquals(s.selectedRun, None)
    assertEquals(s.focus, Focus.Unfocused)

  test("a Snapshot keeps a still-present focused node and clears a vanished one"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    val kept = AppState(selectedRun = Some("r"), focus = Focus.Node("a")).reduce(WireMessage.Snapshot(List("r" -> f)))
    assertEquals(kept.focus, Focus.Node("a"))
    val cleared = AppState(selectedRun = Some("r"), focus = Focus.Node("gone")).reduce(WireMessage.Snapshot(List("r" -> f)))
    assertEquals(cleared.focus, Focus.Unfocused)

  test("a Snapshot keeps a Code focus only while the run still has code"):
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    val noCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(AppState(selectedRun = Some("r"), focus = Focus.Code).reduce(WireMessage.Snapshot(List("r" -> withCode))).focus, Focus.Code)
    assertEquals(AppState(selectedRun = Some("r"), focus = Focus.Code).reduce(WireMessage.Snapshot(List("r" -> noCode))).focus, Focus.Unfocused)

  // -- transcript (activity) ---------------------------------------------------

  test("an Activity event folds into the addressed (run, node) transcript"):
    val s = AppState()
      .reduce(act(TranscriptEvent.Said("r", "a", "hello ")))
      .reduce(act(TranscriptEvent.Said("r", "a", "world")))
    assertEquals(s.transcripts("r")("a").items, Vector(TranscriptItem.Said("hello world")))

  test("Activity for different nodes keeps separate transcripts"):
    val s = AppState()
      .reduce(act(TranscriptEvent.Said("r", "a", "A")))
      .reduce(act(TranscriptEvent.Said("r", "b", "B")))
    assertEquals(s.transcripts("r")("a").items, Vector(TranscriptItem.Said("A")))
    assertEquals(s.transcripts("r")("b").items, Vector(TranscriptItem.Said("B")))

  test("Activity never changes the run/focus"):
    val base = AppState().reduce(ev(NodeDeclared("r", "a", None, Nil)))
    val s = base.reduce(act(TranscriptEvent.Said("r", "a", "hi")))
    assertEquals(s.selectedRun, base.selectedRun)
    assertEquals(s.focus, Focus.Unfocused)

  test("selectedTranscript returns the focused node's transcript, else empty"):
    val s = AppState()
      .reduce(ev(NodeStarted("r", "a", "go")))
      .reduce(act(TranscriptEvent.Said("r", "a", "hi")))
      .selectNode("a")
    assertEquals(s.selectedTranscript.items, Vector(TranscriptItem.Said("hi")))
    assertEquals(AppState().selectedTranscript.items, Vector.empty)

  // -- mutators ----------------------------------------------------------------

  test("withConn updates only the connection status"):
    assertEquals(AppState().withConn(ConnStatus.Open).conn, ConnStatus.Open)

  test("selectRun switches to a known run and clears the focus"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      order = Vector("r1", "r2"), selectedRun = Some("r1"), focus = Focus.Node("a")
    )
    val sw = s.selectRun("r2")
    assertEquals(sw.selectedRun, Some("r2"))
    assertEquals(sw.focus, Focus.Unfocused)

  test("selectRun ignores an unknown run id"):
    val s = AppState(forests = Map("r1" -> Forest.empty), selectedRun = Some("r1"))
    assertEquals(s.selectRun("nope").selectedRun, Some("r1"))

  test("selectNode focuses a node within the current run"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selectedRun = Some("r")).selectNode("a")
    assertEquals(s.focus, Focus.Node("a"))

  test("selectNode ignores an unknown node id"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selectedRun = Some("r")).selectNode("zzz")
    assertEquals(s.focus, Focus.Unfocused)

  test("selectCode focuses the code only when the run has code"):
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    assertEquals(AppState(forests = Map("r" -> withCode), selectedRun = Some("r")).selectCode.focus, Focus.Code)
    val noCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(AppState(forests = Map("r" -> noCode), selectedRun = Some("r")).selectCode.focus, Focus.Unfocused)
