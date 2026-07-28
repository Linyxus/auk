package auk.webui

import auk.workflow.{Forest, ForestNode, NodeStatus, OrchestrationEvent, Transcript, TranscriptEvent, TranscriptItem, WireMessage}
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

  // -- the run the URL asked for (pendingRun) ----------------------------------

  test("a Snapshot carrying the pending run selects it and clears the intent"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    // at boot there is no selection yet: the intent beats the first-run fallback
    val booted = AppState(pendingRun = Some("r2")).reduce(snap)
    assertEquals(booted.selectedRun, Some("r2"))
    assertEquals(booted.pendingRun, None)
    // and it also beats a selection carried across a reconnect
    val reselected = AppState(selectedRun = Some("r1"), pendingRun = Some("r2")).reduce(snap)
    assertEquals(reselected.selectedRun, Some("r2"))

  test("a pending run the Snapshot doesn't have survives it; the usual fallback selects"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    val s = AppState(pendingRun = Some("later")).reduce(snap)
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.pendingRun, Some("later"))

  test("an Event for the pending run takes it over, clearing the intent and the focus"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val base = AppState(
      forests = Map("r1" -> f), order = Vector("r1"),
      selectedRun = Some("r1"), pendingRun = Some("r2"), focus = Focus.Node("a")
    )
    val s = base.reduce(ev(NodeDeclared("r2", "b", None, Nil)))
    assertEquals(s.selectedRun, Some("r2"))
    assertEquals(s.pendingRun, None)
    assertEquals(s.focus, Focus.Unfocused)
    assertEquals(s.order, Vector("r1", "r2"))

  test("an Event for some other run leaves the pending intent waiting"):
    val s = AppState(pendingRun = Some("later")).reduce(ev(NodeDeclared("r1", "a", None, Nil)))
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.pendingRun, Some("later"))

  test("desireRun selects a run that is already known, exactly like a click"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      order = Vector("r1", "r2"), selectedRun = Some("r1"), focus = Focus.Node("a")
    )
    assertEquals(s.desireRun("r2"), s.selectRun("r2"))
    assertEquals(s.desireRun("r2").selectedRun, Some("r2"))
    assertEquals(s.desireRun("r2").pendingRun, None)

  test("desireRun on an unknown run only records the intent"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r1" -> f), selectedRun = Some("r1"), focus = Focus.Node("a")).desireRun("r9")
    assertEquals(s.pendingRun, Some("r9"))
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.focus, Focus.Node("a"))

  test("urlRun names the pending run first, so the fragment keeps reproducing the intent"):
    assertEquals(AppState(pendingRun = Some("later")).urlRun, Some("later"))
    // the fallback run is on screen, but the URL still asks for the one we're waiting on
    assertEquals(AppState(selectedRun = Some("r1"), pendingRun = Some("later")).urlRun, Some("later"))

  test("urlRun names the selected run once no intent is outstanding"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    // fulfilled by the snapshot: the intent is gone and the URL follows the selection
    assertEquals(AppState(pendingRun = Some("r2")).reduce(snap).urlRun, Some("r2"))
    assertEquals(AppState(selectedRun = Some("r1")).urlRun, Some("r1"))
    assertEquals(AppState().urlRun, None)

  test("selectRun drops a stale pending run (a click outranks the URL)"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      selectedRun = Some("r1"), pendingRun = Some("later")
    )
    assertEquals(s.selectRun("r2").pendingRun, None)

  // -- reconnect must not duplicate transcripts (the lag bug) ------------------

  /** The exact frame sequence the host sends on every (re)connect: a forest
    * Snapshot, then every stored transcript replayed as Activity (see
    * `WorkflowWebServer.onClient` / `Transcript.toEvents`). */
  private def connectFrames(rid: String, f: Forest, transcripts: List[(String, Transcript)]): List[WireMessage] =
    WireMessage.Snapshot(List(rid -> f)) ::
      transcripts.flatMap((nid, t) => t.toEvents(rid, nid).map(WireMessage.Activity(_)).toList)

  /** A representative sub-agent transcript: prose, a completed tool call, more prose. */
  private val sampleTranscript: Transcript =
    List(
      TranscriptEvent.Said("r", "a", "looking… "),
      TranscriptEvent.Said("r", "a", "done."),
      TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "pat"),
      TranscriptEvent.ToolReturned("r", "a", "c1", "3 hits", isError = false)
    ).foldLeft(Transcript.empty)((t, e) => t.update(e))

  test("a Snapshot clears accumulated transcripts (the replay that follows rebuilds them)"):
    val before = AppState().reduce(act(TranscriptEvent.Said("r", "a", "old text")))
    assert(before.transcripts.nonEmpty)
    val after = before.reduce(WireMessage.Snapshot(List("r" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running))))))
    assertEquals(after.transcripts, Map.empty[String, Map[String, Transcript]])

  test("a Snapshot drops transcripts for runs that vanished"):
    val before = AppState().reduce(act(TranscriptEvent.Said("old", "x", "hi")))
    val after = before.reduce(WireMessage.Snapshot(List("r1" -> Forest.empty)))
    assert(!after.transcripts.contains("old"))

  test("a reconnect (Snapshot + replay) rebuilds the transcript without duplicating it"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val connect = connectFrames("r", f, List("a" -> sampleTranscript))
    val once = connect.foldLeft(AppState())((s, m) => s.reduce(m))
    // the first connect reproduces the server's transcript exactly
    assertEquals(once.transcripts("r")("a"), sampleTranscript)
    // a SECOND connect folds the same replay into the already-populated state…
    val twice = connect.foldLeft(once)((s, m) => s.reduce(m))
    // …and the transcript is still the single, un-duplicated copy
    assertEquals(twice.transcripts, once.transcripts)
    assertEquals(twice.transcripts("r")("a"), sampleTranscript)

  test("many reconnects keep the transcript bounded (no unbounded growth)"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val connect = connectFrames("r", f, List("a" -> sampleTranscript))
    val after10 = (1 to 10).foldLeft(AppState())((s, _) => connect.foldLeft(s)((st, m) => st.reduce(m)))
    assertEquals(after10.transcripts("r")("a"), sampleTranscript)
    assertEquals(after10.transcripts("r")("a").items.size, sampleTranscript.items.size)

  test("a re-running interrupted node drops its stale transcript; other nodes survive"):
    val base = AppState()
      .reduce(ev(NodeDeclared("r", "a", None, Nil)))
      .reduce(ev(NodeStarted("r", "a", "go")))
      .reduce(act(TranscriptEvent.Said("r", "a", "partial work")))
      .reduce(ev(NodeDeclared("r", "b", None, Nil)))
      .reduce(ev(NodeFinished("r", "b", true, "done")))
      .reduce(act(TranscriptEvent.Said("r", "b", "b's full output")))
      .reduce(ev(NodeInterrupted("r", "a")))
    // While paused, the interrupted node keeps its partial transcript (inspectable).
    assertEquals(base.transcripts("r").get("a").map(_.items.size), Some(1))
    // Resume re-admits 'a' (its first queued event) → drop the discarded attempt…
    val resumed = base.reduce(ev(NodeQueued("r", "a")))
    assertEquals(resumed.transcripts("r").get("a"), None)
    // …but a different node's transcript is left untouched.
    assertEquals(resumed.transcripts("r").get("b").map(_.items.size), Some(1))
    // The fresh run then builds a clean transcript with none of the old text.
    val fresh = resumed
      .reduce(ev(NodeStarted("r", "a", "go")))
      .reduce(act(TranscriptEvent.Said("r", "a", "fresh start")))
    assertEquals(fresh.transcripts("r")("a").items.collect { case s: TranscriptItem.Said => s.text }, Vector("fresh start"))

  test("live Activity after a reconnect still appends (no over-eager clearing)"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val connect = connectFrames("r", f, List("a" -> sampleTranscript))
    val reconnected = connect.foldLeft(AppState())((s, m) => s.reduce(m))
    // a genuinely new delta after the connect appends as usual (the transcript
    // ends with the replayed tool call, so a fresh Said item is added)
    val s = reconnected.reduce(act(TranscriptEvent.Said("r", "a", " more")))
    assertEquals(s.transcripts("r")("a").items.last, TranscriptItem.Said(" more"))
    assertEquals(s.transcripts("r")("a").items.size, sampleTranscript.items.size + 1)

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

  test("clearFocus drops the focus without touching the run"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selectedRun = Some("r")).selectNode("a").clearFocus
    assertEquals(s.focus, Focus.Unfocused)
    assertEquals(s.selectedRun, Some("r"))

  test("selectCode focuses the code only when the run has code"):
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    assertEquals(AppState(forests = Map("r" -> withCode), selectedRun = Some("r")).selectCode.focus, Focus.Code)
    val noCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(AppState(forests = Map("r" -> noCode), selectedRun = Some("r")).selectCode.focus, Focus.Unfocused)
