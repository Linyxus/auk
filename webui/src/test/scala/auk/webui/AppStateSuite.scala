package auk.webui

import auk.workflow.*
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
    assertEquals(AppState(selected = Some(Target.Run("r2"))).reduce(snap).selectedRun, Some("r2"))

  test("a Snapshot falls back to the first run when the selected run vanished"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    assertEquals(AppState(selected = Some(Target.Run("gone"))).reduce(snap).selectedRun, Some("r1"))

  test("a Snapshot with no runs clears the selection and focus"):
    val s = AppState(selected = Some(Target.Run("r1")), focus = Focus.Node("a")).reduce(WireMessage.Snapshot(Nil))
    assertEquals(s.selectedRun, None)
    assertEquals(s.focus, Focus.Unfocused)

  test("a Snapshot keeps a still-present focused node and clears a vanished one"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    val kept = AppState(selected = Some(Target.Run("r")), focus = Focus.Node("a")).reduce(WireMessage.Snapshot(List("r" -> f)))
    assertEquals(kept.focus, Focus.Node("a"))
    val cleared = AppState(selected = Some(Target.Run("r")), focus = Focus.Node("gone")).reduce(WireMessage.Snapshot(List("r" -> f)))
    assertEquals(cleared.focus, Focus.Unfocused)

  test("a Snapshot keeps a Code focus only while the run still has code"):
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    val noCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(AppState(selected = Some(Target.Run("r")), focus = Focus.Code).reduce(WireMessage.Snapshot(List("r" -> withCode))).focus, Focus.Code)
    assertEquals(AppState(selected = Some(Target.Run("r")), focus = Focus.Code).reduce(WireMessage.Snapshot(List("r" -> noCode))).focus, Focus.Unfocused)

  // -- the target the URL asked for (pending) ----------------------------------

  test("a Snapshot carrying the pending run selects it and clears the intent"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    // at boot there is no selection yet: the intent beats the first-run fallback
    val booted = AppState(pending = Some(Target.Run("r2"))).reduce(snap)
    assertEquals(booted.selectedRun, Some("r2"))
    assertEquals(booted.pending, None)
    // and it also beats a selection carried across a reconnect
    val reselected = AppState(selected = Some(Target.Run("r1")), pending = Some(Target.Run("r2"))).reduce(snap)
    assertEquals(reselected.selectedRun, Some("r2"))

  test("a pending run the Snapshot doesn't have survives it; the usual fallback selects"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    val s = AppState(pending = Some(Target.Run("later"))).reduce(snap)
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.pending, Some(Target.Run("later")))

  test("an Event for the pending run takes it over, clearing the intent and the focus"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val base = AppState(
      forests = Map("r1" -> f), order = Vector("r1"),
      selected = Some(Target.Run("r1")), pending = Some(Target.Run("r2")), focus = Focus.Node("a")
    )
    val s = base.reduce(ev(NodeDeclared("r2", "b", None, Nil)))
    assertEquals(s.selectedRun, Some("r2"))
    assertEquals(s.pending, None)
    assertEquals(s.focus, Focus.Unfocused)
    assertEquals(s.order, Vector("r1", "r2"))

  test("an Event for some other run leaves the pending intent waiting"):
    val s = AppState(pending = Some(Target.Run("later"))).reduce(ev(NodeDeclared("r1", "a", None, Nil)))
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.pending, Some(Target.Run("later")))

  test("desire selects a run that is already known, exactly like a click"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      order = Vector("r1", "r2"), selected = Some(Target.Run("r1")), focus = Focus.Node("a")
    )
    assertEquals(s.desire(Target.Run("r2")), s.select(Target.Run("r2")))
    assertEquals(s.desire(Target.Run("r2")).selectedRun, Some("r2"))
    assertEquals(s.desire(Target.Run("r2")).pending, None)

  test("desire on an unknown run only records the intent"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r1" -> f), selected = Some(Target.Run("r1")), focus = Focus.Node("a")).desire(Target.Run("r9"))
    assertEquals(s.pending, Some(Target.Run("r9")))
    assertEquals(s.selectedRun, Some("r1"))
    assertEquals(s.focus, Focus.Node("a"))

  test("urlTarget names the pending target first, so the fragment keeps reproducing the intent"):
    assertEquals(AppState(pending = Some(Target.Run("later"))).urlTarget, Some(Target.Run("later")))
    // the fallback run is on screen, but the URL still asks for the one we're waiting on
    assertEquals(AppState(selected = Some(Target.Run("r1")), pending = Some(Target.Run("later"))).urlTarget, Some(Target.Run("later")))

  test("urlTarget names the selection once no intent is outstanding"):
    val snap = WireMessage.Snapshot(List("r1" -> Forest.empty, "r2" -> Forest.empty))
    // fulfilled by the snapshot: the intent is gone and the URL follows the selection
    assertEquals(AppState(pending = Some(Target.Run("r2"))).reduce(snap).urlTarget, Some(Target.Run("r2")))
    assertEquals(AppState(selected = Some(Target.Run("r1"))).urlTarget, Some(Target.Run("r1")))
    assertEquals(AppState().urlTarget, None)

  test("select drops a stale pending target (a click outranks the URL)"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      selected = Some(Target.Run("r1")), pending = Some(Target.Run("later"))
    )
    assertEquals(s.select(Target.Run("r2")).pending, None)

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

  test("select switches to a known run and clears the focus"):
    val s = AppState(
      forests = Map("r1" -> Forest.empty, "r2" -> Forest.empty),
      order = Vector("r1", "r2"), selected = Some(Target.Run("r1")), focus = Focus.Node("a")
    )
    val sw = s.select(Target.Run("r2"))
    assertEquals(sw.selectedRun, Some("r2"))
    assertEquals(sw.focus, Focus.Unfocused)

  test("select ignores an unknown run id"):
    val s = AppState(forests = Map("r1" -> Forest.empty), selected = Some(Target.Run("r1")))
    assertEquals(s.select(Target.Run("nope")).selectedRun, Some("r1"))

  test("selectNode focuses a node within the current run"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selected = Some(Target.Run("r"))).selectNode("a")
    assertEquals(s.focus, Focus.Node("a"))

  test("selectNode ignores an unknown node id"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selected = Some(Target.Run("r"))).selectNode("zzz")
    assertEquals(s.focus, Focus.Unfocused)

  test("clearFocus drops the focus without touching the run"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    val s = AppState(forests = Map("r" -> f), selected = Some(Target.Run("r"))).selectNode("a").clearFocus
    assertEquals(s.focus, Focus.Unfocused)
    assertEquals(s.selectedRun, Some("r"))

  test("selectCode focuses the code only when the run has code"):
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    assertEquals(AppState(forests = Map("r" -> withCode), selected = Some(Target.Run("r"))).selectCode.focus, Focus.Code)
    val noCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(AppState(forests = Map("r" -> noCode), selected = Some(Target.Run("r"))).selectCode.focus, Focus.Unfocused)

  // -- loops -------------------------------------------------------------------

  private def loop(
      id: String,
      generations: List[LoopGenerationWire] = Nil,
      defSource: String = "lib.loop.start(...)",
      liveLabel: Option[String] = None
  ): LoopWire =
    LoopWire(id, "running (gen 1)", "a goal", "a rubric", LoopBudgetsWire(20, 2, 3), defSource, 1,
      held = true, parked = None, orphaned = false, activity = None, liveLabel = liveLabel,
      generations = generations, createdAt = "t")

  private def genw(n: Int, state: String = "accepted"): LoopGenerationWire =
    LoopGenerationWire(n, None, state, "", Nil, None, Nil, "t", None)

  test("a LoopSnapshot replaces the loop set and selects the first when nothing is"):
    val s = AppState().reduce(WireMessage.LoopSnapshot(List(loop("l1"), loop("l2"))))
    assertEquals(s.loopOrder, Vector("l1", "l2"))
    assertEquals(s.selected, Some(Target.Loop("l1")))

  test("a LoopSnapshot leaves a selected run alone — it says nothing about workflows"):
    val base = AppState().reduce(ev(NodeDeclared("r1", "a", None, Nil)))
    val s = base.reduce(WireMessage.LoopSnapshot(List(loop("l1"))))
    assertEquals(s.selected, Some(Target.Run("r1")))
    assertEquals(s.loops.keySet, Set("l1"))

  test("a LoopSnapshot keeps a selected loop that is still there, and re-selects when it went"):
    val two = WireMessage.LoopSnapshot(List(loop("l1"), loop("l2")))
    assertEquals(AppState(selected = Some(Target.Loop("l2"))).reduce(two).selected, Some(Target.Loop("l2")))
    assertEquals(AppState(selected = Some(Target.Loop("gone"))).reduce(two).selected, Some(Target.Loop("l1")))

  test("a LoopSnapshot must not clear the transcripts the workflow snapshot already answered for"):
    // the host's connect order is Snapshot, LoopSnapshot, then the replay of both
    val s = AppState()
      .reduce(act(TranscriptEvent.Said("r", "a", "replayed")))
      .reduce(WireMessage.LoopSnapshot(List(loop("l1"))))
    assertEquals(s.transcripts("r")("a").items.size, 1)

  test("a workflow Snapshot leaves the loops and a generation focus alone"):
    val l = loop("l1", List(genw(1)))
    val base = AppState(loops = Map("l1" -> l), loopOrder = Vector("l1"),
      selected = Some(Target.Loop("l1")), focus = Focus.Generation(1, GenPane.Overview, None))
    val s = base.reduce(WireMessage.Snapshot(List("r1" -> Forest.empty)))
    assertEquals(s.loops.keySet, Set("l1"))
    assertEquals(s.selected, Some(Target.Loop("l1")))
    assertEquals(s.focus, Focus.Generation(1, GenPane.Overview, None))

  test("a LoopSnapshot drops a focus on a generation that is no longer there"):
    val base = AppState(loops = Map("l1" -> loop("l1", List(genw(1)))), loopOrder = Vector("l1"),
      selected = Some(Target.Loop("l1")), focus = Focus.Generation(1, GenPane.Overview, None))
    val s = base.reduce(WireMessage.LoopSnapshot(List(loop("l1"))))
    assertEquals(s.focus, Focus.Unfocused)

  test("a Loop upserts, keeping insertion order and selecting only when nothing is"):
    val s = AppState().reduce(WireMessage.Loop(loop("l1")))
    assertEquals(s.selected, Some(Target.Loop("l1")))
    val next = s.reduce(WireMessage.Loop(loop("l2"))).reduce(WireMessage.Loop(loop("l1", List(genw(1)))))
    assertEquals(next.loopOrder, Vector("l1", "l2"))
    assertEquals(next.selected, Some(Target.Loop("l1")))
    assertEquals(next.loops("l1").generations.size, 1)

  test("a pending loop is taken over the moment it arrives, in either message"):
    val bySnapshot = AppState(pending = Some(Target.Loop("l2")))
      .reduce(WireMessage.LoopSnapshot(List(loop("l1"), loop("l2"))))
    assertEquals(bySnapshot.selected, Some(Target.Loop("l2")))
    assertEquals(bySnapshot.pending, None)
    val byUpsert = AppState().reduce(ev(NodeDeclared("r1", "a", None, Nil)))
      .copy(pending = Some(Target.Loop("l9")))
      .reduce(WireMessage.Loop(loop("l9")))
    assertEquals(byUpsert.selected, Some(Target.Loop("l9")))
    assertEquals(byUpsert.pending, None)

  test("a pending loop a workflow Snapshot cannot fulfil survives it"):
    val s = AppState(pending = Some(Target.Loop("later"))).reduce(WireMessage.Snapshot(List("r1" -> Forest.empty)))
    assertEquals(s.selected, Some(Target.Run("r1")))
    assertEquals(s.pending, Some(Target.Loop("later")))

  test("a pending loop switching the selection closes the drawer the old one owned"):
    val base = AppState().reduce(ev(NodeDeclared("r1", "a", None, Nil))).selectNode("a")
    assertEquals(base.focus, Focus.Node("a"))
    val s = base.copy(pending = Some(Target.Loop("l1"))).reduce(WireMessage.Loop(loop("l1")))
    assertEquals(s.focus, Focus.Unfocused)

  test("urlTarget and UrlHash agree about a loop"):
    val s = AppState().reduce(WireMessage.Loop(loop("l1")))
    assertEquals(s.urlTarget, Some(Target.Loop("l1")))
    assertEquals(UrlHash.parse(UrlHash.format(s.urlTarget)), Some(Target.Loop("l1")))

  // -- the generation window's own selections ----------------------------------

  private def withLoop(gens: List[LoopGenerationWire]): AppState =
    AppState(loops = Map("l" -> loop("l", gens)), loopOrder = Vector("l"), selected = Some(Target.Loop("l")))

  test("selecting a generation opens it on the overview, at its latest attempt"):
    assertEquals(withLoop(List(genw(1))).selectGeneration(1).focus, Focus.Generation(1, GenPane.Overview, None))

  test("selecting a generation the loop does not have is ignored"):
    assertEquals(withLoop(List(genw(1))).selectGeneration(9).focus, Focus.Unfocused)

  test("re-selecting the open generation keeps the pane and attempt the reader chose"):
    val s = withLoop(List(genw(1))).selectGeneration(1).selectPane(GenPane.Worker).selectAttempt(2)
    assertEquals(s.selectGeneration(1).focus, Focus.Generation(1, GenPane.Worker, Some(2)))

  test("switching to a different generation starts over on the overview"):
    val s = withLoop(List(genw(1), genw(2))).selectGeneration(1).selectPane(GenPane.Worker).selectGeneration(2)
    assertEquals(s.focus, Focus.Generation(2, GenPane.Overview, None))

  test("pane and attempt only mean anything with a generation open"):
    val closed = withLoop(List(genw(1)))
    assertEquals(closed.selectPane(GenPane.Worker).focus, Focus.Unfocused)
    assertEquals(closed.selectAttempt(2).focus, Focus.Unfocused)

  test("focusedGeneration names the open generation and nothing else"):
    assertEquals(withLoop(List(genw(1))).selectGeneration(1).focusedGeneration, Some(1))
    assertEquals(AppState().selectNode("a").focusedGeneration, None)

  test("selecting a loop clears a focus the previous selection owned"):
    val s = AppState(
      forests = Map("r" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))),
      loops = Map("l" -> loop("l")), loopOrder = Vector("l"),
      selected = Some(Target.Run("r")), focus = Focus.Node("a")
    ).select(Target.Loop("l"))
    assertEquals(s.selected, Some(Target.Loop("l")))
    assertEquals(s.focus, Focus.Unfocused)

  test("selectCode works for a loop's definition and refuses one that has none"):
    val s = AppState(loops = Map("l" -> loop("l")), loopOrder = Vector("l"), selected = Some(Target.Loop("l")))
    assertEquals(s.selectCode.focus, Focus.Code)
    assertEquals(s.copy(loops = Map("l" -> loop("l", defSource = ""))).selectCode.focus, Focus.Unfocused)

  // -- the on-demand payloads --------------------------------------------------

  test("a payload is worth asking for exactly once, whatever the answer was"):
    val s = AppState()
    assert(s.needsTranscript("l", "gen-1-worker"))
    assert(!s.transcriptRequested("l", "gen-1-worker").needsTranscript("l", "gen-1-worker"))
    assert(!s.transcriptFailed("l", "gen-1-worker", "gone").needsTranscript("l", "gen-1-worker"))
    assert(s.needsDiff("l", 1, 1))
    assert(!s.diffRequested("l", 1, 1).needsDiff("l", 1, 1))
    assert(!s.diffFailed("l", 1, 1, "gone").needsDiff("l", 1, 1))

  /** The tee file's own format: one `Activity` frame per line, which is exactly what
    * the host replays live — so the fetched copy folds through the very same path. */
  private def teeFile(loopId: String, label: String, texts: String*): String =
    texts.map(t => WireCodec.encode(WireMessage.Activity(TranscriptEvent.Said(loopId, label, t)))).mkString("\n")

  test("a fetched transcript folds into the same map a live stream folds into"):
    val s = AppState().transcriptLoaded("l", "gen-1-worker", teeFile("l", "gen-1-worker", "one ", "two"))
    assertEquals(s.transcripts("l")("gen-1-worker").items, Vector(TranscriptItem.Said("one two")))
    assertEquals(s.fetches(("l", "gen-1-worker")), FetchState.Ready)

  test("a fetched transcript is indistinguishable from having watched it stream"):
    val streamed = List("one ", "two").foldLeft(AppState())((st, t) =>
      st.reduce(act(TranscriptEvent.Said("l", "gen-1-worker", t))))
    val fetched = AppState().transcriptLoaded("l", "gen-1-worker", teeFile("l", "gen-1-worker", "one ", "two"))
    assertEquals(fetched.transcripts, streamed.transcripts)

  test("a fetched file's frames for anything else are dropped"):
    val mixed = List(
      teeFile("l", "gen-1-worker", "mine"),
      teeFile("l", "gen-2-worker", "not mine"),
      teeFile("other", "gen-1-worker", "not mine either")
    ).mkString("\n")
    val s = AppState().transcriptLoaded("l", "gen-1-worker", mixed)
    assertEquals(s.transcripts("l")("gen-1-worker").items, Vector(TranscriptItem.Said("mine")))
    assertEquals(s.transcripts("l").keySet, Set("gen-1-worker"))

  test("an unreadable line is skipped rather than failing the file"):
    val s = AppState().transcriptLoaded("l", "gen-1-worker", teeFile("l", "gen-1-worker", "kept") + "\n{ half a lin")
    assertEquals(s.transcripts("l")("gen-1-worker").items, Vector(TranscriptItem.Said("kept")))

  test("a fetch that lands after the label went live loses to the stream"):
    val live = AppState(loops = Map("l" -> loop("l", liveLabel = Some("gen-1-worker"))), loopOrder = Vector("l"))
      .reduce(act(TranscriptEvent.Said("l", "gen-1-worker", "the live one")))
    val s = live.transcriptLoaded("l", "gen-1-worker", teeFile("l", "gen-1-worker", "the stale one"))
    assertEquals(s.transcripts("l")("gen-1-worker").items, Vector(TranscriptItem.Said("the live one")))
    assertEquals(s.fetches(("l", "gen-1-worker")), FetchState.Ready)

  test("a reconnect forgets what was fetched, since the Snapshot dropped it"):
    val s = AppState()
      .transcriptLoaded("l", "gen-1-worker", teeFile("l", "gen-1-worker", "text"))
      .reduce(WireMessage.Snapshot(Nil))
    assert(s.needsTranscript("l", "gen-1-worker"))
    assertEquals(s.transcripts, Map.empty[String, Map[String, Transcript]])

  test("a reconnect keeps the diffs: a settled patch cannot go stale"):
    val s = AppState().diffLoaded("l", 1, 1, "@@ patch").reduce(WireMessage.Snapshot(Nil))
    assertEquals(s.diffs(("l", 1, 1)), DiffState.Ready("@@ patch"))

  test("a loop transcript is reachable by its label"):
    val s = AppState().reduce(act(TranscriptEvent.Said("l", "gen-1-eval", "hi")))
    assertEquals(s.loopTranscript("l", "gen-1-eval").map(_.items.size), Some(1))
    assertEquals(s.loopTranscript("l", "gen-2-eval"), None)
