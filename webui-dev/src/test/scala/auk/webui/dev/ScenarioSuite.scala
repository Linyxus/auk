package auk.webui.dev

import auk.workflow.*
import OrchestrationEvent.*

class ScenarioSuite extends munit.FunSuite:

  private def orch(s: Scenarios.Script): Vector[OrchestrationEvent] =
    s.collect { case (_, WireMessage.Event(e)) => e }
  private def activity(s: Scenarios.Script): Vector[TranscriptEvent] =
    s.collect { case (_, WireMessage.Activity(e)) => e }
  /** The fixtures that script a workflow. Chosen by what they contain rather than
    * by name, so a fixture for something else (the `loops` one is the first) is not
    * held to a workflow's contract and a new one needs no list updated. */
  private def workflowFixtures: List[String] =
    Scenarios.names.filter(n => orch(Scenarios.byName(n)).exists { case _: NodeDeclared => true; case _ => false })

  private def loops(s: Scenarios.Script): Vector[LoopWire] =
    s.flatMap {
      case (_, WireMessage.LoopSnapshot(ls)) => ls
      case (_, WireMessage.Loop(l))          => Vector(l)
      case _                                 => Vector.empty
    }

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

  test("a scenario with no loops leads with the workflow snapshot alone"):
    val lines = Scenario.schedule(Vector(0 -> WireMessage.Event(NodeDeclared("r", "a", None, Nil))), leadSnapshot = true)
    assert(!lines.exists(l => WireCodec.decode(l.data).exists(_.isInstanceOf[WireMessage.LoopSnapshot])))

  /** The host's connect order, which the browser relies on: Snapshot, then
    * LoopSnapshot, then the replays that address what both just introduced. */
  test("leadSnapshot follows the workflow snapshot with a loop snapshot"):
    val lines = Scenario.schedule(Scenarios.byName("loops"), leadSnapshot = true)
    assertEquals(lines.take(2).map(l => WireCodec.decode(l.data)).map {
      case Right(_: WireMessage.Snapshot)     => "snapshot"
      case Right(_: WireMessage.LoopSnapshot) => "loopSnapshot"
      case other                              => other.toString
    }, Vector("snapshot", "loopSnapshot"))

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

  test("loopSnapshotFrom keeps the last state of each loop, in first-seen order"):
    val script = Scenarios.byName("loops")
    Scenario.loopSnapshotFrom(script) match
      case Some(WireMessage.LoopSnapshot(ls)) =>
        assertEquals(ls.map(_.id).distinct.size, ls.size, "one entry per loop")
        val live = ls.find(_.id == LoopScenario.LiveId).getOrElse(fail("the live loop is missing"))
        // the scenario ends with generation 6 accepted and 7 under way
        assertEquals(live.generations.size, 7)
        assertEquals(live.generations.find(_.gen == 6).map(_.state), Some("accepted"))
      case other => fail(s"expected a loop snapshot, got $other")

  test("a workflow-only scenario folds to no loop snapshot"):
    assertEquals(Scenario.loopSnapshotFrom(Scenarios.byName("fanout")), None)

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

  test("every workflow fixture ends with all nodes settled (Done, Failed, or Interrupted)"):
    workflowFixtures.foreach: name =>
      val f = orch(Scenarios.byName(name)).foldLeft(Forest.empty)((s, e) => s.update(e))
      // Interrupted is a legitimate end-state for a paused fixture (a sub-agent
      // killed mid-flight); only Pending/Queued/Running would be "stuck active".
      val stuck = f.nodes.filterNot(n =>
        n.status == NodeStatus.Done || n.status == NodeStatus.Failed || n.status == NodeStatus.Interrupted)
      assert(stuck.isEmpty, s"$name left non-terminal nodes: ${stuck.map(_.id)}")

  // -- the loops fixture -------------------------------------------------------

  test("the loops fixture opens with a snapshot holding a parked, an orphaned and a finished loop"):
    val first = Scenarios.byName("loops").head._2
    val ls = first match
      case WireMessage.LoopSnapshot(l) => l
      case other                       => fail(s"expected a LoopSnapshot first, got $other")
    assert(ls.exists(l => l.parked.contains("patience exhausted")), "expected a parked loop")
    assert(ls.exists(_.orphaned), "expected an orphaned loop")
    assert(ls.exists(l => l.parked.contains("goal reached")), "expected a loop that reached its goal")
    assert(ls.exists(l => l.held && l.parked.isEmpty && !l.orphaned), "expected a live held loop")

  test("the live loop's lineage has accepted generations, a branch stack, and one in flight"):
    val live = loops(Scenarios.byName("loops")).filter(_.id == LoopScenario.LiveId).head
    val gens = live.generations
    assert(gens.count(_.state == "accepted") >= 2, "expected accepted generations")
    assertEquals(gens.count(_.state == "running"), 1, "exactly one generation is in flight")
    // two abandoned generations naming the same parent are what a branch stack is
    val stacks = gens.filter(_.state == "abandoned").groupBy(_.parent).values.map(_.size)
    assert(stacks.exists(_ >= 2), s"expected a stack of abandoned generations, got $stacks")

  test("the live loop walks the drive cycle: working, checking, evaluating"):
    val steps = loops(Scenarios.byName("loops")).flatMap(_.activity).distinct
    assert(steps.exists(_.endsWith("working")), steps.toString)
    assert(steps.exists(_.endsWith("checking")), steps.toString)
    assert(steps.exists(_.endsWith("evaluating")), steps.toString)

  test("a checker rejection costs the live generation an attempt"):
    val gen6 = loops(Scenarios.byName("loops")).flatMap(_.generations).filter(_.gen == 6)
    assert(gen6.exists(_.attempts.exists(_.check.exists(!_.pass))), "expected a rejected attempt")
    assert(gen6.exists(_.attempts.size == 2), "expected the rejection to cost a second attempt")

  test("the live loop's generation is accepted before the scenario ends, starting the next"):
    val last = loops(Scenarios.byName("loops")).filter(_.id == LoopScenario.LiveId).last
    assertEquals(last.generations.find(_.gen == 6).map(_.state), Some("accepted"))
    assertEquals(last.generations.find(_.gen == 7).map(_.state), Some("running"))
    assert(last.generations.find(_.gen == 6).flatMap(_.commit).isDefined, "an accepted generation has a commit")

  test("the live loop streams into the label its wire names"):
    val script = Scenarios.byName("loops")
    val labels = activity(script).filter(_.runId == LoopScenario.LiveId).map(_.nodeId).toSet
    val named = loops(script).flatMap(_.liveLabel).toSet
    assert(named.nonEmpty && named.subsetOf(labels), s"named $named but streamed $labels")

  test("every loop the fixture shows carries a definition the code panel can render"):
    loops(Scenarios.byName("loops")).foreach: l =>
      assert(l.defSource.contains("lib.loop.start"), s"${l.id}: ${l.defSource.take(40)}")
      assert(l.goal.nonEmpty && l.rubric.nonEmpty, l.id)

  test("the api answers a transcript for a settled generation and a diff for an attempt"):
    val transcript = LoopScenario.api(List("loop", LoopScenario.LiveId, "transcript", "gen-1-worker"))
      .getOrElse(fail("expected a transcript"))
    val frames = transcript.linesIterator.map(WireCodec.decode).toVector
    assert(frames.nonEmpty && frames.forall(_.isRight), s"a tee file is decodable JSONL: $frames")
    assert(frames.forall {
      case Right(WireMessage.Activity(e)) => e.runId == LoopScenario.LiveId && e.nodeId == "gen-1-worker"
      case _                              => false
    }, "every frame addresses the label it was served for")
    assert(LoopScenario.api(List("loop", LoopScenario.LiveId, "diff", "6", "2")).exists(_.startsWith("diff --git")))

  test("the api answers nothing for what was never recorded, so the browser sees a 404"):
    assertEquals(LoopScenario.api(List("loop", LoopScenario.LiveId, "transcript", "gen-9-worker")), None)
    assertEquals(LoopScenario.api(List("loop", "no-such-loop", "diff", "1", "1")), None)
    assertEquals(LoopScenario.api(Nil), None)

  test("every attempt the fixture says has a snapshot can be diffed, and vice versa"):
    val live = loops(Scenarios.byName("loops")).filter(_.id == LoopScenario.LiveId).last
    for
      g <- live.generations
      a <- g.attempts
      if LoopScenario.api(List("loop", LoopScenario.LiveId, "diff", g.gen.toString, a.attempt.toString)).isDefined
    do assert(a.hasSnapshot, s"gen ${g.gen} attempt ${a.attempt} serves a diff but claims no snapshot")

  // -- transcript coverage -----------------------------------------------------

  test("every fixture emits transcript activity, loops included"):
    Scenarios.names.foreach: name =>
      assert(activity(Scenarios.byName(name)).nonEmpty, s"$name emitted no transcript activity")

  test("the workflow fixtures are the ones this suite thinks they are"):
    assertEquals(workflowFixtures.toSet, Scenarios.names.toSet - "loops")

  test("every started node in a workflow fixture has at least one transcript event"):
    workflowFixtures.foreach: name =>
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

  test("every workflow fixture announces its code (so the code tab has content)"):
    workflowFixtures.foreach: name =>
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
