package auk.workflow

import OrchestrationEvent.*

/** Round-trip every wire message through encode/decode, and confirm malformed
  * input degrades to `Left` rather than throwing. */
class WireCodecSuite extends munit.FunSuite:

  private def roundtrip(m: WireMessage): WireMessage =
    val json = WireCodec.encode(m)
    WireCodec.decode(json) match
      case Right(d) => d
      case Left(e)  => fail(s"decode failed: $e  (json: $json)")

  private def ev(e: OrchestrationEvent): WireMessage = WireMessage.Event(e)

  // -- events ------------------------------------------------------------------

  test("GroupDeclared round-trips with parent None and Some"):
    assertEquals(roundtrip(ev(GroupDeclared("r", "g1", "hunt", "find bugs", None))),
      ev(GroupDeclared("r", "g1", "hunt", "find bugs", None)))
    assertEquals(roundtrip(ev(GroupDeclared("r", "g2", "verify", "check", Some("g1")))),
      ev(GroupDeclared("r", "g2", "verify", "check", Some("g1"))))

  test("NodeDeclared round-trips with empty and multi-element deps, group None/Some"):
    assertEquals(roundtrip(ev(NodeDeclared("r", "a", None, Nil))), ev(NodeDeclared("r", "a", None, Nil)))
    assertEquals(roundtrip(ev(NodeDeclared("r", "b", Some("g1"), List("a", "x")))),
      ev(NodeDeclared("r", "b", Some("g1"), List("a", "x"))))

  test("NodeQueued round-trips"):
    assertEquals(roundtrip(ev(NodeQueued("r", "a"))), ev(NodeQueued("r", "a")))

  test("NodeStarted round-trips the prompt, including special characters"):
    val e = NodeStarted("r", "a", "do \"x\"\nand y: ${z}")
    assertEquals(roundtrip(ev(e)), ev(e))

  test("NodeProgress round-trips tokens and Some/None currentTool"):
    assertEquals(roundtrip(ev(NodeProgress("r", "a", 12L, 3456L, Some("eval_scala")))),
      ev(NodeProgress("r", "a", 12L, 3456L, Some("eval_scala"))))
    assertEquals(roundtrip(ev(NodeProgress("r", "a", 0L, 0L, None))),
      ev(NodeProgress("r", "a", 0L, 0L, None)))

  test("NodeFinished round-trips ok=true and ok=false with summary"):
    assertEquals(roundtrip(ev(NodeFinished("r", "a", true, "ok"))), ev(NodeFinished("r", "a", true, "ok")))
    assertEquals(roundtrip(ev(NodeFinished("r", "a", false, "boom: it failed"))),
      ev(NodeFinished("r", "a", false, "boom: it failed")))

  test("NodeInterrupted round-trips"):
    assertEquals(roundtrip(ev(NodeInterrupted("r", "a"))), ev(NodeInterrupted("r", "a")))

  test("Log round-trips the message"):
    assertEquals(roundtrip(ev(Log("r", "hello world"))), ev(Log("r", "hello world")))

  test("WorkflowCode round-trips multi-line source with quotes"):
    val e = WorkflowCode("r", "wf.start:\n  agent[String](\"go\", id = \"x\")")
    assertEquals(roundtrip(ev(e)), ev(e))

  test("WorkflowFinished round-trips ok=true and ok=false with summary"):
    assertEquals(roundtrip(ev(WorkflowFinished("r", true, "Report(...)"))), ev(WorkflowFinished("r", true, "Report(...)")))
    assertEquals(roundtrip(ev(WorkflowFinished("r", false, "worker disconnected"))),
      ev(WorkflowFinished("r", false, "worker disconnected")))

  test("WorkflowPaused and WorkflowResumed round-trip"):
    assertEquals(roundtrip(ev(WorkflowPaused("r"))), ev(WorkflowPaused("r")))
    assertEquals(roundtrip(ev(WorkflowResumed("r"))), ev(WorkflowResumed("r")))

  test("a forest's run status survives a snapshot round-trip"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), status = RunStatus.Paused)
    roundtrip(WireMessage.Snapshot(List("r" -> f))) match
      case WireMessage.Snapshot(List((_, g))) => assertEquals(g.status, RunStatus.Paused)
      case other                              => fail(s"expected a snapshot, got $other")

  test("NodeStarted's prompt survives a forest round-trip via the node's prompt field"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running, prompt = Some("do the thing"))))
    val m = WireMessage.Snapshot(List("r" -> f))
    roundtrip(m) match
      case WireMessage.Snapshot(List((_, g))) => assertEquals(g.nodes.head.prompt, Some("do the thing"))
      case other                              => fail(s"expected a snapshot, got $other")

  // -- activity (transcript) ---------------------------------------------------

  private def act(e: TranscriptEvent): WireMessage = WireMessage.Activity(e)

  test("Said round-trips, including special characters"):
    val e = TranscriptEvent.Said("r", "a", "line one\nand \"two\": ${x}")
    assertEquals(roundtrip(act(e)), act(e))

  test("Thought round-trips"):
    assertEquals(roundtrip(act(TranscriptEvent.Thought("r", "a", "hmm, let me think"))),
      act(TranscriptEvent.Thought("r", "a", "hmm, let me think")))

  test("ToolCalled round-trips name and input JSON"):
    val e = TranscriptEvent.ToolCalled("r", "a", "c1", "eval_scala", """{"code":"1 + 1"}""")
    assertEquals(roundtrip(act(e)), act(e))

  test("ToolReturned round-trips output with isError true and false"):
    assertEquals(roundtrip(act(TranscriptEvent.ToolReturned("r", "a", "c1", "2", false))),
      act(TranscriptEvent.ToolReturned("r", "a", "c1", "2", false)))
    assertEquals(roundtrip(act(TranscriptEvent.ToolReturned("r", "a", "c1", "boom", true))),
      act(TranscriptEvent.ToolReturned("r", "a", "c1", "boom", true)))

  test("ToolProgressed round-trips its metadata map"):
    val e = TranscriptEvent.ToolProgressed("r", "a", "c1",
      Map(EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> "900", EvalDisplay.MetaWorkerPid -> "4211"))
    assertEquals(roundtrip(act(e)), act(e))

  test("ToolProgressed round-trips an empty metadata map"):
    val e = TranscriptEvent.ToolProgressed("r", "a", "c1", Map.empty)
    assertEquals(roundtrip(act(e)), act(e))

  test("ToolProgressed round-trips keys and values with special characters"):
    val e = TranscriptEvent.ToolProgressed("r", "a", "c1", Map("a.b\"c" -> "line one\nand \"two\": ${x}"))
    assertEquals(roundtrip(act(e)), act(e))

  test("a metadata field that is absent or not an object decodes as an empty map"):
    val absent = """{"kind":"activity","t":"toolProgressed","runId":"r","nodeId":"a","callId":"c1"}"""
    val wrongShape = """{"kind":"activity","t":"toolProgressed","runId":"r","nodeId":"a","callId":"c1","metadata":["x"]}"""
    for line <- List(absent, wrongShape) do
      assertEquals(WireCodec.decode(line), Right(act(TranscriptEvent.ToolProgressed("r", "a", "c1", Map.empty))))

  test("non-string metadata values are dropped, not guessed at"):
    val line =
      """{"kind":"activity","t":"toolProgressed","runId":"r","nodeId":"a","callId":"c1",""" +
        """"metadata":{"replPhase":"compiling","replPhaseMs":900,"x":null}}"""
    assertEquals(WireCodec.decode(line), Right(act(TranscriptEvent.ToolProgressed("r", "a", "c1",
      Map(EvalDisplay.MetaPhase -> "compiling")))))

  // -- the ephemeral contract: progress is wire-only ---------------------------
  //
  // The tees that persist transcripts live outside this module (the workflow,
  // team and loop bridges), so what is pinned here is the boundary they go
  // through: `encodeDurable` withholds exactly the events marked `ephemeral`,
  // and nothing else. The tables below are walked rather than spot-checked, so a
  // case added to either enum has to be placed on the durable/ephemeral side
  // deliberately instead of inheriting a default nobody looked at.

  /** One sample per [[TranscriptEvent]] case; [[eventCase]] keeps it complete. */
  private def everyEvent: List[TranscriptEvent] = List(
    TranscriptEvent.Said("r", "a", "hi"),
    TranscriptEvent.Thought("r", "a", "hmm"),
    TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "p"),
    TranscriptEvent.ToolProgressed("r", "a", "c1", Map(EvalDisplay.MetaPhase -> "running")),
    TranscriptEvent.ToolReturned("r", "a", "c1", "out", false),
    TranscriptEvent.Received("r", "a", "lead", "go")
  )

  /** Names a case. Deliberately total and default-free: a case added to the enum
    * makes this match inexhaustive, which is the compiler telling whoever added
    * it to give it a sample above and decide which side of the boundary it is
    * on. */
  private def eventCase(ev: TranscriptEvent): String = ev match
    case _: TranscriptEvent.Said           => "said"
    case _: TranscriptEvent.Thought        => "thought"
    case _: TranscriptEvent.ToolCalled     => "toolCalled"
    case _: TranscriptEvent.ToolProgressed => "toolProgressed"
    case _: TranscriptEvent.ToolReturned   => "toolReturned"
    case _: TranscriptEvent.Received       => "received"

  /** One sample per [[WireMessage]] case, likewise kept complete by [[messageCase]]. */
  private def everyMessage: List[WireMessage] = List(
    WireMessage.Snapshot(List("r" -> Forest.empty)),
    WireMessage.Event(NodeQueued("r", "a")),
    WireMessage.Activity(TranscriptEvent.Said("r", "a", "hi")),
    WireMessage.LoopSnapshot(List(loop(Nil))),
    WireMessage.Loop(loop(Nil))
  )

  private def messageCase(m: WireMessage): String = m match
    case _: WireMessage.Snapshot     => "snapshot"
    case _: WireMessage.Event        => "event"
    case _: WireMessage.Activity     => "activity"
    case _: WireMessage.LoopSnapshot => "loopSnapshot"
    case _: WireMessage.Loop         => "loop"

  test("the boundary tables carry every case of both enums, once each"):
    assertEquals(everyEvent.map(eventCase).distinct.size, everyEvent.size)
    assertEquals(everyMessage.map(messageCase).distinct.size, everyMessage.size)

  test("ToolProgressed is the only ephemeral event"):
    for e <- everyEvent do
      assertEquals(e.ephemeral, eventCase(e) == "toolProgressed", s"case=${eventCase(e)}")

  test("encodeDurable withholds every ephemeral event and encodes every other one"):
    for e <- everyEvent do
      val expected = if e.ephemeral then None else Some(WireCodec.encode(act(e)))
      assertEquals(WireCodec.encodeDurable(act(e)), expected, s"case=${eventCase(e)}")

  test("no other message kind is ever withheld — only an activity can be ephemeral"):
    for m <- everyMessage if messageCase(m) != "activity" do
      assertEquals(WireCodec.encodeDurable(m), Some(WireCodec.encode(m)), s"case=${messageCase(m)}")

  test("a progress frame still decodes — it is refused on write, never on read"):
    val e = TranscriptEvent.ToolProgressed("r", "a", "c1", Map(EvalDisplay.MetaPhase -> "compiling"))
    assertEquals(WireCodec.decode(WireCodec.encode(act(e))), Right(act(e)))

  test("Received round-trips its sender and text"):
    val e = TranscriptEvent.Received("r", "a", "lead", "please do it\nby \"noon\"")
    assertEquals(roundtrip(act(e)), act(e))

  test("decode returns Left on an activity with an unknown t"):
    assert(WireCodec.decode("""{"kind":"activity","t":"frobnicate","runId":"r","nodeId":"a"}""").isLeft)

  test("decode returns Left on an activity missing t"):
    assert(WireCodec.decode("""{"kind":"activity","runId":"r","nodeId":"a"}""").isLeft)

  // -- snapshots ---------------------------------------------------------------

  test("Snapshot round-trips an empty forest list"):
    assertEquals(roundtrip(WireMessage.Snapshot(Nil)), WireMessage.Snapshot(Nil))

  test("Snapshot round-trips multiple runs with groups, nodes (every status), and logs"):
    val f1 = Forest(
      groups = Vector(ForestGroup("g1", "hunt", "find"), ForestGroup("g2", "verify", "check")),
      nodes = Vector(
        ForestNode("a", Some("g1"), Nil, NodeStatus.Pending),
        ForestNode("b", Some("g1"), List("a"), NodeStatus.Queued),
        ForestNode("c", Some("g2"), List("b"), NodeStatus.Running, 10L, 20L, Some("eval_scala"), None),
        ForestNode("d", None, Nil, NodeStatus.Done, 5L, 6L, None, Some("done")),
        ForestNode("e", None, Nil, NodeStatus.Failed, 0L, 0L, None, Some("nope")),
        ForestNode("f", None, Nil, NodeStatus.Interrupted, 7L, 8L, None, None)
      ),
      logs = Vector("line 1", "line 2"),
      code = Some("wf.start(agent[String](\"go\"))")
    )
    val f2 = Forest(nodes = Vector(ForestNode("z", None, Nil, NodeStatus.Running)))
    val m = WireMessage.Snapshot(List("run-1" -> f1, "run-2" -> f2))
    assertEquals(roundtrip(m), m)

  // -- loops -------------------------------------------------------------------

  private def budgets = LoopBudgetsWire(50, 2, 3)

  private def attempt(n: Int, check: Option[LoopCheckWire], verdict: Option[LoopVerdictWire]) =
    LoopAttemptWire(n, s"try $n", s"""{"p99Ms":${70 + n}}""", hasSnapshot = true, check, verdict, "2026-07-30T12:00:00Z")

  private def loop(generations: List[LoopGenerationWire]) =
    LoopWire(
      id = "opt",
      phase = "running (gen 3)",
      goal = "cut p99 latency\nand keep it there",
      rubric = "faster is better",
      budgets = budgets,
      defSource = "lib.loop.start:\n  check { c => c.pass() }",
      defVersion = 2,
      held = true,
      parked = None,
      orphaned = false,
      activity = Some("gen 3, attempt 2 — evaluating"),
      stage = Some(LoopStageWire(3, 2, "evaluating", 4_100, 260)),
      liveLabel = Some("gen-3-eval"),
      generations = generations,
      createdAt = "2026-07-30T11:00:00Z",
      inputTokens = 128_000,
      outputTokens = 9_400
    )

  test("Loop round-trips a whole lineage: accepted, abandoned and running generations"):
    val accepted = LoopGenerationWire(
      gen = 1,
      parent = None,
      state = "accepted",
      description = "made it faster",
      metrics = List("allocMb" -> 12.0, "p99Ms" -> 90.0),
      commit = Some("c0ffee"),
      attempts = List(
        attempt(1, Some(LoopCheckWire(false, List("too slow", "leaks"), Nil)), None),
        attempt(2, Some(LoopCheckWire(true, Nil, List("p99Ms" -> 90.0))), Some(LoopVerdictWire(true, "good", false)))
      ),
      startedAt = "2026-07-30T11:01:00Z",
      settledAt = Some("2026-07-30T11:30:00Z"),
      inputTokens = 90_000,
      outputTokens = 6_000
    )
    val abandoned = LoopGenerationWire(2, Some(1), "abandoned", "a dead end", Nil, None, Nil, "2026-07-30T11:31:00Z", Some("2026-07-30T11:50:00Z"), 30_000, 2_000)
    val running = LoopGenerationWire(3, Some(1), "running", "", Nil, None, List(attempt(1, None, None)), "2026-07-30T11:51:00Z", None, 8_000, 1_400)
    val m = WireMessage.Loop(loop(List(accepted, abandoned, running)))
    assertEquals(roundtrip(m), m)

  test("a parked, orphaned, unheld loop round-trips its reason and its absent live fields"):
    val m = WireMessage.Loop(
      loop(Nil).copy(
        phase = "parked: budget exhausted",
        held = false,
        parked = Some("budget exhausted"),
        orphaned = true,
        activity = None,
        stage = None,
        liveLabel = None
      )
    )
    assertEquals(roundtrip(m), m)

  test("LoopSnapshot round-trips an empty list and several loops"):
    assertEquals(roundtrip(WireMessage.LoopSnapshot(Nil)), WireMessage.LoopSnapshot(Nil))
    val m = WireMessage.LoopSnapshot(List(loop(Nil), loop(Nil).copy(id = "other", held = false)))
    assertEquals(roundtrip(m), m)

  test("the drive cycle's stage round-trips as parts, and each step by name"):
    List("working", "checking", "evaluating").foreach: step =>
      val m = WireMessage.Loop(loop(Nil).copy(stage = Some(LoopStageWire(12, 3, step))))
      assertEquals(roundtrip(m), m)

  test("what a loop and its generations cost survives the wire, live stage included"):
    // A count large enough to matter: these are Longs on both sides, and a long-running
    // loop passes what an Int would hold.
    val gen = LoopGenerationWire(1, None, "accepted", "d", Nil, Some("c"), Nil, "t", Some("t"), 5_000_000_000L, 12L)
    val m = WireMessage.Loop(
      loop(List(gen)).copy(inputTokens = 5_000_000_000L, outputTokens = 12L, stage = Some(LoopStageWire(1, 1, "working", 7, 0)))
    )
    assertEquals(roundtrip(m), m)

  test("a loop frame from before the tokens were counted decodes as having cost nothing"):
    WireCodec.decode(
      """{"kind":"loop","loop":{"id":"opt","stage":{"gen":2,"attempt":1,"step":"working"},""" +
        """"generations":[{"gen":1,"state":"accepted"}]}}""") match
      case Right(WireMessage.Loop(l)) =>
        assertEquals((l.inputTokens, l.outputTokens), (0L, 0L))
        assertEquals(l.stage.map(s => (s.inputTokens, s.outputTokens)), Some((0L, 0L)))
        assertEquals(l.generations.map(g => (g.inputTokens, g.outputTokens)), List((0L, 0L)))
      case other => fail(s"expected a tolerant decode, got $other")

  test("a stage the browser has no station for still decodes, rather than losing the loop"):
    WireCodec.decode("""{"kind":"loop","loop":{"id":"opt","stage":{"gen":2,"attempt":1,"step":"dancing"}}}""") match
      case Right(WireMessage.Loop(l)) => assertEquals(l.stage, Some(LoopStageWire(2, 1, "dancing", 0, 0)))
      case other                      => fail(s"expected a tolerant decode, got $other")

  test("a metric key that looks like a number keeps its place in the list"):
    // JS objects reorder integer-like keys, which is why metrics travel as pairs.
    val keys = List("10" -> 1.0, "2" -> 2.0, "zz" -> 3.0, "1" -> 4.0)
    val gen = LoopGenerationWire(1, None, "accepted", "d", keys, Some("c"), Nil, "t", Some("t"))
    roundtrip(WireMessage.Loop(loop(List(gen)))) match
      case WireMessage.Loop(l) => assertEquals(l.generations.head.metrics, keys)
      case other               => fail(s"expected a loop, got $other")

  test("a multi-line goal and definition source survive the wire whole"):
    val source = "lib.loop.start:\n  goal(\"go \\\"fast\\\"\")\n  check { c => c.pass() }"
    val m = WireMessage.Loop(loop(Nil).copy(goal = "line one\nline two: ${x}", defSource = source))
    assertEquals(roundtrip(m), m)

  test("decode tolerates a loop frame missing every optional field"):
    WireCodec.decode("""{"kind":"loop","loop":{"id":"opt"}}""") match
      case Right(WireMessage.Loop(l)) =>
        assertEquals(l.id, "opt")
        assertEquals(l.parked, None)
        assertEquals(l.stage, None)
        assertEquals(l.generations, Nil)
        assertEquals(l.budgets, LoopBudgetsWire(0, 0, 0))
      case other => fail(s"expected a tolerant decode, got $other")

  // -- malformed input ---------------------------------------------------------

  test("decode returns Left on non-JSON input"):
    assert(WireCodec.decode("not json at all").isLeft)

  test("decode returns Left on JSON missing the kind discriminator"):
    assert(WireCodec.decode("""{"runId":"r"}""").isLeft)

  test("decode returns Left on an unknown kind"):
    assert(WireCodec.decode("""{"kind":"bogus"}""").isLeft)

  test("decode returns Left on an event missing t"):
    assert(WireCodec.decode("""{"kind":"event","runId":"r"}""").isLeft)

  test("decode returns Left on an event with an unknown t"):
    assert(WireCodec.decode("""{"kind":"event","t":"frobnicate","runId":"r"}""").isLeft)

  test("decode never throws on truncated JSON"):
    assert(WireCodec.decode("""{"kind":"even""").isLeft)
    assert(WireCodec.decode("").isLeft)

  // -- table-driven identity ---------------------------------------------------

  test("encode then decode is identity for a representative message of every kind"):
    val all: List[WireMessage] = List(
      ev(GroupDeclared("r", "g", "n", "d", Some("p"))),
      ev(NodeDeclared("r", "a", Some("g"), List("x", "y"))),
      ev(NodeQueued("r", "a")),
      ev(NodeStarted("r", "a", "prompt")),
      ev(NodeProgress("r", "a", 7L, 9L, Some("t"))),
      ev(NodeFinished("r", "a", false, "s")),
      ev(Log("r", "m")),
      ev(WorkflowCode("r", "wf.start(...)")),
      ev(WorkflowFinished("r", true, "Report(...)")),
      ev(WorkflowPaused("r")),
      ev(WorkflowResumed("r")),
      act(TranscriptEvent.Said("r", "a", "hello")),
      act(TranscriptEvent.Thought("r", "a", "ponder")),
      act(TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "{}")),
      act(TranscriptEvent.ToolReturned("r", "a", "c1", "out", false)),
      WireMessage.Snapshot(List("r" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))))),
      WireMessage.Loop(loop(Nil)),
      WireMessage.LoopSnapshot(List(loop(Nil)))
    )
    all.foreach(m => assertEquals(roundtrip(m), m, s"failed for $m"))
