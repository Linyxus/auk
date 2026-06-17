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

  test("Log round-trips the message"):
    assertEquals(roundtrip(ev(Log("r", "hello world"))), ev(Log("r", "hello world")))

  test("WorkflowCode round-trips multi-line source with quotes"):
    val e = WorkflowCode("r", "wf.start:\n  agent[String](\"go\", id = \"x\")")
    assertEquals(roundtrip(ev(e)), ev(e))

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
        ForestNode("e", None, Nil, NodeStatus.Failed, 0L, 0L, None, Some("nope"))
      ),
      logs = Vector("line 1", "line 2"),
      code = Some("wf.start(agent[String](\"go\"))")
    )
    val f2 = Forest(nodes = Vector(ForestNode("z", None, Nil, NodeStatus.Running)))
    val m = WireMessage.Snapshot(List("run-1" -> f1, "run-2" -> f2))
    assertEquals(roundtrip(m), m)

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
      act(TranscriptEvent.Said("r", "a", "hello")),
      act(TranscriptEvent.Thought("r", "a", "ponder")),
      act(TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "{}")),
      act(TranscriptEvent.ToolReturned("r", "a", "c1", "out", false)),
      WireMessage.Snapshot(List("r" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))))
    )
    all.foreach(m => assertEquals(roundtrip(m), m, s"failed for $m"))
