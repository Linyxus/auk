package auk.webui

import auk.workflow.{Forest, ForestNode, NodeStatus, OrchestrationEvent, TranscriptEvent, TranscriptItem, WireCodec, WireMessage}
import OrchestrationEvent.*

class SseClientSuite extends munit.FunSuite:

  test("step folds a valid Event line into the state"):
    val line = WireCodec.encode(WireMessage.Event(NodeDeclared("r", "a", None, Nil)))
    assertEquals(SseClient.step(AppState(), line).forests("r").nodes.map(_.id), Vector("a"))

  test("step folds a valid Activity line into the (run, node) transcript"):
    val line = WireCodec.encode(WireMessage.Activity(TranscriptEvent.Said("r", "a", "hi")))
    assertEquals(SseClient.step(AppState(), line).transcripts("r")("a").items, Vector(TranscriptItem.Said("hi")))

  test("step applies a Snapshot line, replacing forests"):
    val before = SseClient.step(AppState(), WireCodec.encode(WireMessage.Event(NodeDeclared("old", "x", None, Nil))))
    val snapLine = WireCodec.encode(WireMessage.Snapshot(List("r" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))))))
    val s = SseClient.step(before, snapLine)
    assert(!s.forests.contains("old"))
    assertEquals(s.forests("r").nodes.head.status, NodeStatus.Done)

  test("step ignores a malformed line, returning the state unchanged"):
    val s0 = AppState()
    assertEquals(SseClient.step(s0, "not json"), s0)
    assertEquals(SseClient.step(s0, """{"kind":"bogus"}"""), s0)

  test("step ignores an empty line (SSE keep-alive)"):
    val s0 = AppState()
    assertEquals(SseClient.step(s0, ""), s0)

  test("the selected node's transcript grows incrementally as frames stream in"):
    val r = "run-1"; val n = "alpha"
    def enc(m: WireMessage): String = WireCodec.encode(m)
    // frames in arrival order, exactly as the SSE stream delivers them
    val frames = List(
      enc(WireMessage.Event(NodeDeclared(r, n, None, Nil))),
      enc(WireMessage.Event(NodeStarted(r, n, "Inspect alpha"))),
      enc(WireMessage.Activity(TranscriptEvent.Said(r, n, "Looking "))),
      enc(WireMessage.Activity(TranscriptEvent.Said(r, n, "around. "))),
      enc(WireMessage.Activity(TranscriptEvent.ToolCalled(r, n, "c1", "eval_scala", """{"code":"1 + 1"}"""))),
      enc(WireMessage.Activity(TranscriptEvent.ToolReturned(r, n, "c1", "2", false))),
      enc(WireMessage.Event(NodeFinished(r, n, true, "done")))
    )
    // fold frame-by-frame (selecting the node, which becomes valid after frame 0),
    // snapshotting the rendered View after each — what the browser would re-render.
    var st = AppState()
    val views = frames.map: f =>
      st = SseClient.step(st, f).selectNode(n)
      WorkflowView.from(st)

    def agent(v: View): AgentView = v.panel match
      case PanelView.Agent(a) => a
      case other              => fail(s"expected an Agent panel, got $other")
    def prose(v: View): Option[String] = agent(v).rows.collectFirst { case TranscriptRow.Prose(cs) => cs.mkString }
    def tool(v: View): Option[TranscriptRow.Tool] = agent(v).rows.collectFirst { case t: TranscriptRow.Tool => t }

    // prose accumulates across the two Said deltas (streaming text)
    assertEquals(prose(views(2)), Some("Looking "))
    assertEquals(prose(views(3)), Some("Looking around. "))
    // the tool call appears first with no output (running), then its output fills in
    assertEquals(tool(views(4)).flatMap(_.output), None)
    assertEquals(tool(views(5)).flatMap(_.output), Some("2"))
    // the running flag tracks the node's lifecycle
    assert(agent(views(3)).streaming, "should still be streaming while running")
    assert(!agent(views(6)).streaming, "should stop streaming once finished")
    assertEquals(agent(views(6)).summary, Some("done"))
    // each step strictly extends the previous render (never shrinks)
    val rowCounts = views.drop(1).map(agent(_).rows.size)
    assert(rowCounts == rowCounts.sorted, s"row count should be monotonic, was $rowCounts")

  test("stepping encoded lines equals folding the decoded messages"):
    val msgs = List(
      WireMessage.Event(NodeDeclared("r", "a", None, Nil)),
      WireMessage.Event(NodeQueued("r", "a")),
      WireMessage.Event(NodeStarted("r", "a", "go")),
      WireMessage.Event(NodeFinished("r", "a", true, "ok"))
    )
    val viaLines = msgs.map(WireCodec.encode).foldLeft(AppState())(SseClient.step)
    val viaApply = msgs.foldLeft(AppState())((s, m) => s.reduce(m))
    assertEquals(viaLines, viaApply)
