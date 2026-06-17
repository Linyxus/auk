package auk.webui

import auk.workflow.{Forest, ForestNode, NodeStatus, OrchestrationEvent, WireCodec, WireMessage}
import OrchestrationEvent.*

class SseClientSuite extends munit.FunSuite:

  test("step folds a valid Event line into the state"):
    val line = WireCodec.encode(WireMessage.Event(NodeDeclared("r", "a", None, Nil)))
    assertEquals(SseClient.step(AppState(), line).forests("r").nodes.map(_.id), Vector("a"))

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
