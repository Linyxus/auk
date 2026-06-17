package auk.workflow

import TranscriptEvent.*
import auk.workflow.TranscriptItem as I

/** The pure `Transcript.update` fold — streamed prose/thinking deltas accumulate,
  * and a tool call's output fills in when it returns. Shared by the host and the
  * web UI. */
class TranscriptSuite extends munit.FunSuite:

  test("a single Said becomes one prose item"):
    val t = Transcript.empty.update(Said("r", "a", "hello"))
    assertEquals(t.items, Vector(I.Said("hello")))

  test("consecutive Said deltas concatenate into one prose item (streaming)"):
    val t = Transcript.empty
      .update(Said("r", "a", "Let me "))
      .update(Said("r", "a", "inspect "))
      .update(Said("r", "a", "the code."))
    assertEquals(t.items, Vector(I.Said("Let me inspect the code.")))

  test("consecutive Thought deltas concatenate into one thinking item"):
    val t = Transcript.empty
      .update(Thought("r", "a", "first "))
      .update(Thought("r", "a", "second"))
    assertEquals(t.items, Vector(I.Thought("first second")))

  test("a ToolCalled adds a tool item with no output yet"):
    val t = Transcript.empty.update(ToolCalled("r", "a", "c1", "eval_scala", """{"code":"1"}"""))
    assertEquals(t.items, Vector(I.ToolCall("c1", "eval_scala", """{"code":"1"}""", None, false)))

  test("a ToolReturned fills the matching call's output by callId"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "grep", "pat"))
      .update(ToolReturned("r", "a", "c1", "3 matches", false))
    assertEquals(t.items, Vector(I.ToolCall("c1", "grep", "pat", Some("3 matches"), false)))

  test("a ToolReturned with isError marks the call as an error"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "grep", "pat"))
      .update(ToolReturned("r", "a", "c1", "no such file", true))
    assertEquals(t.items.head.asInstanceOf[I.ToolCall].isError, true)

  test("ToolReturned for an unknown callId is a no-op"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "grep", "pat"))
      .update(ToolReturned("r", "a", "other", "x", false))
    assertEquals(t.items, Vector(I.ToolCall("c1", "grep", "pat", None, false)))

  test("ToolReturned fills only the first matching open call when ids repeat"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "grep", "p1"))
      .update(ToolCalled("r", "a", "c1", "grep", "p2"))
      .update(ToolReturned("r", "a", "c1", "done", false))
    assertEquals(t.items, Vector(
      I.ToolCall("c1", "grep", "p1", Some("done"), false),
      I.ToolCall("c1", "grep", "p2", None, false)
    ))

  test("prose interrupted by a tool call starts a fresh prose run afterward"):
    val t = Transcript.empty
      .update(Said("r", "a", "Looking… "))
      .update(ToolCalled("r", "a", "c1", "grep", "p"))
      .update(ToolReturned("r", "a", "c1", "ok", false))
      .update(Said("r", "a", "Found it."))
    assertEquals(t.items, Vector(
      I.Said("Looking… "),
      I.ToolCall("c1", "grep", "p", Some("ok"), false),
      I.Said("Found it.")
    ))

  test("interleaved prose and thinking keep separate runs"):
    val t = Transcript.empty
      .update(Said("r", "a", "A"))
      .update(Thought("r", "a", "B"))
      .update(Said("r", "a", "C"))
    assertEquals(t.items, Vector(I.Said("A"), I.Thought("B"), I.Said("C")))

  test("update is a pure left-fold: folding a fixed event list is stable"):
    val events = List(
      Said("r", "a", "hi "), Said("r", "a", "there"),
      ToolCalled("r", "a", "c1", "t", "in"), ToolReturned("r", "a", "c1", "out", false),
      Said("r", "a", "bye")
    )
    val once = events.foldLeft(Transcript.empty)(_.update(_))
    val twice = events.foldLeft(Transcript.empty)(_.update(_))
    assertEquals(once, twice)
