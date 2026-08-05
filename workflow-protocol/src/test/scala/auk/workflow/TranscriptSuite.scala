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

  // -- live progress on an open call -------------------------------------------

  private val compiling = Map(EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> "900")
  private val running = Map(EvalDisplay.MetaPhase -> "running", EvalDisplay.MetaPhaseMs -> "40")

  private def call(t: Transcript): I.ToolCall = t.items.head.asInstanceOf[I.ToolCall]

  test("a ToolProgressed lands on the open call with its arrival time as the anchor"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", """{"code":"1"}"""))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(5_000L))
    assertEquals(call(t).progress, compiling)
    assertEquals(call(t).progressAtMs, Some(5_000L))

  test("each report replaces the last — a key absent from the newest is gone, not stale"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "c1", compiling ++ Map(EvalDisplay.MetaWorkerPid -> "4211")), Some(1_000L))
      .update(ToolProgressed("r", "a", "c1", running), Some(2_000L))
    assertEquals(call(t).progress, running)
    assertEquals(call(t).progressAtMs, Some(2_000L))

  test("a consumer with no clock gets a report with no anchor"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "c1", compiling))
    assertEquals(call(t).progress, compiling)
    assertEquals(call(t).progressAtMs, None)

  test("ToolReturned clears the progress and its anchor — a closed call renders statically"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(1_000L))
      .update(ToolReturned("r", "a", "c1", "res0: Int = 1", false), Some(2_000L))
    assertEquals(call(t), I.ToolCall("c1", "eval_scala", "{}", Some("res0: Int = 1"), false))
    assertEquals(call(t).progress, Map.empty[String, String])
    assertEquals(call(t).progressAtMs, None)

  test("a ToolProgressed after the call returned is a no-op — a closed call never ticks again"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolReturned("r", "a", "c1", "done", false))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(9_000L))
    assertEquals(call(t).progress, Map.empty[String, String])
    assertEquals(call(t).progressAtMs, None)

  test("ToolProgressed for an unknown callId is a no-op"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "other", compiling), Some(1_000L))
    assertEquals(call(t).progress, Map.empty[String, String])

  test("ToolProgressed reaches only the first open call when ids repeat"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "p1"))
      .update(ToolCalled("r", "a", "c1", "eval_scala", "p2"))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(1_000L))
    assertEquals(t.items.map(_.asInstanceOf[I.ToolCall].progress), Vector(compiling, Map.empty[String, String]))

  test("progress does not disturb the items around it"):
    val t = Transcript.empty
      .update(Said("r", "a", "running it"))
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(1_000L))
    assertEquals(t.items.size, 2)
    assertEquals(t.items.head, I.Said("running it"))

  test("toEvents never replays progress — it is live-only, so it cannot reach a log"):
    val t = Transcript.empty
      .update(ToolCalled("r", "a", "c1", "eval_scala", "{}"))
      .update(ToolProgressed("r", "a", "c1", compiling), Some(1_000L))
    assertEquals(t.toEvents("r", "a"), Vector(ToolCalled("r", "a", "c1", "eval_scala", "{}")))
    // Replay therefore rebuilds the durable transcript minus the live annotation:
    // the next report re-establishes it about a second later.
    val rebuilt = t.toEvents("r", "a").foldLeft(Transcript.empty)(_.update(_))
    assertEquals(call(rebuilt).progress, Map.empty[String, String])
    assertEquals(rebuilt.items.map(_.asInstanceOf[I.ToolCall].copy(progress = compiling, progressAtMs = Some(1_000L))),
      t.items)

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

  test("a Received becomes its own atomic item"):
    val t = Transcript.empty.update(Received("r", "a", "lead", "please do it"))
    assertEquals(t.items, Vector(I.Received("lead", "please do it")))

  test("two Received messages stay separate items — they never accumulate"):
    val t = Transcript.empty
      .update(Received("r", "a", "lead", "first"))
      .update(Received("r", "a", "m02", "second"))
    assertEquals(t.items, Vector(I.Received("lead", "first"), I.Received("m02", "second")))

  test("a Received closes the open prose run; the next delta starts a new one"):
    val t = Transcript.empty
      .update(Said("r", "a", "working on it"))
      .update(Received("r", "a", "lead", "also do this"))
      .update(Said("r", "a", "on it"))
    assertEquals(t.items, Vector(
      I.Said("working on it"),
      I.Received("lead", "also do this"),
      I.Said("on it")
    ))

  test("a Received closes the open thinking run too"):
    val t = Transcript.empty
      .update(Thought("r", "a", "hmm"))
      .update(Received("r", "a", "lead", "hurry"))
      .update(Thought("r", "a", "ok"))
    assertEquals(t.items, Vector(I.Thought("hmm"), I.Received("lead", "hurry"), I.Thought("ok")))

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

  test("toEvents is the inverse of update: replaying it rebuilds the transcript"):
    val t = Transcript.empty
      .update(Received("r", "a", "lead", "do the thing"))
      .update(Said("r", "a", "Hello "))
      .update(Said("r", "a", "world"))
      .update(Thought("r", "a", "hmm"))
      .update(Received("r", "a", "m02", "and this too"))
      .update(ToolCalled("r", "a", "c1", "grep", "pat"))
      .update(ToolReturned("r", "a", "c1", "3 hits", false))
      .update(ToolCalled("r", "a", "c2", "eval_scala", "1 + 1")) // left open (no return)
    val rebuilt = t.toEvents("r", "a").foldLeft(Transcript.empty)(_.update(_))
    assertEquals(rebuilt, t)

  test("toEvents tags every event with the given run and node id"):
    val t = Transcript.empty.update(Said("x", "y", "hi")).update(ToolCalled("x", "y", "c1", "grep", "p"))
    assert(t.toEvents("RUN", "NODE").forall(e => e.runId == "RUN" && e.nodeId == "NODE"))

  test("an open tool call re-expands to a ToolCalled with no ToolReturned"):
    val t = Transcript.empty.update(ToolCalled("r", "a", "c1", "grep", "p"))
    assertEquals(t.toEvents("r", "a"), Vector(ToolCalled("r", "a", "c1", "grep", "p")))

  // -- chunked storage: O(1) append, no per-delta re-concatenation --------------

  test("a streamed prose run keeps each delta as a separate chunk (no concatenation)"):
    val t = Transcript.empty
      .update(Said("r", "a", "one "))
      .update(Said("r", "a", "two "))
      .update(Said("r", "a", "three"))
    val said = t.items.head.asInstanceOf[I.Said]
    assertEquals(said.chunks, Vector("one ", "two ", "three"))
    assertEquals(said.text, "one two three")

  test("a streamed thinking run keeps each delta as a separate chunk"):
    val t = Transcript.empty.update(Thought("r", "a", "a")).update(Thought("r", "a", "b")).update(Thought("r", "a", "c"))
    assertEquals(t.items.head.asInstanceOf[I.Thought].chunks, Vector("a", "b", "c"))

  test("Said/Thought are equal by text regardless of how the chunks were split"):
    assertEquals(new I.Said(Vector("ab", "c")), I.Said("abc"))
    assertEquals(new I.Said(Vector("ab", "c")).hashCode, I.Said("abc").hashCode)
    assert(new I.Said(Vector("a")) != I.Said("b"))
    assertEquals(new I.Thought(Vector("x", "y")), I.Thought("xy"))
    assert((new I.Said(Vector("ab")): TranscriptItem) != new I.Thought(Vector("ab"))) // different kinds, same text

  test("appended grows a run by one chunk and leaves the original untouched (immutable)"):
    val s0 = I.Said("a")
    val s1 = s0.appended("b")
    assertEquals(s0.chunks, Vector("a"))
    assertEquals(s1.chunks, Vector("a", "b"))
    assertEquals(s1.text, "ab")

  test("toEvents collapses a multi-chunk run to a single fully-accumulated event"):
    val t = Transcript.empty
      .update(Said("r", "a", "Hel")).update(Said("r", "a", "lo ")).update(Said("r", "a", "there"))
    assertEquals(t.toEvents("r", "a"), Vector(Said("r", "a", "Hello there")))

  test("replaying a long stream rebuilds an equal but single-chunk run (linear, not re-streamed)"):
    val many = (1 to 500).foldLeft(Transcript.empty)((t, i) => t.update(Said("r", "a", i.toString)))
    val rebuilt = many.toEvents("r", "a").foldLeft(Transcript.empty)(_.update(_))
    assertEquals(rebuilt, many) // text-equal
    assertEquals(many.items.head.asInstanceOf[I.Said].chunks.size, 500) // original kept every delta as a chunk
    assertEquals(rebuilt.items.head.asInstanceOf[I.Said].chunks.size, 1) // replay collapsed them to one
