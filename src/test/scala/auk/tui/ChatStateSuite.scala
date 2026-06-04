package auk.tui

import auk.llm.endpoint.Message
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}

class ChatStateSuite extends munit.FunSuite:

  private val base = ChatState.initial

  test("recall is a no-op with no history"):
    assertEquals(base.recallPrev, base)
    assertEquals(base.recallNext, base)

  test("submitted appends to the transcript and the input history"):
    val s = base.submitted("one")
    assertEquals(s.history, Vector(Entry.User("one")))
    assertEquals(s.inputHistory, Vector("one"))
    assertEquals(s.input, "")
    assertEquals(s.histNav, 1) // parked at the draft slot

  test("submitted collapses an immediate repeat"):
    val s = base.submitted("a").submitted("a").submitted("b").submitted("a")
    assertEquals(s.inputHistory, Vector("a", "b", "a"))

  test("key bindings overlay can be opened and closed"):
    assertEquals(base.overlay, Overlay.None)
    assertEquals(base.showKeyBindings.overlay, Overlay.KeyBindings)
    assertEquals(base.showKeyBindings.hideOverlay.overlay, Overlay.None)

  test("session picker selection stays within available sessions"):
    val sessions = Vector(
      SessionSummary("a", None, 1, "first"),
      SessionSummary("b", None, 2, "second")
    )
    val s = base.showSessionPicker(sessions)
    assertEquals(s.selectedSessionId, Some("a"))
    assertEquals(s.moveSessionSelection(1).selectedSessionId, Some("b"))
    assertEquals(s.moveSessionSelection(5).selectedSessionId, Some("b"))
    assertEquals(s.moveSessionSelection(-5).selectedSessionId, Some("a"))

  test("switching sessions rebuilds transcript and input history"):
    val events = List(
      SessionEvent.UserSubmitted("old prompt"),
      SessionEvent.AssistantResponded(Message.assistant("old answer"))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(input = "draft", cursor = 5).showKeyBindings.switchedTo(snapshot)
    assertEquals(switched.history, Vector(Entry.User("old prompt"), Entry.Assistant(Vector(Block.Answer(Typewriter.shown("old answer"))))))
    assertEquals(switched.inputHistory, Vector("old prompt"))
    assertEquals(switched.input, "")
    assertEquals(switched.overlay, Overlay.None)
    assertEquals(switched.transcriptEpoch, base.transcriptEpoch + 1)

  test("Up walks back through history and stops at the oldest"):
    val s = base.submitted("one").submitted("two")
    val u1 = s.recallPrev
    assertEquals(u1.input, "two")
    val u2 = u1.recallPrev
    assertEquals(u2.input, "one")
    val u3 = u2.recallPrev
    assertEquals(u3.input, "one") // can't go older

  test("Down walks forward and restores the draft past the newest"):
    val s = base.submitted("one").submitted("two").copy(input = "draft")
    val up = s.recallPrev.recallPrev // -> "one"
    assertEquals(up.input, "one")
    val d1 = up.recallNext // -> "two"
    assertEquals(d1.input, "two")
    val d2 = d1.recallNext // -> draft
    assertEquals(d2.input, "draft")
    assertEquals(d2.histNav, d2.inputHistory.size)

  test("the live draft is stashed on the first Up and restored on Down"):
    val s = base.submitted("cmd").copy(input = "typing…")
    val up = s.recallPrev
    assertEquals(up.input, "cmd")
    assertEquals(up.draft, "typing…")
    assertEquals(up.recallNext.input, "typing…")

  test("recallNext is a no-op while editing the draft"):
    val s = base.submitted("x")
    assertEquals(s.recallNext, s)

  test("recall puts the cursor at the end of the recalled line"):
    val s = base.submitted("hello").submitted("world")
    val up = s.recallPrev
    assertEquals(up.input, "world")
    assertEquals(up.cursor, 5)

  // ---- line editing ----

  /** Build a draft state with the cursor at `cur`. */
  private def line(text: String, cur: Int): ChatState =
    base.copy(input = text, cursor = cur)

  test("insert places the character at the cursor and advances it"):
    val s = line("ac", 1).insert('b')
    assertEquals(s.input, "abc")
    assertEquals(s.cursor, 2)

  test("insert supports newline characters"):
    val s = line("ab", 1).insert('\n')
    assertEquals(s.input, "a\nb")
    assertEquals(s.cursor, 2)

  test("backspace deletes before the cursor; no-op at the start"):
    val s = line("abc", 2).backspace
    assertEquals((s.input, s.cursor), ("ac", 1))
    assertEquals(line("abc", 0).backspace, line("abc", 0))

  test("deleteForward removes the char under the cursor; no-op at the end"):
    val s = line("abc", 1).deleteForward
    assertEquals((s.input, s.cursor), ("ac", 1))
    assertEquals(line("abc", 3).deleteForward, line("abc", 3))

  test("cursor movement clamps to the line bounds"):
    assertEquals(line("abc", 0).cursorLeft.cursor, 0)
    assertEquals(line("abc", 3).cursorRight.cursor, 3)
    assertEquals(line("abc", 1).cursorHome.cursor, 0)
    assertEquals(line("abc", 1).cursorEnd.cursor, 3)

  test("Ctrl+K kills to end, Ctrl+U kills to start"):
    val k = line("hello world", 5).killToEnd
    assertEquals((k.input, k.cursor), ("hello", 5))
    val u = line("hello world", 6).killToStart
    assertEquals((u.input, u.cursor), ("world", 0))

  test("Ctrl+W deletes the previous word, including trailing spaces"):
    val s = line("foo bar  ", 9).deleteWordBack
    assertEquals((s.input, s.cursor), ("foo ", 4))

  test("Ctrl+W deletes a word in the middle, keeping the tail"):
    val s = line("foo bar baz", 7).deleteWordBack // cursor after "bar"
    assertEquals((s.input, s.cursor), ("foo  baz", 4))

  // ---- streaming a reply: thinking -> tools -> answer -> done ----

  private def waiting = base.copy(phase = Phase.Waiting)

  test("thinking accumulates as one open block"):
    val s = waiting.appendThinking("rea", now = 1000).appendThinking("soning", now = 1100)
    // Open reasoning: text has arrived but is not yet revealed (shown = 0).
    assertEquals(s.streamingBlocks, Vector(Block.Thinking(Typewriter("reasoning", 0), 1000, None)))

  test("answering collapses prior reasoning into a fixed duration"):
    val s = waiting.appendThinking("reasoning", now = 1000).appendReply("hi", now = 3500)
    assertEquals(
      s.streamingBlocks,
      // The answer has arrived but not yet been revealed (shown = 0).
      // Collapsed reasoning is settled (its text no longer shows); the answer is unrevealed.
      Vector(Block.Thinking(Typewriter.shown("reasoning"), 1000, Some(2500)), Block.Answer(Typewriter("hi", 0)))
    )

  test("answer deltas accumulate into one block; no thinking, no duration"):
    val s = waiting.appendReply("hi", now = 500).appendReply(" there", now = 540)
    assertEquals(s.streamingBlocks, Vector(Block.Answer(Typewriter("hi there", 0))))

  test("a tool call collapses prior reasoning and records name + streamed args"):
    val s = waiting
      .appendThinking("hmm", now = 1000)
      .startTool("t1", "read", now = 1500)
      .appendToolArgs("""{"path":""")
      .appendToolArgs(""""a.scala"}""")
    assertEquals(
      s.streamingBlocks,
      Vector(
        Block.Thinking(Typewriter.shown("hmm"), 1000, Some(500L)),
        Block.Tool("t1", "read", """{"path":"a.scala"}""")
      )
    )

  test("blocks keep their arrival order across a tool round"):
    val s = waiting
      .appendThinking("plan", now = 100)
      .startTool("t1", "bash", now = 200)
      .appendReply("done", now = 300)
    assertEquals(
      s.streamingBlocks,
      Vector(
        Block.Thinking(Typewriter.shown("plan"), 100, Some(100L)),
        Block.Tool("t1", "bash", ""),
        Block.Answer(Typewriter("done", 0))
      )
    )

  test("a running tool records its start but no elapsed/tokens yet"):
    val s = waiting.startTool("t1", "sub_agent", now = 1000).startToolRun("t1", now = 1200)
    assertEquals(
      s.streamingBlocks,
      Vector(Block.Tool("t1", "sub_agent", "", startedMs = Some(1200), elapsedMs = None, tokens = None))
    )

  test("finishing a tool freezes the duration and totals the tokens"):
    val s = waiting
      .startTool("t1", "sub_agent", now = 1000)
      .startToolRun("t1", now = 1200)
      .endToolRun("t1", Map("inputTokens" -> "300", "outputTokens" -> "120"), now = 5200)
    assertEquals(
      s.streamingBlocks.head,
      Block.Tool("t1", "sub_agent", "", startedMs = Some(1200), elapsedMs = Some(4000L), tokens = Some(420L))
    )

  test("a finished tool with no token metadata carries no token total"):
    val s = waiting
      .startTool("t1", "bash", now = 1000)
      .startToolRun("t1", now = 1000)
      .endToolRun("t1", Map("exitCode" -> "0"), now = 1050)
    assertEquals(s.streamingBlocks.head.asInstanceOf[Block.Tool].tokens, None)

  test("run events target the matching tool by id, leaving others untouched"):
    val s = waiting
      .startTool("t1", "read", now = 100)
      .startTool("t2", "sub_agent", now = 200)
      .startToolRun("t2", now = 250)
    val blocks = s.streamingBlocks.collect { case t: Block.Tool => t }
    assertEquals(blocks.find(_.id == "t1").flatMap(_.startedMs), None)
    assertEquals(blocks.find(_.id == "t2").flatMap(_.startedMs), Some(250L))

  /** Drive the reveal to completion as the UI's tick loop would: advance until
    * settled, then commit. */
  private def revealed(s: ChatState): ChatState =
    if s.idle then s
    else Iterator.iterate(s)(_.advanceReveal).find(_.revealSettled).fold(s)(_.commitIfDrained)

  test("finishReply keeps the turn live until the reveal has drained"):
    val finished = waiting
      .appendReply("hello world", now = 1000)
      .finishReply(fallback = "", now = 1100)
    // Closed, but still in the live region with nothing committed yet.
    assert(!finished.idle)
    assert(!finished.revealSettled)
    assertEquals(finished.history, Vector.empty)

  test("open reasoning reveals a prefix at a time, like the answer"):
    val s = waiting.appendThinking("reasoning about the problem", now = 1000)
    val shown = s.advanceReveal.streamingBlocks.collect { case Block.Thinking(t, _, None) => t.visible }.head
    assert(shown.nonEmpty && shown.length < "reasoning about the problem".length, shown)
    assert("reasoning about the problem".startsWith(shown), shown)

  test("advanceReveal uncovers the answer a prefix at a time"):
    val finished = waiting
      .appendReply("hello world", now = 1000)
      .finishReply(fallback = "", now = 1100)
    val shown = finished.advanceReveal.streamingBlocks.collect { case Block.Answer(t) => t.visible }.head
    assert(shown.nonEmpty && shown.length < "hello world".length, shown)
    assert("hello world".startsWith(shown), shown)

  test("a drained turn commits the accumulated blocks, then idles"):
    val s = revealed(
      waiting
        .appendThinking("mull", now = 1000)
        .appendReply("answer", now = 2000)
        .finishReply(fallback = "ignored", now = 9999)
    )
    assertEquals(
      s.history.last,
      Entry.Assistant(Vector(Block.Thinking(Typewriter.shown("mull"), 1000, Some(1000L)), Block.Answer(Typewriter.shown("answer"))))
    )
    assert(s.idle)

  test("a turn with no streamed answer falls back to the Done text"):
    val s = revealed(
      waiting
        .appendThinking("just thinking", now = 1000)
        .finishReply(fallback = "final answer", now = 2500)
    )
    assertEquals(
      s.history.last,
      Entry.Assistant(
        Vector(Block.Thinking(Typewriter.shown("just thinking"), 1000, Some(1500L)), Block.Answer(Typewriter.shown("final answer")))
      )
    )

  test("an empty turn with no fallback adds no transcript entry"):
    val s = waiting.finishReply(fallback = "", now = 100)
    assertEquals(s.history, Vector.empty)
    assert(s.idle)

  test("a closed turn with no answer commits immediately (nothing to reveal)"):
    val s = waiting
      .startTool("t1", "bash", now = 1000)
      .endToolRun("t1", Map.empty, now = 1100)
      .finishReply(fallback = "", now = 1200)
    // No answer block means the reveal is already settled.
    assert(s.revealSettled)
    assert(s.commitIfDrained.idle)

  test("failed appends an error line and idles"):
    val s = waiting.failed("⚠ boom")
    assertEquals(s.history.last, Entry.Error("⚠ boom"))
    assert(s.idle)
