package auk.tui

import auk.llm.endpoint.{ChatResponse, Content, FinishReason, Message, ReasoningBlock, Role, Usage}
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}

class ChatStateSuite extends munit.FunSuite:

  private val base = ChatState.initial

  private def responded(
      message: Message,
      finishReason: FinishReason = FinishReason.Stop,
      usage: Option[Usage] = None
  ): SessionEvent =
    SessionEvent.AssistantResponded(ChatResponse(message, finishReason, usage))

  test("recall is a no-op with no history"):
    assertEquals(base.recallPrev, base)
    assertEquals(base.recallNext, base)

  test("startingTurn stamps the clock and clears the live token accounting"):
    val dirty = base.copy(anchoredOutputTokens = 99, anchorChars = 42, clockMs = 1)
    val fresh = dirty.startingTurn(now = 5000)
    assertEquals(fresh.turnStartMs, 5000L)
    assertEquals(fresh.clockMs, 5000L)
    assertEquals(fresh.anchoredOutputTokens, 0L)
    assertEquals(fresh.anchorChars, 0L)

  test("streamedOutputChars sums thinking, answer, and tool-arg text"):
    val s = base.startingTurn(0)
      .appendThinking("abcde", now = 0)          // 5 chars
      .appendReply("hello world", now = 1)       // 11 chars (collapses thinking, full text kept)
    assertEquals(s.streamedOutputChars, 16L)

  test("anchorRoundUsage adds exact output tokens and advances the estimate baseline"):
    // Round 1 streams 100 chars of reasoning, then reports 30 exact output
    // tokens; round 2 then streams a 40-char answer (≈10 estimated tokens).
    val afterRound1 = base.startingTurn(0).appendThinking("x" * 100, now = 0)
    val anchored = afterRound1.anchorRoundUsage(Usage(inputTokens = 500, outputTokens = 30))
    assertEquals(anchored.anchoredOutputTokens, 30L)
    assertEquals(anchored.anchorChars, 100L) // baseline moved past round 1's chars
    val afterRound2 = anchored.appendReply("y" * 40, now = 1)
    // Only round 2's 40 chars are estimated; round 1 rests on its exact 30.
    assertEquals(afterRound2.streamedOutputChars - afterRound2.anchorChars, 40L)
    assertEquals(afterRound2.anchoredOutputTokens, 30L)

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

  test("debug info overlay can be opened and closed"):
    assertEquals(base.overlay, Overlay.None)
    assertEquals(base.showDebugInfo.overlay, Overlay.DebugInfo)
    assertEquals(base.showDebugInfo.hideOverlay.overlay, Overlay.None)

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
      responded(Message.assistant("old answer"))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(input = "draft", cursor = 5).showKeyBindings.switchedTo(snapshot)
    assertEquals(switched.history, Vector(Entry.User("old prompt"), Entry.Assistant(Vector(Block.shownAnswer("old answer")))))
    assertEquals(switched.inputHistory, Vector("old prompt"))
    assertEquals(switched.input, "")
    assertEquals(switched.overlay, Overlay.None)
    assertEquals(switched.transcriptEpoch, base.transcriptEpoch + 1)

  test("resuming restores the context gauge from the last reply's persisted usage"):
    val events = List(
      SessionEvent.UserSubmitted("q1"),
      responded(Message.assistant("a1"), usage = Some(Usage(10_000, 500))),
      SessionEvent.UserSubmitted("q2"),
      responded(Message.assistant("a2"), usage = Some(Usage(40_000, 1_000)))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000).switchedTo(snapshot)
    // The latest reply's input + output (41_000) drives the gauge: 41_000 / 200_000 ≈ 21%.
    assertEquals(switched.contextTokens, 41_000L)
    assertEquals(switched.contextPercentUsed, Some(21))

  test("resuming a usage-free session leaves the gauge at zero"):
    val events = List(
      SessionEvent.UserSubmitted("q1"),
      responded(Message.assistant("a1")) // no usage (e.g. a pre-usage log)
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000, contextTokens = 999).switchedTo(snapshot)
    assertEquals(switched.contextTokens, 0L)

  test("resuming after context compaction with no persisted estimate falls back to the summary"):
    val summary = "## Current Goal\nFinish compaction support."
    val events = List(
      SessionEvent.UserSubmitted("q1"),
      responded(Message.assistant("a1"), usage = Some(Usage(10_000, 500))),
      SessionEvent.ContextCompacted(summary) // old log: no persisted estimate
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000).switchedTo(snapshot)
    assertEquals(switched.history.last, Entry.ContextCompacted(summary))
    assertEquals(switched.inputHistory, Vector("q1"))
    assertEquals(switched.contextTokens, ChatState.estimatedTokens(summary))

  test("resuming after context compaction uses the persisted estimate over the summary"):
    val summary = "## Current Goal\nFinish compaction support."
    val events = List(
      responded(Message.assistant("a1"), usage = Some(Usage(10_000, 500))),
      // The persisted estimate includes system prompt + tool schemas, so it far
      // exceeds an estimate of the summary text; the gauge must honour it.
      SessionEvent.ContextCompacted(summary, estimatedTokens = Some(30_000))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000).switchedTo(snapshot)
    assertEquals(switched.contextTokens, 30_000L)

  test("a live context compaction event appends a marker and sets the gauge to the estimate"):
    val summary = "## Current Goal\nContinue."
    val compacted = base.copy(contextTokens = 99_000, phase = Phase.Waiting).contextCompacted(summary, 12_345)
    assertEquals(compacted.history, Vector(Entry.ContextCompacted(summary)))
    assertEquals(compacted.phase, Phase.Idle)
    // The engine's estimate is used verbatim (not re-derived from the summary).
    assertEquals(compacted.contextTokens, 12_345L)

  test("resuming counts trailing events after the last anchor"):
    // A session resumed mid-turn: the last measured reply is the anchor, and the
    // user line + tool results logged after it were never measured, so the gauge
    // must add their estimates rather than drop them.
    val toolOut = "x" * 400 // ~100 estimated tokens
    val events = List(
      responded(Message.assistant("a1"), usage = Some(Usage(40_000, 1_000))),
      SessionEvent.UserSubmitted("please continue"),
      SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", toolOut, isError = false)))
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000).switchedTo(snapshot)
    val expected = 41_000L + ChatState.estimatedTokens("please continue") + ChatState.estimatedTokens(toolOut)
    assertEquals(switched.contextTokens, expected)

  test("resuming counts a usage-free interrupt-partial reply as trailing"):
    // The persisted interrupt-partial (Engine.reconcileInterrupted) carries text
    // but no usage; its text is in context on resume and must be counted.
    val partial = "partial answer before the interrupt"
    val events = List(
      responded(Message.assistant("a1"), usage = Some(Usage(40_000, 1_000))),
      responded(Message.assistant(partial)) // no usage
    )
    val snapshot = SessionSnapshot(SessionSummary.from("s1", None, events), events)
    val switched = base.copy(contextWindow = 200_000).switchedTo(snapshot)
    assertEquals(switched.contextTokens, 41_000L + ChatState.estimatedTokens(partial))

  test("startCompaction enters a compacting phase with a fresh clock"):
    val compacting = base.copy(anchoredOutputTokens = 10, anchorChars = 4).showKeyBindings.startCompaction(1234)
    assertEquals(compacting.phase, Phase.Compacting)
    assertEquals(compacting.overlay, Overlay.None)
    assertEquals(compacting.turnStartMs, 1234L)
    assertEquals(compacting.clockMs, 1234L)
    assertEquals(compacting.anchoredOutputTokens, 0L)
    assertEquals(compacting.anchorChars, 0L)

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

  test("insertText splices a multi-line paste at the cursor"):
    val s = line("ab", 1).insertText("x\ny")
    assertEquals(s.input, "ax\nyb")
    assertEquals(s.cursor, 4)

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

  test("Ctrl+A / Ctrl+E go to the start/end of the current line, not the field"):
    // "ab\ncd": cursor 4 sits on the second line ("c|d").
    assertEquals(line("ab\ncd", 4).cursorHome.cursor, 3) // start of "cd"
    assertEquals(line("ab\ncd", 4).cursorEnd.cursor, 5)  // end of "cd" (field end)
    // cursor 1 sits on the first line ("a|b").
    assertEquals(line("ab\ncd", 1).cursorHome.cursor, 0) // start of "ab"
    assertEquals(line("ab\ncd", 1).cursorEnd.cursor, 2)  // end of "ab", before the newline

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

  // ---- vertical cursor movement across logical lines ----

  test("onFirstLine / onLastLine track the cursor's line"):
    // "ab\ncd": indices 0,1 on line 1; 3,4 on line 2; index 2 is the newline.
    assert(line("ab\ncd", 1).onFirstLine)
    assert(!line("ab\ncd", 4).onFirstLine)
    assert(!line("ab\ncd", 1).onLastLine)
    assert(line("ab\ncd", 4).onLastLine)
    // A single line is both first and last.
    assert(line("abc", 1).onFirstLine && line("abc", 1).onLastLine)

  test("cursorUp moves to the line above, keeping the column"):
    val s = line("hello\nworld", 9).cursorUp // column 3 on line 2 ("wor|ld")
    assertEquals(s.cursor, 3) // "hel|lo"

  test("cursorUp clamps the column to a shorter line above"):
    val s = line("ab\nlonger", 8).cursorUp // column 5 on line 2
    assertEquals(s.cursor, 2) // clamps to end of "ab"

  test("cursorUp is a no-op on the first line"):
    assertEquals(line("hello\nworld", 3).cursorUp, line("hello\nworld", 3))

  test("cursorDown moves to the line below, keeping the column"):
    val s = line("hello\nworld", 3).cursorDown // column 3 on line 1
    assertEquals(s.cursor, 9) // "wor|ld"

  test("cursorDown clamps the column to a shorter line below"):
    val s = line("longer\nab", 5).cursorDown // column 5 on line 1
    assertEquals(s.cursor, 9) // clamps to end of "ab"

  test("cursorDown is a no-op on the last line"):
    assertEquals(line("hello\nworld", 9).cursorDown, line("hello\nworld", 9))

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
      Vector(Block.Thinking(Typewriter.shown("reasoning"), 1000, Some(2500)), Block.answer(Typewriter("hi", 0)))
    )

  test("answer deltas accumulate into one block; no thinking, no duration"):
    val s = waiting.appendReply("hi", now = 500).appendReply(" there", now = 540)
    assertEquals(s.streamingBlocks, Vector(Block.answer(Typewriter("hi there", 0))))

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
        Block.Tool("t1", "read", """{"path":"a.scala"}""", streamingSinceMs = Some(1500L))
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
        Block.Tool("t1", "bash", "", streamingSinceMs = Some(200L)),
        Block.answer(Typewriter("done", 0))
      )
    )

  test("a running tool records its start but no elapsed/tokens yet"):
    val s = waiting.startTool("t1", "eval_scala", now = 1000).startToolRun("t1", now = 1200)
    // Two moments, both kept: when the model began writing the call, and when
    // the call began to run.
    assertEquals(
      s.streamingBlocks,
      Vector(
        Block.Tool(
          "t1",
          "eval_scala",
          "",
          startedMs = Some(1200),
          elapsedMs = None,
          tokens = None,
          streamingSinceMs = Some(1000L)
        )
      )
    )

  test("live progress updates a running tool's token total without freezing it"):
    val s = waiting
      .startTool("t1", "eval_scala", now = 1000)
      .startToolRun("t1", now = 1200)
      .progressToolRun("t1", Map("inputTokens" -> "10", "outputTokens" -> "5"), now = 1400)
      .progressToolRun("t1", Map("inputTokens" -> "30", "outputTokens" -> "12"), now = 2200)
    // The latest report is kept verbatim for the view, and it is the LATEST: the
    // first report's numbers are gone, not merged under the second's, and the
    // arrival stamp moved with them — a viewer extrapolating from the second
    // report must not anchor on when the first one landed.
    assertEquals(
      s.streamingBlocks.head,
      Block.Tool(
        "t1",
        "eval_scala",
        "",
        startedMs = Some(1200),
        elapsedMs = None,
        tokens = Some(42L),
        progress = Map("inputTokens" -> "30", "outputTokens" -> "12"),
        streamingSinceMs = Some(1000L),
        progressAtMs = Some(2200L)
      )
    )

  test("finishing a tool freezes the duration and totals the tokens"):
    val s = waiting
      .startTool("t1", "eval_scala", now = 1000)
      .startToolRun("t1", now = 1200)
      .endToolRun("t1", isError = false, Map("inputTokens" -> "300", "outputTokens" -> "120"), output = "", now = 5200)
    assertEquals(
      s.streamingBlocks.head,
      Block.Tool(
        "t1",
        "eval_scala",
        "",
        startedMs = Some(1200),
        elapsedMs = Some(4000L),
        tokens = Some(420L),
        streamingSinceMs = Some(1000L)
      )
    )

  test("a finished tool with no token metadata carries no token total"):
    val s = waiting
      .startTool("t1", "bash", now = 1000)
      .startToolRun("t1", now = 1000)
      .endToolRun("t1", isError = false, Map("exitCode" -> "0"), output = "ok", now = 1050)
    assertEquals(s.streamingBlocks.head.asInstanceOf[Block.Tool].tokens, None)

  test("run events target the matching tool by id, leaving others untouched"):
    val s = waiting
      .startTool("t1", "read", now = 100)
      .startTool("t2", "eval_scala", now = 200)
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
    val shown = finished.advanceReveal.streamingBlocks.collect { case Block.Answer(t, _) => t.visible }.head
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
      Entry.Assistant(Vector(Block.Thinking(Typewriter.shown("mull"), 1000, Some(1000L)), Block.shownAnswer("answer")))
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
        Vector(Block.Thinking(Typewriter.shown("just thinking"), 1000, Some(1500L)), Block.shownAnswer("final answer"))
      )
    )

  test("an empty turn with no fallback adds no transcript entry"):
    val s = waiting.finishReply(fallback = "", now = 100)
    assertEquals(s.history, Vector.empty)
    assert(s.idle)

  test("a closed turn with no answer commits immediately (nothing to reveal)"):
    val s = waiting
      .startTool("t1", "bash", now = 1000)
      .endToolRun("t1", isError = false, Map.empty, output = "", now = 1100)
      .finishReply(fallback = "", now = 1200)
    // No answer block means the reveal is already settled.
    assert(s.revealSettled)
    assert(s.commitIfDrained.idle)

  test("failed appends an error line and idles"):
    val s = waiting.failed("⚠ boom")
    assertEquals(s.history.last, Entry.Error("⚠ boom"))
    assert(s.idle)

  // ---- API-failure retry: rewind point, countdown, terminal commit ----

  test("roundStarted marks the rewind point and ends the retry countdown"):
    val s = waiting
      .appendReply("round one", now = 100)
      .copy(retry = Some(RetryState(1, 6, 5000)))
      .roundStarted
    assertEquals(s.roundMark, 1)
    assertEquals(s.retry, None)

  test("retrying rewinds the dead attempt's partial blocks to the round marker"):
    val s = waiting
      .appendReply("round one", now = 100)
      .startTool("t1", "read", now = 200)
      .roundStarted // the next round begins after one answer + one tool block
      .appendThinking("doomed reasoning", now = 300)
      .appendReply("doomed partial", now = 350)
      .retrying(attempt = 1, maxAttempts = 6, delayMs = 4000, now = 1000)
    // The failed attempt's thinking + partial answer are gone; the settled
    // round's blocks survive, and the countdown targets now + delay.
    assertEquals(
      s.streamingBlocks.map {
        case a: Block.Answer => a.typed.full
        case t: Block.Tool   => t.name
        case other           => other.toString
      },
      Vector("round one", "read")
    )
    assertEquals(s.retry, Some(RetryState(1, 6, 5000)))

  test("a first-round retry drops back to the plain waiting spinner"):
    val s = waiting.roundStarted
      .appendReply("doomed partial", now = 100)
      .retrying(attempt = 2, maxAttempts = 6, delayMs = 1000, now = 500)
    assertEquals(s.phase, Phase.Waiting)
    assertEquals(s.retry, Some(RetryState(2, 6, 1500)))

  test("failed commits the streamed blocks before the error line"):
    val s = waiting
      .appendReply("kept step", now = 100)
      .startTool("t1", "bash", now = 200)
      .failed("⚠ api down")
    assert(s.idle)
    s.history.init.last match
      case Entry.Assistant(blocks) => assertEquals(blocks.length, 2)
      case other                   => fail(s"expected the committed turn, got $other")
    assertEquals(s.history.last, Entry.Error("⚠ api down"))
    assertEquals(s.retry, None)

  test("a persisted ApiErrored replays as an error line"):
    val entries = ChatState.historyFrom(List(
      SessionEvent.UserSubmitted("hi"),
      SessionEvent.ApiErrored("Anthropic API error: 529 overloaded")
    ))
    assertEquals(entries.last, Entry.Error("⚠ Anthropic API error: 529 overloaded"))

  test("finishing a tool records its output text and error flag"):
    val s = waiting
      .startTool("t1", "eval_scala", now = 1000)
      .startToolRun("t1", now = 1000)
      .endToolRun("t1", isError = true, Map.empty, output = "boom", now = 1050)
    val t = s.streamingBlocks.head.asInstanceOf[Block.Tool]
    assertEquals(t.output, Some("boom"))
    assertEquals(t.isError, true)

  test("context usage is unknown only while the window is unknown"):
    assertEquals(base.contextPercentUsed, None)
    // usage but no window: still unknown
    assertEquals(base.withContextUsage(Some(Usage(100, 50))).contextPercentUsed, None)

  test("context usage reads 0% once the window is known but no tokens are spent"):
    assertEquals(base.copy(contextWindow = 200_000).contextPercentUsed, Some(0))

  test("context usage is the rounded share of the window from the latest round"):
    val s = base.copy(contextWindow = 200_000).withContextUsage(Some(Usage(inputTokens = 49_000, outputTokens = 1_000)))
    assertEquals(s.contextTokens, 50_000L)
    assertEquals(s.contextPercentUsed, Some(25))

  test("context usage tracks the most recent round, not the sum"):
    val s = base
      .copy(contextWindow = 200_000)
      .withContextUsage(Some(Usage(10_000, 0)))
      .withContextUsage(Some(Usage(80_000, 0)))
    assertEquals(s.contextPercentUsed, Some(40))

  test("context usage caps at 100% when the window is exceeded"):
    val s = base.copy(contextWindow = 100).withContextUsage(Some(Usage(250, 0)))
    assertEquals(s.contextPercentUsed, Some(100))

  test("notice records sticky notices (not transcript entries), dedups, and caps"):
    val s = base.notice("Workflow dashboard: http://localhost:7777")
    assertEquals(s.notices, Vector("Workflow dashboard: http://localhost:7777"))
    // a sticky notice never enters the scrolling transcript
    assertEquals(s.history, Vector.empty)
    // identical notices are not stacked
    assertEquals(s.notice("Workflow dashboard: http://localhost:7777").notices.size, 1)
    // distinct notices accumulate but are capped to the most recent few
    val many = (1 to 6).foldLeft(base)((st, i) => st.notice(s"n$i"))
    assertEquals(many.notices, Vector("n3", "n4", "n5", "n6"))

  test("a Done event with no usage leaves the context figure untouched"):
    val s = base.copy(contextWindow = 200_000, contextTokens = 50_000)
    assertEquals(s.withContextUsage(None).contextTokens, 50_000L)

  test("historyFrom shows a reasoning block as a thinking block on resume"):
    val events = List(
      SessionEvent.UserSubmitted("go"),
      responded(Message(Role.Assistant, List(
        Content.Reasoning(List(ReasoningBlock("reasoning.text", text = Some("pondering")))),
        Content.Text("the answer")
      )))
    )
    val blocks = ChatState.historyFrom(events).collect { case Entry.Assistant(bs) => bs }.flatten
    assert(blocks.exists { case _: Block.Thinking => true; case _ => false }, s"expected a Thinking block: $blocks")

  test("historyFrom attaches each tool result to its call by id"):
    val events = List(
      SessionEvent.UserSubmitted("go"),
      responded(
        Message(Role.Assistant, List(Content.ToolUse("e1", "eval_scala", """{"code":"1 + 1"}"""))),
        finishReason = FinishReason.ToolUse
      ),
      SessionEvent.ToolResultsReceived(List(Content.ToolResult("e1", "val res0: Int = 2\n")))
    )
    val tool = ChatState
      .historyFrom(events)
      .collect { case Entry.Assistant(bs) => bs }
      .flatten
      .collectFirst { case t: Block.Tool => t }
      .get
    assertEquals(tool.output, Some("val res0: Int = 2\n"))
    assertEquals(tool.isError, false)

  /* ---- Loops window ---- */

  /** A loop this session is driving — the window's selection has no opinion either way,
    * and a running loop is always one the session holds. */
  private def loopView(id: String): auk.agent.LoopView =
    auk.agent.LoopView(id, "running", "go faster", Vector.empty, None, None, None, false, true)

  test("showLoops opens the window at its top, loops or no loops"):
    assertEquals(base.showLoops.overlay, Overlay.Loops(0))
    assertEquals(base.copy(loops = Vector(loopView("a"))).showLoops.overlay, Overlay.Loops(0))

  test("moveLoopSelection clamps to the live loop count and is inert without loops"):
    val three = base.copy(loops = Vector(loopView("a"), loopView("b"), loopView("c")), overlay = Overlay.Loops(0))
    assertEquals(three.moveLoopSelection(1).overlay, Overlay.Loops(1))
    assertEquals(three.moveLoopSelection(-1).overlay, Overlay.Loops(0))
    assertEquals(three.copy(overlay = Overlay.Loops(2)).moveLoopSelection(1).overlay, Overlay.Loops(2))
    assertEquals(base.copy(overlay = Overlay.Loops(0)).moveLoopSelection(1).overlay, Overlay.Loops(0))

  test("applyLoops clamps an open window's selection when the set shrinks under it"):
    val open = base.copy(loops = Vector(loopView("a"), loopView("b"), loopView("c")), overlay = Overlay.Loops(2))
    assertEquals(open.applyLoops(Vector(loopView("a"))).overlay, Overlay.Loops(0))
    assertEquals(open.applyLoops(Vector.empty).overlay, Overlay.Loops(0))
    // A selection the new set still covers is left exactly where it was.
    assertEquals(open.applyLoops(Vector(loopView("a"), loopView("b"), loopView("z"))).overlay, Overlay.Loops(2))
    // Nothing else's overlay is touched by a loop snapshot.
    assertEquals(open.copy(overlay = Overlay.DebugInfo).applyLoops(Vector.empty).overlay, Overlay.DebugInfo)

  test("openSelectedLoop opens the selected loop's transcript, re-clamping a stale index"):
    val open = base.copy(loops = Vector(loopView("a"), loopView("b")), overlay = Overlay.Loops(1))
    assertEquals(open.openSelectedLoop.overlay, Overlay.LoopTranscript("b", 0))
    assertEquals(open.copy(loops = Vector(loopView("a"))).openSelectedLoop.overlay, Overlay.LoopTranscript("a", 0))
    // Nothing to open: the window stays put rather than opening an empty view.
    assertEquals(base.copy(overlay = Overlay.Loops(0)).openSelectedLoop.overlay, Overlay.Loops(0))

  test("closeLoopTranscript steps back to the window with that loop selected"):
    val loops = Vector(loopView("a"), loopView("b"), loopView("c"))
    val open = base.copy(loops = loops, overlay = Overlay.LoopTranscript("c", 4))
    assertEquals(open.closeLoopTranscript.overlay, Overlay.Loops(2))
    // A loop that has gone since lands the window at its top.
    assertEquals(open.copy(loops = Vector(loopView("a"))).closeLoopTranscript.overlay, Overlay.Loops(0))

  test("selectedLoopId names the row under the cursor, re-clamping a stale index"):
    val open = base.copy(loops = Vector(loopView("a"), loopView("b")), overlay = Overlay.Loops(1))
    assertEquals(open.selectedLoopId, Some("b"))
    assertEquals(open.copy(loops = Vector(loopView("a"))).selectedLoopId, Some("a"))
    // Nothing to point at, and nowhere to point from.
    assertEquals(base.copy(overlay = Overlay.Loops(0)).selectedLoopId, None)
    assertEquals(base.copy(loops = Vector(loopView("a"))).selectedLoopId, None)

  test("loopDashboardTarget is the dashboard URL with the loop as a #loop/ fragment"):
    val ready = base.copy(dashboardUrl = Some("http://localhost:7777"))
    assertEquals(ready.loopDashboardTarget("perf"), Some("http://localhost:7777/#loop/perf"))
    // The workflow target's trailing-slash discipline, one segment deeper.
    val slashed = base.copy(dashboardUrl = Some("http://localhost:7777/"))
    assertEquals(slashed.loopDashboardTarget("perf"), Some("http://localhost:7777/#loop/perf"))
    // Loop ids are [A-Za-z0-9_-]+, so the id goes in verbatim — nothing to escape.
    assertEquals(ready.loopDashboardTarget("A_b-9"), Some("http://localhost:7777/#loop/A_b-9"))
    // No server, no target: `o` waits on one rather than opening something wrong.
    assertEquals(base.loopDashboardTarget("perf"), None)

  test("a waiting loop-dashboard intent is replaced, then spent by the URL arriving"):
    val waiting = base.awaitingLoopDashboard("perf")
    assertEquals(waiting.pendingLoopDashboard, Some("perf"))
    // A second `o` moves the intent; it never accumulates.
    assertEquals(waiting.awaitingLoopDashboard("other").pendingLoopDashboard, Some("other"))
    // The URL landing spends it, so one keypress can only ever open one window.
    val ready = waiting.dashboardReady("http://localhost:7777")
    assertEquals(ready.dashboardUrl, Some("http://localhost:7777"))
    assertEquals(ready.pendingLoopDashboard, None)
    // And an intent nobody set stays unset — the fold invents no window.
    assertEquals(base.dashboardReady("http://localhost:7777").pendingLoopDashboard, None)
