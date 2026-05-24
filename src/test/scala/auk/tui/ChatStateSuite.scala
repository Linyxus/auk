package auk.tui

class ChatStateSuite extends munit.FunSuite:

  private val base = ChatState.initial

  test("recall is a no-op with no history"):
    assertEquals(base.recallPrev, base)
    assertEquals(base.recallNext, base)

  test("submitted appends to the transcript and the input history"):
    val s = base.submitted("one")
    assertEquals(s.history, Vector(Message(Role.You, "one")))
    assertEquals(s.inputHistory, Vector("one"))
    assertEquals(s.input, "")
    assertEquals(s.histNav, 1) // parked at the draft slot

  test("submitted collapses an immediate repeat"):
    val s = base.submitted("a").submitted("a").submitted("b").submitted("a")
    assertEquals(s.inputHistory, Vector("a", "b", "a"))

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

  // ---- streaming a reply: thinking -> answer -> done ----

  private val waiting = base.copy(phase = Phase.Waiting)

  test("thinking then answer collapses reasoning into a duration"):
    val s = waiting.appendThinking("rea", now = 1000).appendThinking("soning", now = 1100)
    assertEquals(s.phase, Phase.Streaming("reasoning", "", 1000, None))
    val a = s.appendReply("hi", now = 3500)
    assertEquals(a.phase, Phase.Streaming("reasoning", "hi", 1000, Some(2500)))

  test("an answer with no thinking carries no duration"):
    val s = waiting.appendReply("hi", now = 500).appendReply(" there", now = 540)
    assertEquals(s.phase, Phase.Streaming("", "hi there", 0, None))

  test("completeReply commits the answer with its thinking duration, then idles"):
    val s = waiting
      .appendThinking("mull", now = 1000)
      .appendReply("answer", now = 2000)
      .completeReply(fallback = "ignored", now = 9999)
    assertEquals(s.history.last, Message(Role.Auk, "answer", Some(1000L)))
    assert(s.idle)

  test("a thinking-only turn falls back to the Done text and times to completion"):
    val s = waiting
      .appendThinking("just thinking", now = 1000)
      .completeReply(fallback = "final answer", now = 2500)
    assertEquals(s.history.last, Message(Role.Auk, "final answer", Some(1500L)))

  test("failed appends an error line and idles"):
    val s = waiting.failed("⚠ boom")
    assertEquals(s.history.last, Message(Role.Auk, "⚠ boom", None))
    assert(s.idle)
