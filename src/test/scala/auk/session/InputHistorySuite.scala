package auk.session

import auk.TestFs

class InputHistorySuite extends munit.FunSuite:

  private def tempPath(): String = TestFs.join(TestFs.tempDir("auk-history"), "history.jsonl")

  test("records prompts and reads them back oldest-first"):
    val h = InputHistory(tempPath())
    assertEquals(h.record("first", "s1", 1000L), Right(()))
    assertEquals(h.record("second", "s2", 2000L), Right(()))
    assertEquals(
      h.entries,
      Right(List(InputEntry("first", "s1", 1000L), InputEntry("second", "s2", 2000L)))
    )

  test("a missing file is an empty history"):
    assertEquals(InputHistory(tempPath()).entries, Right(Nil))

  test("text with newlines, quotes and tabs round-trips exactly"):
    val h = InputHistory(tempPath())
    val text = "line one\nline \"two\"\ttabbed"
    h.record(text, "s1", 5L)
    assertEquals(h.entries, Right(List(InputEntry(text, "s1", 5L))))

  test("each recorded prompt is one JSONL line"):
    val p = tempPath()
    val h = InputHistory(p)
    h.record("a", "s", 1L)
    h.record("b", "s", 2L)
    assertEquals(JsonlLog.readLines(p).map(_.length), Right(2))

  test("history spans sessions (the session id is stored per entry)"):
    val h = InputHistory(tempPath())
    h.record("in session one", "s1", 1L)
    h.record("in session two", "s2", 2L)
    assertEquals(h.entries.map(_.map(_.sessionId)), Right(List("s1", "s2")))
