package auk.library

/** The pure logic of [[CommandResult]] — `ok`, `output`, and the `toString`
  * footer — exercised without running any process. */
class CommandResultSuite extends munit.FunSuite:

  test("ok is true only on a clean exit that did not time out"):
    assert(CommandResult("", "", 0, false).ok)
    assert(!CommandResult("", "", 1, false).ok)
    assert(!CommandResult("", "", 0, true).ok)  // exit 0 but killed by timeout
    assert(!CommandResult("", "", 137, false).ok)

  test("output is stdout when stderr is empty"):
    assertEquals(CommandResult("hello", "", 0, false).output, "hello")

  test("output separates stdout and stderr with a newline when both are present"):
    assertEquals(CommandResult("out", "err", 1, false).output, "out\nerr")

  test("output does not double a newline that stdout already ends with"):
    assertEquals(CommandResult("out\n", "err", 1, false).output, "out\nerr")

  test("output of two multi-line streams joins them on a fresh line"):
    assertEquals(CommandResult("l1\nl2", "e1\ne2", 1, false).output, "l1\nl2\ne1\ne2")

  test("output is just stderr when stdout is empty"):
    assertEquals(CommandResult("", "boom", 1, false).output, "boom")

  test("output is empty when both streams are empty"):
    assertEquals(CommandResult("", "", 0, false).output, "")

  test("toString appends an exit footer to the output"):
    assertEquals(CommandResult("hi", "", 0, false).toString, "hi\n[exit 0]")

  test("toString of both streams shows the separated output then the footer"):
    assertEquals(CommandResult("out", "err", 1, false).toString, "out\nerr\n[exit 1]")

  test("toString reports a non-zero exit code"):
    assertEquals(CommandResult("", "nope", 2, false).toString, "nope\n[exit 2]")

  test("toString is just the footer when there is no output"):
    assertEquals(CommandResult("", "", 0, false).toString, "[exit 0]")

  test("toString shows a timed-out footer regardless of exit code"):
    assertEquals(CommandResult("partial", "", 124, true).toString, "partial\n[timed out]")
    assertEquals(CommandResult("", "", 124, true).toString, "[timed out]")

  test("it is a value type — equality and copy behave structurally"):
    assertEquals(CommandResult("a", "b", 0, false), CommandResult("a", "b", 0, false))
    assertEquals(
      CommandResult("a", "b", 0, false).copy(exitCode = 1),
      CommandResult("a", "b", 1, false)
    )
