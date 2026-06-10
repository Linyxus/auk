package auk.platform.js

class LineSplitterSuite extends munit.FunSuite:

  test("buffers until a newline arrives"):
    val s = new LineSplitter
    assertEquals(s.feed("ab"), Nil)
    assertEquals(s.pending, "ab")
    assertEquals(s.feed("c\nde"), List("abc"))
    assertEquals(s.pending, "de")
    assertEquals(s.feed("\n"), List("de"))
    assertEquals(s.pending, "")

  test("several lines in one chunk come out in order"):
    val s = new LineSplitter
    assertEquals(s.feed("one\ntwo\nthree\n"), List("one", "two", "three"))

  test("CRLF line endings are stripped to the bare line"):
    val s = new LineSplitter
    assertEquals(s.feed("a\r\nb\r\n"), List("a", "b"))

  test("empty lines are preserved as empty strings"):
    val s = new LineSplitter
    assertEquals(s.feed("a\n\nb\n"), List("a", "", "b"))
