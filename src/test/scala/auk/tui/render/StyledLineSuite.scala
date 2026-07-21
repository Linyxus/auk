package auk.tui.render

class StyledLineSuite extends munit.FunSuite:

  // The selection highlight used in the fullscreen chat: black on soft blue.
  private val HlBg: Color = Color.True(135, 206, 235)
  private val Hl: Style = Style(fg = Color.Black, bg = HlBg)
  private def mark(s: Style): Style = Hl

  /** The concatenated text of the spans carrying the highlight background. */
  private def highlighted(line: StyledLine): String =
    line.spans.filter(_.style.bgColor == HlBg).map(_.text).mkString

  test("restyleCells highlights a mid-line cell range and leaves the rest untouched"):
    val out = StyledLine.restyleCells(StyledLine.text("hello world"), 6, 11, mark)
    // The plain text and total width are unchanged; only styling moved.
    assertEquals(out.plain, "hello world")
    assertEquals(out.width, 11)
    assertEquals(highlighted(out), "world")

  test("restyleCells over a whole line highlights every cell"):
    val line = StyledLine.text("abc")
    val out = StyledLine.restyleCells(line, 0, line.width, mark)
    assertEquals(highlighted(out), "abc")
    assertEquals(out.plain, "abc")

  test("restyleCells includes a wide CJK glyph whole when a boundary straddles it"):
    // "A你B你C": A@0, 你@1-2, B@3, 你@4-5, C@6. The range [2,5) starts inside the
    // first 你 and ends inside the second, so both wide glyphs are restyled whole.
    val out = StyledLine.restyleCells(StyledLine.text("A你B你C"), 2, 5, mark)
    assertEquals(out.plain, "A你B你C")
    assertEquals(highlighted(out), "你B你")

  test("restyleCells is a no-op for an empty or inverted range"):
    val line = StyledLine.text("abc")
    assertEquals(StyledLine.restyleCells(line, 3, 3, mark), line)
    assertEquals(StyledLine.restyleCells(line, 2, 1, mark), line)
