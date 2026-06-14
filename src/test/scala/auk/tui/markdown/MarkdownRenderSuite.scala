package auk.tui.markdown

import auk.tui.app.{Element, Layout}
import auk.tui.markdown.render.MarkdownRender

class MarkdownRenderSuite extends munit.FunSuite:

  private def doc(s: String): MarkdownDocument = MarkdownDocument.parse(s)
  private def lines(e: Element, w: Int = 60): Vector[String] = Layout.lay(e, w).map(_.plain)
  private def sgr(e: Element, w: Int = 60): String = Layout.lay(e, w).map(_.render).mkString("\n")
  private def md(s: String): Element = MarkdownRender.render(doc(s))

  test("a paragraph is indented two spaces"):
    assertEquals(lines(md("hello world")), Vector("  hello world"))

  test("emphasis and strong strip to plain text but carry SGR attributes"):
    assertEquals(lines(md("*hi*")), Vector("  hi"))
    assert(sgr(md("*hi*")).contains(";3"), "italic SGR expected")
    assertEquals(lines(md("**hi**")), Vector("  hi"))
    assert(sgr(md("**hi**")).contains(";1"), "bold SGR expected")

  test("a level-1 heading is followed by a rule"):
    val ls = lines(md("# Title"))
    assertEquals(ls.head, "  Title")
    assert(ls(1).forall(_ == '─'), ls(1))

  test("bullet and ordered list markers"):
    assertEquals(lines(md("- a\n- b")), Vector("  • a", "  • b"))
    assertEquals(lines(md("1. a\n2. b")), Vector("  1. a", "  2. b"))

  test("task list checkboxes"):
    assertEquals(lines(md("- [x] done\n- [ ] todo")), Vector("  • ☑ done", "  • ☐ todo"))

  test("a wrapped list item hangs under its first character"):
    // Narrow width forces the item text to wrap; the continuation aligns past the marker.
    val ls = lines(md("- alpha beta gamma delta"), w = 12)
    assert(ls.length > 1, ls.toString)
    assert(ls.head.startsWith("  • "), ls.head)
    // Continuation lines hang under the first content character (past "• ").
    assert(ls.tail.forall(_.startsWith("    ")), ls.toString)

  test("a fenced code block draws a dim left rail"):
    assertEquals(lines(md("```\nval x = 1\n```")), Vector("  ▏ val x = 1"))

  test("a fenced code block shows its language tag"):
    assertEquals(lines(md("```scala\nx\n```")), Vector("  scala", "  ▏ x"))

  test("a blockquote draws a left bar"):
    assertEquals(lines(md("> quoted")), Vector("  ▌ quoted"))

  test("strikethrough overlays a combining stroke"):
    val plain = lines(md("~~gone~~")).head
    assertEquals(plain.replace("\u0336", ""), "  gone")
    assert(plain.contains("\u0336"))

  test("a thematic break is a horizontal rule"):
    val ls = lines(md("---"))
    assertEquals(ls.length, 1)
    assert(ls.head.forall(_ == '─'))

  test("a table renders a header, a rule, and body rows"):
    val ls = lines(md("| a | b |\n|---|---|\n| 1 | 2 |"))
    assertEquals(ls.length, 3)
    assert(ls(0).contains("a") && ls(0).contains("b"))
    assert(ls(1).contains("─"))
    assert(ls(2).contains("1") && ls(2).contains("2"))

  test("a table that fits keeps natural column widths (no wrapping)"):
    // Wide terminal: one physical line per logical row (header, rule, one body).
    assertEquals(lines(md("| a | b |\n|---|---|\n| 1 | 2 |"), 80).length, 3)

  test("an overlong cell wraps within the screen width"):
    val src = "| name | description |\n|---|---|\n| x | a very long description that has to wrap across several lines |"
    val ls = lines(md(src), 30)
    assert(ls.forall(l => auk.tui.render.Width.stringWidth(l) <= 30), ls.mkString("\n"))
    // The body row spans multiple physical lines once its cell wraps.
    assert(ls.length > 3, ls.mkString("\n"))

  test("column alignment pads cells"):
    // col0 right-aligned, header "long" sets the column width to 4.
    val ls = lines(md("| long | b |\n|---:|:--|\n| 1 | 2 |"), 60)
    assert(ls.exists(_.contains("   1")), ls.mkString("|"))

  test("paragraphs are separated by a blank line"):
    assertEquals(lines(md("one\n\ntwo")), Vector("  one", "", "  two"))

  test("a streaming answer glows its tail and rides the cursor"):
    val g = MarkdownRender.answerBlock(doc("hello"), glow = Some((3, 0)))
    val ls = lines(g)
    assert(ls.last.contains("_"), ls.toString)
    assert(sgr(g).contains("38;2"), "expected a truecolor glow sequence")

  test("with no glow a committed answer is plain (no cursor)"):
    val c = MarkdownRender.answerBlock(doc("hello"), glow = None)
    assert(!lines(c).exists(_.contains("_")))

  test("only the trailing block carries the cursor while streaming"):
    val g = MarkdownRender.answerBlock(doc("first para\n\nsecond para"), glow = Some((4, 0)))
    val ls = lines(g)
    assert(!ls.head.contains("_"), ls.head)
    assert(ls.last.contains("_"), ls.last)
