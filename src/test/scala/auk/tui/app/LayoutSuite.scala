package auk.tui.app

import auk.tui.render.{Ansi, Color, Span, Style, StyledLine}

class LayoutSuite extends munit.FunSuite:

  test("a single styled span renders as setSequence + text + reset") {
    // ChatApp builds the prompt cursor cell directly as this string instead of
    // via Text(_).style(_).render; the two must stay byte-identical.
    assertEquals(
      Style.Underline.setSequence + "x" + Ansi.Reset,
      Text("x").style(Style.Underline).render
    )
  }

  test("Text becomes one styled line; newlines split into rows") {
    assertEquals(Layout.lay(Text("hi"), 10), Vector(StyledLine(Vector(Span("hi", Style.Default)))))
    val two = Layout.lay(Text("a\nb"), 10)
    assertEquals(two.length, 2)
    assertEquals(two(0).plain, "a")
    assertEquals(two(1).plain, "b")
  }

  test("layout stacks children; br is a blank line; Empty contributes nothing") {
    val ls = Layout.lay(layout(Text("a"), br, Empty, Text("b")), 10)
    assertEquals(ls.map(_.plain), Vector("a", "", "b"))
  }

  test("hr expands to the layout width") {
    assertEquals(Layout.lay(hr('-'), 5).head.plain, "-----")
  }

  test("labelledHr centers its label in a full-width rule") {
    assertEquals(Layout.lay(labelledHr("hey"), 15).head.plain, "───── hey ─────")
    // An odd fill tips the extra dash to the right.
    assertEquals(Layout.lay(labelledHr("hey"), 14).head.plain, "──── hey ─────")
    // Reflows with the width — nothing is baked in (the resize-repaint invariant).
    assertEquals(Layout.lay(labelledHr("hey"), 9).head.plain, "── hey ──")
  }

  test("labelledHr degrades to the bare label when the line is too narrow") {
    assertEquals(Layout.lay(labelledHr("hey"), 6).head.plain, "hey")
    assertEquals(Layout.lay(labelledHr("hey"), 2).head.plain, "he")
  }

  test("Color application and .style attach the right Style") {
    val cyan = Layout.lay(Color.Cyan("x"), 10).head
    assertEquals(cyan.spans, Vector(Span("x", Style.fg(Color.Cyan))))

    val boldCyan = Layout.lay(Color.Cyan("x").style(Style.Bold), 10).head
    assertEquals(boldCyan.spans, Vector(Span("x", Style.fg(Color.Cyan) ++ Style.Bold)))
  }

  test("embedded ANSI in a Text string is re-tokenized into styled spans") {
    // The app builds strings from sub-elements' .render; layout must recover spans.
    val s = Color.Cyan("›").render + " hello"
    val line = Layout.lay(Text(s), 40).head
    assertEquals(line.spans, Vector(Span("›", Style.fg(Color.Cyan)), Span(" hello", Style.Default)))
  }

  test("reverse-video cursor cell tokenizes to a Reverse span") {
    val cell = Text("X").style(Style.Reverse).render // ESC[0;7mXESC[0m
    val line = Layout.lay(Text(s"> $cell"), 40).head
    assertEquals(line.spans, Vector(Span("> ", Style.Default), Span("X", Style.Reverse)))
  }

  test("spinner renders a glyph followed by the label") {
    val line = Layout.lay(spinner("waiting", 0), 20).head
    assert(line.plain.endsWith(" waiting"), line.plain)
    assertEquals(line.plain.length, "waiting".length + 2) // glyph + space + label
  }

  test("wrapped text uses first and continuation prefixes") {
    val lines = Layout.lay(wrapText("> ", "  ", "abcdef"), 5)
    assertEquals(lines.map(_.plain), Vector("> abc", "  def"))
    assert(lines.forall(_.width <= 5), lines.map(_.plain).mkString("|"))
  }

  test("wrapped text preserves embedded ANSI styles across rows") {
    val cursor = Text("c").style(Style.Reverse).render
    val lines = Layout.lay(wrapText("> ", "  ", s"ab${cursor}def"), 5)

    assertEquals(lines.map(_.plain), Vector("> abc", "  def"))
    assertEquals(lines.head.spans, Vector(Span("> ", Style.Default), Span("ab", Style.Default), Span("c", Style.Reverse)))
  }

  test("wrapped text treats explicit newlines as row breaks") {
    val lines = Layout.lay(wrapText("> ", "  ", "ab\ncd"), 10)
    assertEquals(lines.map(_.plain), Vector("> ab", "  cd"))
  }

  // ---- clipRows (the prompt-box height cap) ----

  /** `n` one-row lines `l1..ln`, with row `cursor` (0-based) underline-styled —
    * the shape [[ClipFocus.Cursor]] windows around. */
  private def tallStack(n: Int, cursor: Int = -1): Element =
    layout((0 until n).map { i =>
      if i == cursor then Text(s"l${i + 1}").style(Style.Underline) else Text(s"l${i + 1}")
    }*)

  test("clipRows passes short content through untouched") {
    val lines = Layout.lay(clipRows(tallStack(12), 12, ClipFocus.Head), 20)
    assertEquals(lines.map(_.plain), (1 to 12).map(i => s"l$i").toVector)
  }

  test("clipRows Head keeps the first rows and a counted marker") {
    val lines = Layout.lay(clipRows(tallStack(30), 12, ClipFocus.Head), 30)
    assertEquals(lines.length, 12)
    assertEquals(lines.take(11).map(_.plain), (1 to 11).map(i => s"l$i").toVector)
    assertEquals(lines.last.plain, "… 19 more lines")
  }

  test("clipRows Head appends the hint to the marker") {
    val lines = Layout.lay(clipRows(tallStack(30), 12, ClipFocus.Head, hint = "see transcript"), 40)
    assertEquals(lines.last.plain, "… 19 more lines (see transcript)")
  }

  test("clipRows Cursor centers the window on the cursor row") {
    // Cursor on l15 (row 14): window rows 10..19, ten rows hidden on each side,
    // the cursor row sitting mid-window.
    val lines = Layout.lay(clipRows(tallStack(30, cursor = 14), 12, ClipFocus.Cursor), 30)
    assertEquals(lines.length, 12)
    assertEquals(lines.head.plain, "… 10 more lines")
    assertEquals(lines.last.plain, "… 10 more lines")
    assertEquals(lines.slice(1, 11).map(_.plain), (11 to 20).map(i => s"l$i").toVector)
  }

  test("clipRows Cursor pins to the top edge with a single bottom marker") {
    val lines = Layout.lay(clipRows(tallStack(30, cursor = 1), 12, ClipFocus.Cursor), 30)
    assertEquals(lines.take(11).map(_.plain), (1 to 11).map(i => s"l$i").toVector)
    assertEquals(lines.last.plain, "… 19 more lines")
  }

  test("clipRows Cursor pins to the bottom edge with a single top marker") {
    val lines = Layout.lay(clipRows(tallStack(30, cursor = 29), 12, ClipFocus.Cursor), 30)
    assertEquals(lines.head.plain, "… 19 more lines")
    assertEquals(lines.tail.map(_.plain), (20 to 30).map(i => s"l$i").toVector)
  }

  test("clipRows Cursor falls back to the last row when no cursor cell is laid") {
    val lines = Layout.lay(clipRows(tallStack(30), 12, ClipFocus.Cursor), 30)
    assertEquals(lines.head.plain, "… 19 more lines")
    assertEquals(lines.last.plain, "l30")
  }

  test("clipRows markers use the singular for one hidden line") {
    // Cursor at l6: the centered window starts at row 1, hiding exactly l1.
    val lines = Layout.lay(clipRows(tallStack(30, cursor = 5), 12, ClipFocus.Cursor), 30)
    assertEquals(lines.head.plain, "… 1 more line")
  }

  test("clipRows markers are dim rows truncated to the width") {
    val lines = Layout.lay(clipRows(tallStack(30), 12, ClipFocus.Head), 8)
    assertEquals(lines.last.spans, Vector(Span("… 19 mor", Style.Dim)))
  }
