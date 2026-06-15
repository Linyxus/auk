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
