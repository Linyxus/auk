package auk.tui.render

class PrimitivesSuite extends munit.FunSuite:

  private val ESC = ""

  test("Cell packs and unpacks codepoint, styleId, widthClass") {
    val c = Cell.pack(0x1f600, 5, Cell.Wide) // emoji, style 5, wide
    assertEquals(Cell.codePoint(c), 0x1f600)
    assertEquals(Cell.styleId(c), 5)
    assertEquals(Cell.widthClass(c), Cell.Wide)

    // styleId uses the full 32 bits independent of the codepoint field
    val big = Cell.pack('x'.toInt, 1_000_000, Cell.ZeroWidth)
    assertEquals(Cell.codePoint(big), 'x'.toInt)
    assertEquals(Cell.styleId(big), 1_000_000)
    assertEquals(Cell.widthClass(big), Cell.ZeroWidth)

    assertEquals(Cell.codePoint(Cell.Blank), ' '.toInt)
    assertEquals(Cell.styleId(Cell.Blank), 0)
  }

  test("Width: ASCII=1, CJK/emoji=2, combining/zero-width=0") {
    assertEquals(Width.displayWidth('a'.toInt), 1)
    assertEquals(Width.displayWidth(' '.toInt), 1)
    assertEquals(Width.displayWidth(0x4e00), 2) // 一 CJK
    assertEquals(Width.displayWidth(0xac00), 2) // 가 Hangul syllable
    assertEquals(Width.displayWidth(0x1f600), 2) // 😀 emoji
    assertEquals(Width.displayWidth(0x0301), 0) // combining acute accent
    assertEquals(Width.displayWidth(0x200b), 0) // zero-width space
    assertEquals(Width.displayWidth(0x1b), 0) // control
  }

  test("Width: stringWidth sums codepoints incl. astral planes") {
    assertEquals(Width.stringWidth("hello"), 5)
    assertEquals(Width.stringWidth("一二三"), 6)
    assertEquals(Width.stringWidth("a😀b"), 4) // 1 + 2 + 1
    assertEquals(Width.stringWidth("é"), 1) // e + combining accent
    assertEquals(Width.stringWidth("›"), 1) // U+203A used in the prompt
  }

  test("Style.setSequence is a self-contained reset+set") {
    assertEquals(Style.Default.setSequence, s"$ESC[0m")
    assertEquals(Style.Bold.setSequence, s"$ESC[0;1m")
    assertEquals(Style.Reverse.setSequence, s"$ESC[0;7m")
    assertEquals(Style.fg(Color.Cyan).setSequence, s"$ESC[0;36m")
    assertEquals(Style.fg(Color.Red).setSequence, s"$ESC[0;31m")
    assertEquals(Style(fg = Color.Cyan, attrs = Attr.Bold).setSequence, s"$ESC[0;1;36m")
    assertEquals(Style.fg(Color.True(135, 206, 235)).setSequence, s"$ESC[0;38;2;135;206;235m")
  }

  test("Style.++ merges attrs and overrides colour") {
    val s = Style.fg(Color.Cyan) ++ Style.Bold
    assert(s.hasAttr(Attr.Bold))
    assertEquals(s.fgColor, Color.Cyan)
    // a non-default fg on the right wins
    val s2 = Style.fg(Color.Cyan) ++ Style.fg(Color.Red)
    assertEquals(s2.fgColor, Color.Red)
  }

  test("StylePool: interns ids and caches transitions") {
    val pool = StylePool()
    assertEquals(pool.intern(Style.Default), 0)
    val a = pool.intern(Style.Bold)
    val b = pool.intern(Style.fg(Color.Cyan))
    assertEquals(pool.intern(Style.Bold), a) // stable
    assertNotEquals(a, b)
    assertEquals(pool.transition(a, a), "")
    assertEquals(pool.transition(0, b), Style.fg(Color.Cyan).setSequence)
  }

  test("StyledLine.render styles each span and resets at the end") {
    val line = StyledLine(Vector(Span("You", Style.fg(Color.Cyan) ++ Style.Bold), Span(" hi", Style.Default)))
    assertEquals(line.render, s"$ESC[0;1;36mYou$ESC[0m hi$ESC[0m")
    assertEquals(line.plain, "You hi")
    assertEquals(line.width, 6)
  }
