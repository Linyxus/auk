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

  test("Width: matches the terminal for every emoji presentation class") {
    // Default-emoji-presentation characters are wide — BMP ones included.
    // These were the screen-smear culprits: the model said 1, Ghostty said 2,
    // and every relative cursor move after one of them landed off by a column.
    assertEquals(Width.displayWidth(0x2705), 2) // ✅
    assertEquals(Width.displayWidth(0x274c), 2) // ❌
    assertEquals(Width.displayWidth(0x26a1), 2) // ⚡
    assertEquals(Width.displayWidth(0x2b50), 2) // ⭐
    assertEquals(Width.displayWidth(0x2728), 2) // ✨
    assertEquals(Width.displayWidth(0x23f0), 2) // ⏰
    assertEquals(Width.displayWidth(0x231a), 2) // ⌚
    assertEquals(Width.displayWidth(0x1f680), 2) // 🚀 transport block
    assertEquals(Width.displayWidth(0x1f6d1), 2) // 🛑
    assertEquals(Width.displayWidth(0x1f7e2), 2) // 🟢 geometric ext block
    // Text-presentation pictographs are narrow (EAW=N); the old blanket
    // 1F300–1F64F range over-counted these.
    assertEquals(Width.displayWidth(0x1f321), 1) // 🌡
    assertEquals(Width.displayWidth(0x1f5a5), 1) // 🖥
    assertEquals(Width.displayWidth(0x1f441), 1) // 👁
    // Cluster-forming codepoints are zero-width (and dropped at emission).
    assertEquals(Width.displayWidth(0xfe0f), 0) // VS16
    assertEquals(Width.displayWidth(0x200d), 0) // ZWJ
    assertEquals(Width.displayWidth(0x1f3fb), 0) // skin-tone modifier
    assertEquals(Width.displayWidth(0x3099), 0) // combining kana voicing mark
    // Ambiguous-width renders narrow; soft hyphen is a visible column.
    assertEquals(Width.displayWidth(0x3248), 1) // ㉈ circled ten
    assertEquals(Width.displayWidth(0x00ad), 1)
    assertEquals(Width.displayWidth(0x1f1e6), 1) // regional indicator
  }

  test("Width: range tables are sorted and disjoint (binary-search invariant)") {
    def checkShape(name: String, t: Array[Int]): Unit =
      assert(t.length % 2 == 0, s"$name has a dangling bound")
      var i = 0
      while i < t.length do
        assert(t(i) <= t(i + 1), s"$name range $i inverted")
        if i + 2 < t.length then assert(t(i + 1) < t(i + 2), s"$name ranges $i/${i + 2} overlap or unsorted")
        i += 2
    checkShape("ZeroWidthRanges", Width.ZeroWidthRanges)
    checkShape("WideRanges", Width.WideRanges)
    // A codepoint must never be in both tables: zero-width wins by check
    // order, but an overlap would mean the generator rule broke.
    var i = 0
    while i < Width.WideRanges.length do
      var cp = Width.WideRanges(i)
      while cp <= Width.WideRanges(i + 1) do
        assert(Width.displayWidth(cp) == 2, s"wide U+${cp.toHexString} shadowed by zero table")
        cp += 1
      i += 2
  }

  test("Surface packs a BMP wide emoji as lead + spacer") {
    val pool = StylePool()
    val s = Surface.build(4, Vector(StyledLine.text("✅ab")), pool)
    assertEquals(Cell.codePoint(s.at(0, 0)), 0x2705)
    assertEquals(Cell.widthClass(s.at(0, 0)), Cell.Wide)
    assertEquals(Cell.widthClass(s.at(0, 1)), Cell.Spacer)
    assertEquals(Cell.codePoint(s.at(0, 2)), 'a'.toInt)
    assertEquals(Cell.codePoint(s.at(0, 3)), 'b'.toInt)
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
    // The transition string depends only on the destination style.
    assertEquals(pool.transition(a, b), pool.transition(0, b))
    assertEquals(pool.setSequence(0), Ansi.Reset)
    assertEquals(pool.setSequence(b), Style.fg(Color.Cyan).setSequence)
  }

  test("StyledLine.render styles each span and resets at the end") {
    val line = StyledLine(Vector(Span("You", Style.fg(Color.Cyan) ++ Style.Bold), Span(" hi", Style.Default)))
    assertEquals(line.render, s"$ESC[0;1;36mYou$ESC[0m hi$ESC[0m")
    assertEquals(line.plain, "You hi")
    assertEquals(line.width, 6)
  }
