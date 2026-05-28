package auk.tui.app

class KeyParserSuite extends munit.FunSuite:

  /** Feed a byte sequence and collect every key produced. */
  private def parse(bytes: Int*): List[Key] =
    val p = KeyParser()
    bytes.toList.flatMap(p.feed)

  test("printable ASCII becomes Char") {
    assertEquals(parse('a'.toInt), List(Key.Char('a')))
    assertEquals(parse('Q'.toInt, '!'.toInt), List(Key.Char('Q'), Key.Char('!')))
  }

  test("control bytes map to named keys") {
    assertEquals(parse(0x0d), List(Key.Enter))
    assertEquals(parse(0x0a), List(Key.Enter))
    assertEquals(parse(0x7f), List(Key.Backspace))
    assertEquals(parse(0x09), List(Key.Tab))
  }

  test("Ctrl chords") {
    assertEquals(parse(0x11), List(Key.Ctrl('Q'))) // Ctrl-Q
    assertEquals(parse(0x17), List(Key.Ctrl('W'))) // Ctrl-W
    assertEquals(parse(0x01), List(Key.Ctrl('A'))) // Ctrl-A
  }

  test("CSI arrows, Home/End, Delete") {
    assertEquals(parse(0x1b, '['.toInt, 'A'.toInt), List(Key.Up))
    assertEquals(parse(0x1b, '['.toInt, 'B'.toInt), List(Key.Down))
    assertEquals(parse(0x1b, '['.toInt, 'C'.toInt), List(Key.Right))
    assertEquals(parse(0x1b, '['.toInt, 'D'.toInt), List(Key.Left))
    assertEquals(parse(0x1b, '['.toInt, 'H'.toInt), List(Key.Home))
    assertEquals(parse(0x1b, '['.toInt, '3'.toInt, '~'.toInt), List(Key.Delete))
    assertEquals(parse(0x1b, '['.toInt, '1'.toInt, '~'.toInt), List(Key.Home))
  }

  test("modified arrows strip the modifier") {
    // ESC [ 1 ; 5 C  (Ctrl+Right) -> Right
    assertEquals(parse(0x1b, '['.toInt, '1'.toInt, ';'.toInt, '5'.toInt, 'C'.toInt), List(Key.Right))
  }

  test("SS3 arrows") {
    assertEquals(parse(0x1b, 'O'.toInt, 'A'.toInt), List(Key.Up))
    assertEquals(parse(0x1b, 'O'.toInt, 'F'.toInt), List(Key.End))
  }

  test("lone ESC then a key") {
    // ESC buffered, then 'a' flushes Esc and reprocesses 'a'
    val p = KeyParser()
    assertEquals(p.feed(0x1b), Nil)
    assertEquals(p.feed('a'.toInt), List(Key.Esc, Key.Char('a')))
  }

  test("multi-byte UTF-8 decodes to a Char") {
    // "é" = U+00E9 = 0xC3 0xA9
    assertEquals(parse(0xc3, 0xa9), List(Key.Char('é')))
    // "›" = U+203A = 0xE2 0x80 0xBA
    assertEquals(parse(0xe2, 0x80, 0xba), List(Key.Char('›')))
  }
