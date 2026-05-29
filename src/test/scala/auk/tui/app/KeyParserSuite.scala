package auk.tui.app

import java.nio.charset.StandardCharsets.UTF_8

class KeyParserSuite extends munit.FunSuite:

  /** Feed a byte sequence and collect every key produced. */
  private def parse(bytes: Int*): List[Key] =
    val p = KeyParser()
    bytes.toList.flatMap(p.feed)

  private def parseString(s: String): List[Key] =
    val p = KeyParser()
    s.getBytes(UTF_8).toList.flatMap(b => p.feed(b & 0xff))

  test("printable ASCII becomes Char") {
    assertEquals(parse('a'.toInt), List(Key.Char('a')))
    assertEquals(parse('Q'.toInt, '!'.toInt), List(Key.Char('Q'), Key.Char('!')))
  }

  test("control bytes map to named keys") {
    assertEquals(parse(0x0d), List(Key.Enter))
    assertEquals(parse(0x0a), List(Key.Newline))
    assertEquals(parse(0x7f), List(Key.Backspace))
    assertEquals(parse(0x09), List(Key.Tab))
  }

  test("Ctrl chords") {
    assertEquals(parse(0x03), List(Key.Ctrl('C'))) // Ctrl-C
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

  test("CSI-u Enter and Shift+Enter") {
    assertEquals(parse(0x1b, '['.toInt, '1'.toInt, '3'.toInt, 'u'.toInt), List(Key.Enter))
    assertEquals(
      parse(0x1b, '['.toInt, '1'.toInt, '3'.toInt, ';'.toInt, '2'.toInt, 'u'.toInt),
      List(Key.Newline)
    )
  }

  test("CSI-u associated text inserts normal characters") {
    assertEquals(
      parse(0x1b, '['.toInt, '9'.toInt, '7'.toInt, ';'.toInt, '1'.toInt, ';'.toInt, '9'.toInt, '7'.toInt, 'u'.toInt),
      List(Key.Char('a'))
    )
    assertEquals(
      parse(0x1b, '['.toInt, '9'.toInt, '7'.toInt, ';'.toInt, '2'.toInt, ';'.toInt, '6'.toInt, '5'.toInt, 'u'.toInt),
      List(Key.Char('A'))
    )
  }

  test("CSI-u Ctrl letters remain control chords") {
    assertEquals(
      parse(0x1b, '['.toInt, '1'.toInt, '1'.toInt, '3'.toInt, ';'.toInt, '5'.toInt, 'u'.toInt),
      List(Key.Ctrl('Q'))
    )
  }

  test("xterm modifyOtherKeys Shift+Enter becomes newline") {
    assertEquals(
      parse(
        0x1b,
        '['.toInt,
        '2'.toInt,
        '7'.toInt,
        ';'.toInt,
        '2'.toInt,
        ';'.toInt,
        '1'.toInt,
        '3'.toInt,
        '~'.toInt
      ),
      List(Key.Newline)
    )
  }

  test("CSI-u functional private-use keys are not inserted as text") {
    // Kitty all-key mode reports modifier-key presses as functional key codes.
    // U+E061 is Shift; it must not become a literal private-use character.
    assertEquals(
      parse(0x1b, '['.toInt, '5'.toInt, '7'.toInt, '4'.toInt, '4'.toInt, '1'.toInt, ';'.toInt, '2'.toInt, 'u'.toInt),
      List(Key.Unknown)
    )
  }

  test("CSI-u Shift modifier state can make following Enter a newline") {
    assertEquals(parseString("\u001b[57441;2:1u\u001b[13;1u"), List(Key.Unknown, Key.Newline))
  }

  test("CSI-u Shift modifier state remains active across repeated Enter presses") {
    assertEquals(
      parseString("\u001b[57441;2:1u\u001b[13;1u\u001b[13;1u"),
      List(Key.Unknown, Key.Newline, Key.Newline)
    )
  }

  test("CSI-u Shift modifier state remains active across repeated raw Enter bytes") {
    assertEquals(parseString("\u001b[57441;2:1u\r\r"), List(Key.Unknown, Key.Newline, Key.Newline))
  }

  test("CSI-u Shift release clears modifier state") {
    assertEquals(parseString("\u001b[57441;2:1u\u001b[57441;1:3u\r"), List(Key.Unknown, Key.Unknown, Key.Enter))
  }

  test("CSI-u key release with associated text is ignored") {
    assertEquals(parseString("\u001b[97;1:3;97u"), Nil)
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
