package auk.tui

import auk.tui.app.{Layout, Text}
import auk.tui.render.{Color, Style}

class GlowSuite extends munit.FunSuite:

  /** The visible text after the layout strips the embedded SGR — the same path
    * the renderer takes, so this also proves no escape leaks into the glyphs. */
  private def plain(decorated: String): String =
    Layout.lay(Text(decorated), 200).map(_.plain).mkString("\n")

  test("a fully-cooled trail is byte-for-byte plain (no escapes)"):
    val s = Glow.trail("settled text", coolLen = "settled text".length, Glow.AnswerHot, Glow.AnswerCool)
    assertEquals(s, "settled text")

  test("a glowing trail strips back to exactly the original text"):
    val text = "the quick brown fox"
    val s = Glow.trail(text, coolLen = 4, Glow.AnswerHot, Glow.AnswerCool)
    assert(s.contains(''), "expected SGR escapes in the glow zone")
    assertEquals(plain(s), text)

  test("the newest code point is rendered at the hot colour"):
    val s = Glow.trail("abcdef", coolLen = 2, Glow.AnswerHot, Glow.AnswerCool)
    // t = 1 at the final unit, so its colour is the hot endpoint exactly.
    val hot = Style.fg(Color.True(Glow.AnswerHot._1, Glow.AnswerHot._2, Glow.AnswerHot._3)).setSequence
    assert(s.contains(hot), s"missing hot endpoint $hot in $s")

  test("the glow is code-point safe across surrogate pairs"):
    val text = "go 🚀🎉 now" // 🚀 and 🎉 are non-BMP (surrogate pairs)
    // Glow the whole string, so the emoji fall inside the coloured zone.
    val s = Glow.trail(text, coolLen = 0, Glow.AnswerHot, Glow.AnswerCool)
    assertEquals(plain(s), text)

  test("an empty trail is the empty string"):
    assertEquals(Glow.trail("", coolLen = 0, Glow.AnswerHot, Glow.AnswerCool), "")

  test("the cursor is a single underscore glyph that pulses across frames"):
    assert(plain(Glow.cursor(0)) == "_", plain(Glow.cursor(0)))
    // The breathing pulse means different frames carry different colours.
    assertNotEquals(Glow.cursor(0), Glow.cursor(17))

  test("sweep strips back to exactly the original text"):
    assertEquals(plain(Glow.sweep("auk is thinking", 999L)), "auk is thinking")

  test("an empty sweep is the empty string"):
    assertEquals(Glow.sweep("", 1234L), "")

  test("the glyph under the highlight centre is emboldened"):
    // travel = 5 + 2*4 + 8 = 21; centre = (timeMs/70) % 21 - 4. At 420 ms the
    // centre sits on index 2, so that glyph carries the bold SGR flag.
    assert(Glow.sweep("hello", 420L).contains("[0;1;"), Glow.sweep("hello", 420L))

  test("the shimmer advances with wall-clock time"):
    assertNotEquals(Glow.sweep("auk is thinking", 0L), Glow.sweep("auk is thinking", 500L))

  /** Two rows of the Z logo's shape, with their gradient colours. */
  private val ShineArt: Vector[(String, Glow.Rgb)] = Vector(
    "██████████" -> (90, 240, 255),
    "  ▄██▀" -> (123, 183, 248),
  )

  test("shine strips back to exactly the original art"):
    assertEquals(Glow.shine(ShineArt, 1234L).map(plain), ShineArt.map(_._1))

  test("shine at rest renders every glyph at its row's base colour, byte-stable"):
    // span = 10 + 3 = 13; travel = 13 + 2*4 + 18 = 39 columns; the band's centre
    // is (t/75) % 39 - 4. Both instants land in the rest gap (centres 16 and
    // 28), past every row — the frames are identical and carry the base colour.
    val a = Glow.shine(ShineArt, 1500L)
    val b = Glow.shine(ShineArt, 2400L)
    assertEquals(a, b)
    assert(a(0).contains(Style.fg(Color.True(90, 240, 255)).setSequence), a(0))

  test("shine mid-sweep: each row glints at its own centre, trailing the diagonal"):
    // At 750 ms the centre sits on column 6 of row 0 and — three columns of
    // slant later — column 3 of row 1: both glyphs carry the exact hot colour.
    val hot = Style.fg(Color.True(231, 249, 255)).setSequence
    val rows = Glow.shine(ShineArt, 750L)
    assert(rows(0).contains(hot), rows(0))
    assert(rows(1).contains(hot), rows(1))
    assertNotEquals(rows(0), Glow.shine(ShineArt, 0L)(0))

  test("an empty shine row is the empty string"):
    assertEquals(Glow.shine(Vector("" -> (0, 0, 0)), 500L), Vector(""))
