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

  test("the cursor is a single block glyph that pulses across frames"):
    assert(plain(Glow.cursor(0)) == "▌", plain(Glow.cursor(0)))
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
