package auk.tui.app

import auk.tui.render.{Ansi, Style, Width}

/** Exercises the shared wrap engine through the public `wrapText` element + Layout. */
class WrapSuite extends munit.FunSuite:

  private def lines(e: Element, w: Int): Vector[String] = Layout.lay(e, w).map(_.plain)

  test("word mode breaks between words, not mid-word"):
    val ls = lines(wrapText("", "", "alpha beta gamma"), 9)
    assertEquals(ls, Vector("alpha", "beta", "gamma"))

  test("word mode packs as many words as fit per line"):
    val ls = lines(wrapText("", "", "aa bb cc dd"), 6)
    // "aa bb" = 5 ≤ 6; adding " cc" would be 8 > 6.
    assertEquals(ls, Vector("aa bb", "cc dd"))

  test("a word longer than the line is character-broken as a last resort"):
    val ls = lines(wrapText("", "", "abcdefgh"), 4)
    assertEquals(ls, Vector("abcd", "efgh"))

  test("an explicit newline always breaks"):
    assertEquals(lines(wrapText("", "", "a\nb"), 40), Vector("a", "b"))

  test("continuation lines use the next-prefix"):
    val ls = lines(wrapText("> ", "| ", "alpha beta"), 7)
    assertEquals(ls.head, "> alpha")
    assert(ls.tail.forall(_.startsWith("| ")), ls.toString)

  test("char mode breaks at any code point (verbatim)"):
    assertEquals(lines(wrapText("", "", "abcdef", Wrap.Char), 4), Vector("abcd", "ef"))

  test("a trailing styled space is preserved (the input cursor cell)"):
    // A reverse-video space at the very end must survive word wrapping.
    val value = "hi" + Style.Reverse.setSequence + " " + Ansi.Reset
    val ls = lines(wrapText("", "", value), 40)
    assertEquals(ls, Vector("hi "))

  test("wrapped lines never exceed the width"):
    val text = "the quick brown fox jumps over the lazy dog"
    for w <- Seq(8, 12, 20) do
      assert(lines(wrapText("", "", text), w).forall(l => Width.stringWidth(l) <= w), s"width $w")
