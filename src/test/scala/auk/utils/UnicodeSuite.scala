package auk.utils

class UnicodeSuite extends munit.FunSuite:
  import Unicode.replaceLoneSurrogates

  // Lone surrogates are written as `\uXXXX` escapes so the source file itself
  // stays valid UTF-8; the compiler materialises the bad code unit at runtime.

  test("a lone high surrogate is replaced with U+FFFD"):
    assertEquals(replaceLoneSurrogates("a\uD800b"), "a�b")

  test("a lone low surrogate is replaced with U+FFFD"):
    assertEquals(replaceLoneSurrogates("a\uDC00b"), "a�b")

  test("a high surrogate at the very end is replaced"):
    assertEquals(replaceLoneSurrogates("ab\uD800"), "ab�")

  test("a valid surrogate pair (an emoji) is left intact"):
    val grin = "x😀y" // U+1F600 😀
    assertEquals(replaceLoneSurrogates(grin), grin)

  test("two high surrogates in a row each become U+FFFD"):
    assertEquals(replaceLoneSurrogates("\uD800\uD800"), "��")

  test("a reversed pair (low then high) is two lone surrogates"):
    assertEquals(replaceLoneSurrogates("\uDC00\uD800"), "��")

  test("a valid pair followed by a lone low surrogate keeps the pair, fixes the stray"):
    assertEquals(replaceLoneSurrogates("😀\uDC00"), "😀�")

  test("a high surrogate followed by a non-surrogate is replaced"):
    assertEquals(replaceLoneSurrogates("\uD800Z"), "�Z")

  test("surrogate-free text is returned unchanged, same instance (no allocation)"):
    val s = "plain ascii and unicode: café — 日本語"
    assert(replaceLoneSurrogates(s) eq s)

  test("the empty string is returned as-is"):
    assertEquals(replaceLoneSurrogates(""), "")

  test("a messy string keeps the emoji pair and collapses every lone surrogate"):
    // high+non-low, lone low, a valid pair, then a high at the end.
    assertEquals(replaceLoneSurrogates("before\uD800mid\uDC00😀end\uDBFF"), "before�mid�😀end�")
