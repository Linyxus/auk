package auk.webui

import HlKind.*

class HighlightSuite extends munit.FunSuite:

  private def kindOf(code: String, text: String): Option[HlKind] =
    Highlight.scala(code).find(_.text == text).map(_.kind)

  test("the concatenated token text always equals the input (no chars dropped)"):
    val samples = List(
      "val x = 1",
      "def f(a: Int): Int = a + 1",
      "// a comment\n\"a string\"",
      "List(1, 2, 3).map(_ + 1)",
      "object Foo extends Bar",
      """val p = "src/a.scala"""",
      "",
      "   \n  "
    )
    samples.foreach(s => assertEquals(Highlight.scala(s).map(_.text).mkString, s, s"round-trip failed for: $s"))

  test("keywords are tagged Keyword"):
    assertEquals(kindOf("val x = 1", "val"), Some(Keyword))
    assertEquals(kindOf("if a then b", "if"), Some(Keyword))

  test("a name following a def-keyword is a Def"):
    assertEquals(kindOf("def foo = 1", "foo"), Some(Def))
    assertEquals(kindOf("val bar = 2", "bar"), Some(Def))
    assertEquals(kindOf("class Baz", "Baz"), Some(Def)) // def-keyword wins over capitalization

  test("capitalised identifiers are Types"):
    assertEquals(kindOf("val a = List", "List"), Some(Type))
    assertEquals(kindOf("x: Int", "Int"), Some(Type))

  test("strings, numbers, comments are tagged"):
    assert(Highlight.scala(""""hello"""").exists(_.kind == Str))
    assert(Highlight.scala("42").exists(_.kind == Num))
    assert(Highlight.scala("3.14").exists(_.kind == Num))
    assert(Highlight.scala("// note").exists(_.kind == Comment))
    assert(Highlight.scala("/* block */").exists(_.kind == Comment))

  test("operators and punctuation are distinguished"):
    assertEquals(kindOf("a + b", "+"), Some(Op))
    assertEquals(kindOf("a => b", "=>"), Some(Op))
    assertEquals(kindOf("f(x)", "("), Some(Punct))
    assertEquals(kindOf("a.b", "."), Some(Punct))

  test("a triple-quoted string is a single Str token"):
    val ts = Highlight.scala("\"\"\"a\nb\"\"\"")
    assertEquals(ts.count(_.kind == Str), 1)
    assertEquals(ts.map(_.text).mkString, "\"\"\"a\nb\"\"\"")

  test("a string with an escaped quote stays one token"):
    val ts = Highlight.scala(""""a\"b"""")
    assertEquals(ts.count(_.kind == Str), 1)

  test("soft keywords are tagged Soft"):
    assertEquals(kindOf("inline def f = 1", "inline"), Some(Soft))

  test("an unterminated string does not throw and consumes to end"):
    val ts = Highlight.scala("\"oops")
    assertEquals(ts.map(_.text).mkString, "\"oops")
    assert(ts.exists(_.kind == Str))
