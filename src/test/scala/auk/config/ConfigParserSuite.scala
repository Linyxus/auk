package auk.config

class ConfigParserSuite extends munit.FunSuite:

  private def parse(text: String): RawConfig =
    ConfigParser.parse(text) match
      case Right(raw) => raw
      case Left(errs) => fail(s"unexpected parse errors: ${errs.map(_.render).mkString("; ")}")

  private def errors(text: String): List[ConfigError] =
    ConfigParser.parse(text) match
      case Left(errs) => errs
      case Right(_)   => fail("expected parse errors but parsing succeeded")

  test("inline key = value yields an Inline raw value") {
    val raw = parse("model.id = glm-5.1")
    assertEquals(raw.get(List("model", "id")), Some(RawValue.Inline("glm-5.1", 1)))
  }

  test("scope header expands following keys to dotted paths") {
    val raw = parse("""[model]
                      |id = glm-5.1
                      |provider = openrouter""".stripMargin)
    assertEquals(raw.get(List("model", "id")), Some(RawValue.Inline("glm-5.1", 2)))
    assertEquals(raw.get(List("model", "provider")), Some(RawValue.Inline("openrouter", 3)))
  }

  test("dotted scope header sets an absolute prefix") {
    val raw = parse("""[model.thinking]
                      |budget = 1024""".stripMargin)
    assertEquals(raw.get(List("model", "thinking", "budget")), Some(RawValue.Inline("1024", 2)))
  }

  test("dotted keys inside a scope compose") {
    val raw = parse("""[model]
                      |coord.lat = 1""".stripMargin)
    assertEquals(raw.get(List("model", "coord", "lat")), Some(RawValue.Inline("1", 2)))
  }

  test("block form attaches indented lines to an empty-RHS key") {
    val raw = parse("""foo.bar =
                      |  a
                      |  b
                      |  c""".stripMargin)
    assertEquals(raw.get(List("foo", "bar")), Some(RawValue.Block(List("a", "b", "c"), 1)))
  }

  test("a key with an inline RHS followed by indented lines is an error") {
    val es = errors("""foo = a
                      |  b""".stripMargin)
    assertEquals(es.length, 1)
    assertEquals(es.head.line, Some(2))
    assert(es.head.message.contains("does not belong"))
  }

  test("empty-RHS key with no continuation yields an empty inline value") {
    val raw = parse("foo =")
    assertEquals(raw.get(List("foo")), Some(RawValue.Inline("", 1)))
  }

  test("full-line comments and blank lines are ignored") {
    val raw = parse("""# a comment
                      |
                      |a = 1
                      |  # indented comment
                      |b = 2""".stripMargin)
    assertEquals(raw.paths.toList, List(List("a"), List("b")))
  }

  test("blank lines inside a block are transparent") {
    val raw = parse("""xs =
                      |  a
                      |
                      |  b""".stripMargin)
    assertEquals(raw.get(List("xs")), Some(RawValue.Block(List("a", "b"), 1)))
  }

  test("a comment does not terminate a block") {
    val raw = parse("""xs =
                      |  a
                      |# note
                      |  b""".stripMargin)
    assertEquals(raw.get(List("xs")), Some(RawValue.Block(List("a", "b"), 1)))
  }

  test("duplicate key reports its line number") {
    val es = errors("""a = 1
                      |a = 2""".stripMargin)
    assertEquals(es.length, 1)
    assert(es.head.message.contains("duplicate key 'a'"))
    assertEquals(es.head.line, Some(2))
  }

  test("orphan indented line with no pending key is an error") {
    val es = errors("  stray")
    assertEquals(es.length, 1)
    assert(es.head.message.contains("does not belong"))
  }

  test("bare junk line is an error") {
    val es = errors("just some words")
    assertEquals(es.length, 1)
    assert(es.head.message.contains("expected 'key = value'"))
  }

  test("malformed scope headers are errors") {
    assert(errors("[]").head.message.contains("malformed scope header"))
    assert(errors("[a..b]").head.message.contains("malformed scope header"))
    assert(errors("[a b]").head.message.contains("malformed scope header"))
  }

  test("missing key before '=' is an error") {
    val es = errors("= 5")
    assertEquals(es.length, 1)
    assert(es.head.message.contains("missing key"))
  }

  test("invalid key characters are an error") {
    val es = errors("a/b = 1")
    assertEquals(es.length, 1)
    assert(es.head.message.contains("invalid key"))
  }

  test("multiple parse errors are accumulated") {
    val es = errors("""= 1
                      |[]
                      |a = 1
                      |a = 2""".stripMargin)
    assertEquals(es.length, 3)
  }

  test("2-space and 4-space indentation are both accepted for blocks") {
    val two = parse("xs =\n  a\n  b").get(List("xs"))
    val four = parse("xs =\n    a\n    b").get(List("xs"))
    assertEquals(two, Some(RawValue.Block(List("a", "b"), 1)))
    assertEquals(four, Some(RawValue.Block(List("a", "b"), 1)))
  }

  test("paths preserve source order") {
    val raw = parse("""b = 1
                      |a = 2
                      |c = 3""".stripMargin)
    assertEquals(raw.paths.toList, List(List("b"), List("a"), List("c")))
  }
