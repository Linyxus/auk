package auk.config

enum Color derives ConfigType:
  case Red, Green, Blue

class ConfigTypeSuite extends munit.FunSuite:

  private def iv(s: String) = RawValue.Inline(s, 1)
  private def block(xs: String*) = RawValue.Block(xs.toList, 1)

  test("string takes the whole inline remainder, spaces kept") {
    assertEquals(ConfigType[String].parse(iv("a b c")), Right("a b c"))
  }

  test("quoted string strips one surrounding pair and unescapes") {
    assertEquals(ConfigType[String].parse(iv("\"a \\\"b\\\" c\"")), Right("a \"b\" c"))
  }

  test("int parses, rejects non-int with the source line") {
    assertEquals(ConfigType[Int].parse(iv("42")), Right(42))
    val e = ConfigType[Int].parse(RawValue.Inline("x", 9)).left.toOption.get
    assert(e.message.contains("not an integer"))
    assertEquals(e.line, Some(9))
  }

  test("long and double parse") {
    assertEquals(ConfigType[Long].parse(iv("9999999999")), Right(9999999999L))
    assertEquals(ConfigType[Double].parse(iv("3.14")), Right(3.14))
  }

  test("boolean is case-insensitive") {
    assertEquals(ConfigType[Boolean].parse(iv("true")), Right(true))
    assertEquals(ConfigType[Boolean].parse(iv("FALSE")), Right(false))
    assert(ConfigType[Boolean].parse(iv("yes")).isLeft)
  }

  test("a scalar type rejects a block value") {
    val e = ConfigType[Int].parse(RawValue.Block(List("1"), 4)).left.toOption.get
    assert(e.message.contains("got a list"))
    assertEquals(e.line, Some(4))
  }

  test("enum derives and matches case-insensitively") {
    assertEquals(ConfigType[Color].parse(iv("green")), Right(Color.Green))
    assertEquals(ConfigType[Color].parse(iv("BLUE")), Right(Color.Blue))
  }

  test("enum miss lists the allowed values") {
    val e = ConfigType[Color].parse(iv("purple")).left.toOption.get
    assert(e.message.contains("Red"))
    assert(e.message.contains("Green"))
    assert(e.message.contains("Blue"))
  }

  test("enum describe joins labels with |") {
    assertEquals(ConfigType[Color].describe, "Red|Green|Blue")
  }

  test("inline list splits on whitespace") {
    assertEquals(ConfigType[List[String]].parse(iv("a b c")), Right(List("a", "b", "c")))
  }

  test("inline list keeps a quoted item with spaces") {
    assertEquals(ConfigType[List[String]].parse(iv("\"a b\" c")), Right(List("a b", "c")))
  }

  test("inline list of ints") {
    assertEquals(ConfigType[List[Int]].parse(iv("1 2 3")), Right(List(1, 2, 3)))
  }

  test("block list is one item per line") {
    assertEquals(ConfigType[List[String]].parse(block("a", "b", "c")), Right(List("a", "b", "c")))
  }

  test("empty inline and empty block both decode to the empty list") {
    assertEquals(ConfigType[List[String]].parse(iv("")), Right(Nil))
    assertEquals(ConfigType[List[String]].parse(RawValue.Block(Nil, 1)), Right(Nil))
  }

  test("a bad list element error is prefixed with its index") {
    val e = ConfigType[List[Int]].parse(block("1", "x", "3")).left.toOption.get
    assertEquals(e.path, List("[1]"))
    assert(e.message.contains("not an integer"))
  }

  test("an unterminated quote in an inline list is an error") {
    val e = ConfigType[List[String]].parse(iv("\"oops")).left.toOption.get
    assert(e.message.contains("unterminated quote"))
  }

  test("list describe names the element type") {
    assertEquals(ConfigType[List[Int]].describe, "list of integer")
  }

  test("oneOf returns the first matching alternative") {
    val ct = ConfigType.oneOf[Int | String](ConfigType[Int], ConfigType[String])
    assertEquals(ct.parse(iv("123")), Right(123))
    assertEquals(ct.parse(iv("abc")), Right("abc"))
  }

  test("oneOf total failure lists every alternative's describe") {
    val ct = ConfigType.oneOf[Int | Boolean](ConfigType[Int], ConfigType[Boolean])
    val e = ct.parse(iv("abc")).left.toOption.get
    assert(e.message.contains("integer"))
    assert(e.message.contains("boolean"))
  }
