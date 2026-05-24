package auk.llm.tools

class JsonSuite extends munit.FunSuite:

  test("parse primitives") {
    assertEquals(Json.parse("true"), Right(Json.Bool(true)))
    assertEquals(Json.parse("false"), Right(Json.Bool(false)))
    assertEquals(Json.parse("null"), Right(Json.Null))
    assertEquals(Json.parse("\"hello\""), Right(Json.Str("hello")))
  }

  test("parse numbers including fractions and exponents") {
    assertEquals(Json.parse("0"), Right(Json.Num(BigDecimal(0))))
    assertEquals(Json.parse("-42"), Right(Json.Num(BigDecimal(-42))))
    assertEquals(Json.parse("3.14"), Right(Json.Num(BigDecimal("3.14"))))
    assertEquals(Json.parse("1e3"), Right(Json.Num(BigDecimal("1e3"))))
    assertEquals(Json.parse("-2.5E-2"), Right(Json.Num(BigDecimal("-2.5E-2"))))
    // large integers retain precision
    assertEquals(
      Json.parse("123456789012345678901234567890"),
      Right(Json.Num(BigDecimal("123456789012345678901234567890")))
    )
  }

  test("parse string escapes") {
    assertEquals(Json.parse("\"a\\nb\""), Right(Json.Str("a\nb")))
    assertEquals(Json.parse("\"\\t\\r\\b\\f\""), Right(Json.Str("\t\r\b\f")))
    assertEquals(Json.parse("\"quote: \\\"\""), Right(Json.Str("quote: \"")))
    assertEquals(Json.parse("\"slash: \\/\""), Right(Json.Str("slash: /")))
    assertEquals(Json.parse("\"\\u00e9\""), Right(Json.Str("é")))
  }

  test("parse arrays and objects, including empty and nested") {
    assertEquals(Json.parse("[]"), Right(Json.Arr(Nil)))
    assertEquals(Json.parse("{}"), Right(Json.Obj(Nil)))
    assertEquals(
      Json.parse("[1, 2, 3]"),
      Right(Json.Arr(List(Json.num(1), Json.num(2), Json.num(3))))
    )
    assertEquals(
      Json.parse("""{"a": 1, "b": [true, null]}"""),
      Right(
        Json.Obj(
          List(
            "a" -> Json.num(1),
            "b" -> Json.Arr(List(Json.Bool(true), Json.Null))
          )
        )
      )
    )
  }

  test("parse tolerates surrounding and internal whitespace") {
    assertEquals(
      Json.parse("  {\n  \"a\" : 1\n}  "),
      Right(Json.Obj(List("a" -> Json.num(1))))
    )
  }

  test("Obj.get returns the last occurrence of a duplicated key") {
    val obj = Json.parse("""{"a": 1, "a": 2}""").toOption.get.asInstanceOf[Json.Obj]
    assertEquals(obj.get("a"), Some(Json.num(2)))
    assertEquals(obj.get("missing"), None)
  }

  test("parse rejects malformed input") {
    assert(Json.parse("").isLeft)
    assert(Json.parse("{").isLeft)
    assert(Json.parse("[1,]").isLeft)
    assert(Json.parse("{\"a\" 1}").isLeft)
    assert(Json.parse("nul").isLeft)
    assert(Json.parse("\"unterminated").isLeft)
    assert(Json.parse("1 2").isLeft) // trailing characters
    assert(Json.parse("{a: 1}").isLeft) // unquoted key
  }

  test("render round-trips through parse") {
    val texts = List(
      "true",
      "null",
      "\"hi\"",
      "-3.5",
      "[1,2,3]",
      """{"a":1,"b":[true,null],"c":"x"}"""
    )
    texts.foreach { t =>
      val parsed = Json.parse(t)
      assert(parsed.isRight, s"failed to parse $t")
      val rendered = parsed.toOption.get.render
      assertEquals(Json.parse(rendered), parsed, s"round-trip mismatch for $t")
    }
  }

  test("render escapes special characters") {
    assertEquals(Json.Str("a\"b\\c\nd").render, "\"a\\\"b\\\\c\\nd\"")
  }
