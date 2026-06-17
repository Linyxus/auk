package auk.runtime

import auk.llm.tools.Json

/** Unit tests for the host-side result validator. It must mirror the worker
  * decoder ([[auk.library.LibToolInput]]) closely enough that any value it accepts
  * the worker can also decode — so the cases below pin both the accept/reject
  * decisions and the exact, path-qualified error vocabulary. */
class ResultSchemaSuite extends munit.FunSuite:

  // -- schema builders (the JSON-Schema fragments the worker sends) -------------
  private def S(t: String, extra: (String, Json)*): Json = Json.Obj(("type" -> Json.Str(t)) :: extra.toList)
  private val strS = S("string")
  private val intS = S("integer")
  private val numS = S("number")
  private val boolS = S("boolean")
  private def enumS(labels: String*): Json = S("string", "enum" -> Json.Arr(labels.map(Json.Str(_)).toList))
  private def arrS(items: Json): Json = S("array", "items" -> items)
  private def objS(props: (String, Json)*)(required: String*): Json =
    S("object", "properties" -> Json.Obj(props.toList), "required" -> Json.Arr(required.map(Json.Str(_)).toList))

  private def ok(schema: Json, value: Json): Unit =
    assertEquals(ResultSchema.validate(schema, value), Right(()), s"expected $value to match $schema")
  private def err(schema: Json, value: Json, message: String): Unit =
    assertEquals(ResultSchema.validate(schema, value), Left(message))

  // -- primitives --------------------------------------------------------------

  test("string accepts a string and rejects everything else with its type name"):
    ok(strS, Json.Str("x"))
    err(strS, Json.num(1), "expected string, got number")
    err(strS, Json.Bool(true), "expected string, got boolean")
    err(strS, Json.Null, "expected string, got null")
    err(strS, Json.Arr(Nil), "expected string, got array")
    err(strS, Json.Obj(Nil), "expected string, got object")

  test("an enum string must be one of the listed labels"):
    ok(enumS("a", "b"), Json.Str("a"))
    ok(enumS("a", "b"), Json.Str("b"))
    err(enumS("a", "b"), Json.Str("c"), "'c' is not one of a, b")
    err(enumS("a", "b"), Json.num(1), "expected string, got number")

  test("integer accepts any JSON number (worker truncates) and rejects non-numbers"):
    ok(intS, Json.num(3))
    ok(intS, Json.num(3.5)) // lenient, matches the worker (it truncates)
    err(intS, Json.Str("3"), "expected integer, got string")
    err(intS, Json.Bool(true), "expected integer, got boolean")

  test("number accepts any JSON number and rejects others"):
    ok(numS, Json.num(3.5))
    ok(numS, Json.num(3))
    err(numS, Json.Bool(true), "expected number, got boolean")

  test("boolean accepts a boolean and rejects others"):
    ok(boolS, Json.Bool(true))
    ok(boolS, Json.Bool(false))
    err(boolS, Json.num(1), "expected boolean, got number")

  // -- arrays ------------------------------------------------------------------

  test("array validates each element and reports the failing index"):
    ok(arrS(strS), Json.Arr(List(Json.Str("a"), Json.Str("b"))))
    ok(arrS(strS), Json.Arr(Nil))
    err(arrS(strS), Json.Arr(List(Json.Str("a"), Json.num(2))), "[1]: expected string, got number")
    err(arrS(intS), Json.Str("x"), "expected array, got string")

  test("array of objects reports a nested index + field path"):
    val schema = arrS(objS("a" -> intS)("a"))
    ok(schema, Json.Arr(List(Json.Obj(List("a" -> Json.num(1))), Json.Obj(List("a" -> Json.num(2))))))
    err(schema, Json.Arr(List(Json.Obj(List("a" -> Json.num(1))), Json.Obj(List("a" -> Json.Str("x"))))),
      "[1]: a: expected integer, got string")

  // -- objects -----------------------------------------------------------------

  test("object accepts all required fields present and of the right type"):
    val schema = objS("msg" -> strS, "n" -> intS)("msg", "n")
    ok(schema, Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.num(7))))

  test("object rejects a missing required field, naming it"):
    val schema = objS("msg" -> strS, "n" -> intS)("msg", "n")
    err(schema, Json.Obj(List("msg" -> Json.Str("hi"))), "missing required field 'n'")

  test("object reports a wrong field type with the field path"):
    val schema = objS("msg" -> strS, "n" -> intS)("msg", "n")
    err(schema, Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.Str("x"))), "n: expected integer, got string")

  test("a non-object value for an object schema is rejected"):
    val schema = objS("a" -> strS)("a")
    err(schema, Json.Str("x"), "expected object, got string")
    err(schema, Json.Arr(Nil), "expected object, got array")

  test("extra object keys beyond the schema are ignored (worker parity)"):
    val schema = objS("msg" -> strS)("msg")
    ok(schema, Json.Obj(List("msg" -> Json.Str("hi"), "extra" -> Json.Bool(true))))

  test("nested objects validate recursively with a dotted-ish path"):
    val schema = objS("inner" -> objS("a" -> strS)("a"))("inner")
    ok(schema, Json.Obj(List("inner" -> Json.Obj(List("a" -> Json.Str("x"))))))
    err(schema, Json.Obj(List("inner" -> Json.Obj(Nil))), "inner: missing required field 'a'")

  test("an optional field (absent from required) may be missing, null, or present"):
    val schema = objS("name" -> strS, "note" -> strS)("name") // note is optional
    ok(schema, Json.Obj(List("name" -> Json.Str("x"))))                            // absent
    ok(schema, Json.Obj(List("name" -> Json.Str("x"), "note" -> Json.Null)))       // null → None
    ok(schema, Json.Obj(List("name" -> Json.Str("x"), "note" -> Json.Str("y"))))   // present
    err(schema, Json.Obj(List("name" -> Json.Str("x"), "note" -> Json.num(5))), "note: expected string, got number")

  test("a required field present as null is rejected (only optionals accept null)"):
    val schema = objS("name" -> strS)("name")
    err(schema, Json.Obj(List("name" -> Json.Null)), "name: expected string, got null")

  // -- permissive fallthrough --------------------------------------------------

  test("a schema with no/unknown type accepts any value (matches the worker)"):
    ok(Json.Obj(Nil), Json.Str("anything"))
    ok(Json.Obj(Nil), Json.num(1))
    ok(Json.Null, Json.Bool(true)) // schema isn't even an object
    ok(S("mystery"), Json.Arr(Nil))
