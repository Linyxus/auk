package auk.library

import scala.scalajs.js

/** Unit tests for the worker-side typed schema/decode derivation. Runs under
  * Node (the library test jsEnv injects `globalThis.require`). */
class LibToolInputSuite extends munit.FunSuite:

  case class Finding(path: String, line: Int) derives LibToolInput
  case class Report(findings: List[Finding], summary: String, note: Option[String]) derives LibToolInput

  enum Color derives LibToolInput:
    case Red, Green, Blue

  test("product schema is an object with properties and required (Option excluded)"):
    val s = js.JSON.stringify(LibToolInput[Report].schema)
    assert(s.contains("\"type\":\"object\""), s)
    assert(s.contains("\"findings\""), s)
    assert(s.contains("\"summary\""), s)
    // `note` is optional, so it is not in `required`.
    val schema = LibToolInput[Report].schema.asInstanceOf[js.Dynamic]
    val required = schema.required.asInstanceOf[js.Array[String]]
    assert(required.contains("summary"), required.mkString(","))
    assert(!required.contains("note"), required.mkString(","))

  test("nested product decodes a round-tripped JSON value"):
    val json = js.JSON.parse("""{"findings":[{"path":"a.scala","line":3}],"summary":"ok"}""")
    LibToolInput[Report].decode(json) match
      case Right(r) =>
        assertEquals(r.summary, "ok")
        assertEquals(r.findings.length, 1)
        assertEquals(r.findings.head.path, "a.scala")
        assertEquals(r.findings.head.line, 3)
        assertEquals(r.note, None)
      case Left(e) => fail(s"decode failed: $e")

  test("present Option field decodes to Some"):
    val json = js.JSON.parse("""{"findings":[],"summary":"s","note":"hi"}""")
    assertEquals(LibToolInput[Report].decode(json).map(_.note), Right(Some("hi")))

  test("missing required field is an error"):
    val json = js.JSON.parse("""{"findings":[]}""")
    assert(LibToolInput[Report].decode(json).isLeft)

  test("wrong type is an error with a field path"):
    val json = js.JSON.parse("""{"findings":[{"path":"a","line":"nope"}],"summary":"s"}""")
    val res = LibToolInput[Report].decode(json)
    assert(res.isLeft, res.toString)

  test("enum schema lists cases and decodes by name"):
    val s = js.JSON.stringify(LibToolInput[Color].schema)
    assert(s.contains("\"enum\""), s)
    assert(s.contains("Green"), s)
    assertEquals(LibToolInput[Color].decode(js.JSON.parse("\"Green\"")), Right(Color.Green))
    assert(LibToolInput[Color].decode(js.JSON.parse("\"Purple\"")).isLeft)
