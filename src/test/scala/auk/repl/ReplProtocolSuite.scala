package auk.repl

import scala.scalajs.js

class ReplProtocolSuite extends munit.FunSuite:

  // -- response parsing --------------------------------------------------------

  test("parses a successful eval response"):
    val line =
      """{"op":"eval","ok":true,"output":"val xs: List[Int] = List(1, 2, 3)\n","stdout":"hi\n","stderr":"","stateVersion":3}"""
    val r = ReplProtocol.parse(line).toOption.get
    assertEquals(r.op, "eval")
    assertEquals(r.ok, true)
    assertEquals(r.output, "val xs: List[Int] = List(1, 2, 3)\n")
    assertEquals(r.stdout, "hi\n")
    assertEquals(r.stderr, "")
    assertEquals(r.error, None)
    assertEquals(r.stateVersion, 3)

  test("parses a failed eval with the error summary"):
    val line =
      """{"op":"eval","ok":false,"output":"-- [E018] Syntax Error ---","stdout":"","stderr":"","stateVersion":1,"error":"expression expected"}"""
    val r = ReplProtocol.parse(line).toOption.get
    assertEquals(r.ok, false)
    assertEquals(r.error, Some("expression expected"))
    assert(r.output.contains("Syntax Error"))
    assertEquals(r.stateVersion, 1)

  test("parses a shutdown ack"):
    val r = ReplProtocol.parse("""{"op":"shutdown","ok":true,"stateVersion":4}""").toOption.get
    assertEquals(r.op, "shutdown")
    assertEquals(r.ok, true)
    assertEquals(r.stateVersion, 4)

  test("parses a protocol error (no stateVersion)"):
    val r = ReplProtocol
      .parse("""{"op":"protocol","ok":false,"error":"set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN"}""")
      .toOption
      .get
    assertEquals(r.op, "protocol")
    assertEquals(r.ok, false)
    assert(r.error.get.contains("DOTTY_CLASSPATH_BIN"))
    assertEquals(r.stateVersion, 0)

  test("a non-JSON line is a Left"):
    assert(ReplProtocol.parse("Fatal error: out of memory").isLeft)

  test("a JSON line without an op is a Left"):
    assert(ReplProtocol.parse("""{"ok":true}""").isLeft)

  // -- request encoding --------------------------------------------------------

  test("eval requests escape newlines and quotes, staying single-line"):
    val code = "val s = \"a\\nb\"\nprintln(s)"
    val json = ReplProtocol.evalRequest(code)
    assert(!json.contains('\n'), json)
    val parsed = js.JSON.parse(json)
    assertEquals(parsed.op.asInstanceOf[String], "eval")
    assertEquals(parsed.code.asInstanceOf[String], code)

  test("the shutdown request is the bare op"):
    val parsed = js.JSON.parse(ReplProtocol.shutdownRequest)
    assertEquals(parsed.op.asInstanceOf[String], "shutdown")

  // -- ANSI stripping ------------------------------------------------------------

  test("stripAnsi removes SGR colour sequences"):
    assertEquals(ReplProtocol.stripAnsi("a \u001b[31mred\u001b[0m b"), "a red b")

  test("stripAnsi leaves plain text untouched"):
    assertEquals(ReplProtocol.stripAnsi("val x: Int = 1\n"), "val x: Int = 1\n")
