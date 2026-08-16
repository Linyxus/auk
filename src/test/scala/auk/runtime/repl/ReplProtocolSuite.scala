package auk.runtime.repl

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

  // -- line classification -----------------------------------------------------

  private def classified(line: String): ReplProtocol.Line =
    ReplProtocol.classify(line) match
      case Right(l)  => l
      case Left(err) => fail(s"expected a classified line, got: $err")

  test("a response op classifies as a Reply carrying exactly what parse yields"):
    val line =
      """{"op":"eval","ok":true,"output":"val x: Int = 1\n","stdout":"hi\n","stderr":"e","stateVersion":7,"error":"boom"}"""
    classified(line) match
      case ReplProtocol.Line.Reply(r) => assertEquals(r, ReplProtocol.parse(line).toOption.get)
      case other                      => fail(s"expected a Reply, got $other")

  test("reset, shutdown and protocol also classify as replies"):
    for op <- List("reset", "shutdown", "protocol") do
      classified(s"""{"op":"$op","ok":true,"stateVersion":2}""") match
        case ReplProtocol.Line.Reply(r) =>
          assertEquals(r.op, op)
          assertEquals(r.stateVersion, 2)
        case other => fail(s"expected a Reply for $op, got $other")

  test("hello carries the protocol, tick interval and pid"):
    assertEquals(
      classified("""{"op":"hello","protocol":2,"tickMs":250,"pid":4321}"""),
      ReplProtocol.Line.Hello(2, 250, Some(4321))
    )

  test("hello defaults every missing field"):
    assertEquals(classified("""{"op":"hello"}"""), ReplProtocol.Line.Hello(2, 1000, None))

  test("hello without a pid leaves it unknown"):
    assertEquals(
      classified("""{"op":"hello","protocol":3,"tickMs":500}"""),
      ReplProtocol.Line.Hello(3, 500, None)
    )

  test("hello defaults fields of the wrong type"):
    assertEquals(
      classified("""{"op":"hello","protocol":"two","tickMs":null,"pid":"none"}"""),
      ReplProtocol.Line.Hello(2, 1000, None)
    )

  test("received classifies as the bare ack"):
    assertEquals(classified("""{"op":"received"}"""), ReplProtocol.Line.Received)

  test("tick carries its sequence number"):
    assertEquals(classified("""{"op":"tick","seq":12}"""), ReplProtocol.Line.Tick(12))

  test("tick without a seq defaults to zero"):
    assertEquals(classified("""{"op":"tick"}"""), ReplProtocol.Line.Tick(0))

  test("progress carries the phase the worker named"):
    assertEquals(
      classified("""{"op":"progress","phase":"compiling"}"""),
      ReplProtocol.Line.Progress("compiling")
    )

  test("a phase is opaque text, not a vocabulary this parent knows"):
    assertEquals(
      classified("""{"op":"progress","phase":"resolving dependencies"}"""),
      ReplProtocol.Line.Progress("resolving dependencies")
    )

  test("progress that names no phase is Unknown, not an error"):
    // Nothing to show and nothing to reject: a progress line is worth exactly
    // the stage it names, so one without a usable phase is dropped like any op
    // this parent cannot read.
    assertEquals(classified("""{"op":"progress","pct":40}"""), ReplProtocol.Line.Unknown("progress"))
    assertEquals(classified("""{"op":"progress","phase":7}"""), ReplProtocol.Line.Unknown("progress"))
    assertEquals(classified("""{"op":"progress","phase":null}"""), ReplProtocol.Line.Unknown("progress"))

  test("an unrecognised op is preserved as Unknown"):
    assertEquals(classified("""{"op":"wat","pct":40}"""), ReplProtocol.Line.Unknown("wat"))

  test("classify rejects a line without an op"):
    assert(ReplProtocol.classify("""{"ok":true}""").isLeft)

  test("classify rejects a non-string op"):
    assert(ReplProtocol.classify("""{"op":7}""").isLeft)

  test("classify rejects a non-JSON line"):
    assert(ReplProtocol.classify("Fatal error: out of memory").isLeft)

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

  // -- field bounds --------------------------------------------------------------
  // A worker can hand this parent more than it can afford to carry: on
  // 2026-08-15 a 40 MB reply (a grep that had matched the session's own logs)
  // exhausted the heap here, three copies deep into the eval tool's transforms.

  test("a field at the bound is untouched"):
    val text = "x" * ReplProtocol.MaxFieldChars
    val r = ReplProtocol.parse(replyWith(stdout = text)).toOption.get
    assertEquals(r.stdout, text)

  test("an over-long field is bounded, and says how much of it is missing"):
    val huge = "x" * (ReplProtocol.MaxFieldChars * 4)
    val r = ReplProtocol.parse(replyWith(stdout = huge)).toOption.get
    assertEquals(r.stdout.length, ReplProtocol.MaxFieldChars)
    assert(
      r.stdout.contains(s"[${ReplProtocol.MaxFieldChars} of ${huge.length} characters kept"),
      r.stdout.takeRight(200)
    )

  test("every text field is bounded, not just stdout"):
    val huge = "y" * (ReplProtocol.MaxFieldChars + 1)
    val r = ReplProtocol
      .parse(replyWith(stdout = huge, output = huge, stderr = huge, error = Some(huge)))
      .toOption
      .get
    assertEquals(r.output.length, ReplProtocol.MaxFieldChars)
    assertEquals(r.stdout.length, ReplProtocol.MaxFieldChars)
    assertEquals(r.stderr.length, ReplProtocol.MaxFieldChars)
    assertEquals(r.error.map(_.length), Some(ReplProtocol.MaxFieldChars))

  test("classify bounds the same fields as parse"):
    val huge = "z" * (ReplProtocol.MaxFieldChars + 10)
    ReplProtocol.classify(replyWith(stdout = huge)) match
      case Right(ReplProtocol.Line.Reply(r)) => assertEquals(r.stdout.length, ReplProtocol.MaxFieldChars)
      case other                             => fail(s"expected a Reply, got $other")

  test("the tail of an over-long field survives — that is where the last line is"):
    // The loop reads its checker's verdict out of the LAST `auk:loop:check:`
    // line of stdout. A head-only cut would drop it and report a checker that
    // worked as one that printed nothing.
    val marker = """auk:loop:check:{"passed":true,"reasons":[],"metrics":{}}"""
    val noisy = ("chatter\n" * (ReplProtocol.MaxFieldChars / 4)) + marker + "\n"
    assert(noisy.length > ReplProtocol.MaxFieldChars)
    val r = ReplProtocol.parse(replyWith(stdout = noisy)).toOption.get
    assertEquals(r.stdout.linesIterator.filter(_.startsWith("auk:loop:check:")).toList, List(marker))

  /** An eval reply line carrying the given fields. */
  private def replyWith(
      stdout: String = "",
      output: String = "",
      stderr: String = "",
      error: Option[String] = None
  ): String =
    val d = js.Dynamic.literal(
      op = "eval",
      ok = true,
      output = output,
      stdout = stdout,
      stderr = stderr,
      stateVersion = 1
    )
    error.foreach(e => d.updateDynamic("error")(e))
    js.JSON.stringify(d)
