package auk.runtime

import java.nio.file.{Files, Path}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}
import auk.llm.endpoint.Content

class BashSuite extends munit.FunSuite:

  // -- helpers ---------------------------------------------------------------

  private def tempDir(): Path = Files.createTempDirectory("auk-bash").nn

  private def ctxAt(
      dir: Path,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  ): RuntimeContext = RuntimeContext(dir, approvals)

  /** Run a command to completion in a fresh, throwaway working directory. */
  private def run(
      command: String,
      timeoutMs: Option[Int] = None,
      ctx: RuntimeContext = ctxAt(tempDir())
  ): ToolResult =
    Async.blocking:
      given RuntimeContext = ctx
      Bash.execute(BashParams(command, timeoutMs))

  // -- output capture --------------------------------------------------------

  test("captures stdout and reports success"):
    val r = run("echo hello")
    assertEquals(r.output.trim, "hello")
    assertEquals(r.isError, false)
    assertEquals(r.metadata("exitCode"), "0")
    assertEquals(r.metadata("timedOut"), "false")
    assertEquals(r.metadata("truncated"), "false")

  test("merges stderr into the output"):
    val r = run("echo out; echo err 1>&2")
    assert(r.output.contains("out"), r.output)
    assert(r.output.contains("err"), r.output)
    assertEquals(r.isError, false)

  test("preserves multi-line output verbatim, including the trailing newline"):
    val r = run("printf 'a\\nb\\nc\\n'")
    assertEquals(r.output, "a\nb\nc\n")

  test("empty output on success reads as (no output)"):
    val r = run("true")
    assertEquals(r.output, "(no output)")
    assertEquals(r.isError, false)
    assertEquals(r.metadata("exitCode"), "0")

  // -- exit codes ------------------------------------------------------------

  test("non-zero exit is an error and records the code"):
    val r = run("exit 3")
    assert(r.isError)
    assertEquals(r.metadata("exitCode"), "3")
    assert(r.output.contains("[command exited with code 3]"), r.output)

  test("unknown command surfaces shell error and non-zero exit"):
    val r = run("this_command_does_not_exist_xyz")
    assert(r.isError)
    assertNotEquals(r.metadata("exitCode"), "0")
    assert(r.output.toLowerCase.nn.contains("not found"), r.output)

  // -- working directory -----------------------------------------------------

  test("runs in the context working directory"):
    val dir = tempDir()
    Files.writeString(dir.resolve("marker.txt"), "in-cwd")
    val r = run("cat marker.txt", ctx = ctxAt(dir))
    assertEquals(r.output.trim, "in-cwd")
    assertEquals(r.isError, false)

  // -- stdin -----------------------------------------------------------------

  test("stdin is closed so readers see EOF instead of hanging"):
    // `cat` with no redirection would block forever on stdin; with stdin closed
    // it sees EOF, exits 0, and returns promptly (well under the timeout).
    val r = run("cat", timeoutMs = Some(5000))
    assertEquals(r.metadata("timedOut"), "false")
    assertEquals(r.isError, false)

  // -- timeout ---------------------------------------------------------------

  test("a command exceeding the timeout is killed and flagged"):
    val start = System.nanoTime()
    val r = run("sleep 5", timeoutMs = Some(300))
    val elapsedMs = (System.nanoTime() - start) / 1_000_000
    assert(r.isError)
    assertEquals(r.metadata("timedOut"), "true")
    assert(r.output.contains("timed out"), r.output)
    // It must return shortly after the timeout, not after the full sleep.
    assert(elapsedMs < 4000, s"took ${elapsedMs}ms, expected the kill near 300ms")

  // -- truncation ------------------------------------------------------------

  test("output past the cap is truncated and flagged"):
    val r = run("yes a | head -c 200000")
    assertEquals(r.metadata("truncated"), "true")
    assert(r.output.contains("[output truncated"), r.output)
    // Captured text stays bounded (cap + short footer), not the full 200000.
    assert(
      r.output.length <= Bash.MaxOutputBytes + 100,
      s"output length ${r.output.length} exceeds the cap"
    )

  // -- approval gating -------------------------------------------------------

  test("a denied command does not run and reports an error"):
    val dir = tempDir()
    val r = run("touch should_not_exist.txt", ctx = ctxAt(dir, ApprovalPolicy.DenyAll))
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assert(
      !Files.exists(dir.resolve("should_not_exist.txt")),
      "denied command must not have executed"
    )

  test("approval policy receives the tool name and command"):
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val _ = run("echo hi", ctx = ctxAt(tempDir(), recording))
    assertEquals(seen.map(_.toolName), Some("bash"))
    assertEquals(seen.map(_.summary), Some("echo hi"))

  // -- argument validation ---------------------------------------------------

  test("empty command is rejected without running a process"):
    val r = run("   ")
    assert(r.isError)
    assertEquals(r.output, "empty command")

  // -- registry integration --------------------------------------------------

  test("dispatches through the ToolRegistry"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val registry = ToolRegistry.of(Bash)
      val out = registry.dispatch(
        Content.ToolUse("b1", "bash", """{"command":"echo via-registry"}""")
      )
      assertEquals(out.toolUseId, "b1")
      assertEquals(out.isError, false)
      assert(out.content.contains("via-registry"), out.content)

  test("registry advertises a bash schema with a required command"):
    val schema = ToolRegistry.of(Bash).schemas.find(_.name == "bash").get
    assertEquals(schema.parameters.properties("command").`type`, "string")
    assertEquals(schema.parameters.required, List("command"))
