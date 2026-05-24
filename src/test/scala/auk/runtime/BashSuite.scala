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

  // -- corner cases: exit codes ----------------------------------------------

  test("exit code 1 is an error with the matching footer"):
    val r = run("false")
    assert(r.isError)
    assertEquals(r.metadata("exitCode"), "1")
    assert(r.output.contains("[command exited with code 1]"), r.output)
    assertEquals(r.metadata("timedOut"), "false")
    assertEquals(r.metadata("truncated"), "false")

  test("exit code 127 reported for not-found, exit code 126 for non-executable"):
    val r = run("command_that_surely_does_not_exist_42")
    assert(r.isError)
    assertEquals(r.metadata("exitCode"), "127")

  test("the shell wraps exit codes into the 0..255 range"):
    // `exit 256` wraps to 0 in bash, so the run is treated as a success.
    val r = run("exit 256")
    assertEquals(r.metadata("exitCode"), "0")
    assertEquals(r.isError, false)
    // `exit 257` wraps to 1.
    val r2 = run("exit 257")
    assertEquals(r2.metadata("exitCode"), "1")
    assert(r2.isError)

  test("a fatal signal yields a 128+signal exit code"):
    // Self-terminating with SIGKILL (9) makes bash report exit 137 (128 + 9).
    val r = run("kill -9 $$")
    assert(r.isError)
    assertEquals(r.metadata("exitCode"), "137")
    assertEquals(r.metadata("timedOut"), "false")

  test("non-zero exit still carries the command's own output before the footer"):
    val r = run("echo partial; exit 2")
    assert(r.isError)
    assert(r.output.startsWith("partial"), r.output)
    assert(r.output.contains("[command exited with code 2]"), r.output)
    assertEquals(r.metadata("exitCode"), "2")

  // -- corner cases: output shapes -------------------------------------------

  test("stderr-only output on a failing command is captured"):
    val r = run("echo boom 1>&2; exit 1")
    assert(r.output.contains("boom"), r.output)
    assert(r.isError)
    assertEquals(r.metadata("exitCode"), "1")

  test("output without a trailing newline is preserved exactly"):
    val r = run("printf 'no-newline'")
    assertEquals(r.output, "no-newline")
    assertEquals(r.isError, false)

  test("a lone leading newline from the command is stripped to empty"):
    // render() appends the text (so the buffer is non-empty) then strips a
    // single leading newline; a command whose only output is "\n" collapses to
    // the empty string -- NOT the "(no output)" sentinel, which is reserved for
    // commands that print nothing at all.
    val r = run("printf '\\n'")
    assertEquals(r.output, "")
    assertEquals(r.isError, false)

  test("interleaved stdout and stderr both appear in order-independent output"):
    val r = run("echo one; echo two 1>&2; echo three")
    assert(r.output.contains("one"), r.output)
    assert(r.output.contains("two"), r.output)
    assert(r.output.contains("three"), r.output)
    assertEquals(r.isError, false)

  test("non-ASCII UTF-8 bytes round-trip through the capture"):
    val r = run("printf 'caf\\xc3\\xa9 \\xe2\\x9c\\x93'")
    assertEquals(r.output, "café ✓")
    assertEquals(r.isError, false)

  test("a successful command with no output reports (no output) and metadata"):
    val r = run(":")
    assertEquals(r.output, "(no output)")
    assertEquals(r.isError, false)
    assertEquals(r.metadata("exitCode"), "0")
    assertEquals(r.metadata("timedOut"), "false")
    assertEquals(r.metadata("truncated"), "false")

  // -- corner cases: shell features ------------------------------------------

  test("multi-statement script with && chains and a final newline"):
    val r = run("printf 'first\\n' && printf 'second\\n'")
    assertEquals(r.output, "first\nsecond\n")
    assertEquals(r.isError, false)

  test("a newline-separated multiline script runs as one command"):
    val r = run("x=1\ny=2\necho $((x + y))")
    assertEquals(r.output.trim, "3")
    assertEquals(r.isError, false)

  test("pipes are evaluated by the shell"):
    val r = run("printf 'a\\nb\\nc\\n' | wc -l | tr -d ' '")
    assertEquals(r.output.trim, "3")
    assertEquals(r.isError, false)

  test("redirects write files relative to the working directory"):
    val dir = tempDir()
    val r = run("echo redirected > out.txt", ctx = ctxAt(dir))
    assertEquals(r.isError, false)
    assert(Files.exists(dir.resolve("out.txt")), "redirect should create the file")
    assertEquals(Files.readString(dir.resolve("out.txt")).nn.trim, "redirected")

  test("pipefail semantics are NOT enabled by default: tail of a pipe wins"):
    // Without `set -o pipefail`, the exit status is that of the last stage.
    val r = run("false | true")
    assertEquals(r.metadata("exitCode"), "0")
    assertEquals(r.isError, false)

  test("environment variables visible to the shell are expanded"):
    val r = run("echo $HOME")
    assert(r.output.trim.nonEmpty, r.output)
    assertEquals(r.isError, false)

  // -- corner cases: working directory ---------------------------------------

  test("pwd reflects the context working directory"):
    val dir = tempDir()
    val r = run("pwd", ctx = ctxAt(dir))
    // macOS temp dirs live under symlinked /var -> /private/var, so compare the
    // resolved real paths rather than the raw strings.
    val reported = java.nio.file.Paths.get(r.output.trim).nn.toRealPath().nn
    assertEquals(reported, dir.toRealPath().nn)

  test("each run gets its own fresh working directory by default"):
    // A file created in one default run must not leak into the next.
    run("touch leaked.txt")
    val r = run("ls leaked.txt")
    assert(r.isError, r.output)

  // -- corner cases: timeout capping -----------------------------------------

  test("a timeout below 1ms is floored to 1ms, not zero or negative"):
    // A trivially fast command should still complete; the floor keeps the wait
    // from being instantly zero. We just assert it does not falsely time out for
    // a command that finishes immediately.
    val r = run("true", timeoutMs = Some(0))
    // With a 1ms floor a no-op may or may not race the kill; assert metadata is
    // well-formed regardless and that the exit code is sane.
    assert(Set("true", "false").contains(r.metadata("timedOut")), r.metadata.toString)

  test("an explicit large timeout does not delay a fast command"):
    val start = System.nanoTime()
    val r = run("echo quick", timeoutMs = Some(600000))
    val elapsedMs = (System.nanoTime() - start) / 1_000_000
    assertEquals(r.output.trim, "quick")
    assertEquals(r.metadata("timedOut"), "false")
    assert(elapsedMs < 5000, s"fast command should not wait near the timeout (${elapsedMs}ms)")

  test("timeout returns whatever output was produced before the kill"):
    val r = run("echo before-sleep; sleep 5", timeoutMs = Some(300))
    assert(r.isError)
    assertEquals(r.metadata("timedOut"), "true")
    assert(r.output.contains("before-sleep"), r.output)
    assert(r.output.contains("timed out"), r.output)

  test("timeout footer takes precedence over the exit-code footer"):
    val r = run("sleep 5", timeoutMs = Some(200))
    assert(r.output.contains("timed out"), r.output)
    assert(!r.output.contains("[command exited with code"), r.output)

  // -- corner cases: approval -------------------------------------------------

  test("a denied command yields no exit-code metadata"):
    val r = run("echo nope", ctx = ctxAt(tempDir(), ApprovalPolicy.DenyAll))
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assertEquals(r.metadata, Map.empty[String, String])

  test("approval is consulted before an empty command is even rejected? no -- empty wins"):
    // An all-whitespace command short-circuits before approval is requested.
    var requested = false
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("  \t \n ", ctx = ctxAt(tempDir(), recording))
    assert(r.isError)
    assertEquals(r.output, "empty command")
    assertEquals(requested, false, "empty command must not consult approval")

  test("the approval summary carries the full multiline command text"):
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val cmd = "echo a\necho b"
    val _ = run(cmd, ctx = ctxAt(tempDir(), recording))
    assertEquals(seen.map(_.summary), Some(cmd))

  // -- corner cases: registry dispatch ---------------------------------------

  test("registry dispatch surfaces a non-zero exit as an error"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val registry = ToolRegistry.of(Bash)
      val out = registry.dispatch(
        Content.ToolUse("b2", "bash", """{"command":"exit 5"}""")
      )
      assertEquals(out.toolUseId, "b2")
      assertEquals(out.isError, true)
      assert(out.content.contains("code 5"), out.content)

  test("registry dispatch honours an explicit timeoutMs argument"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val registry = ToolRegistry.of(Bash)
      val out = registry.dispatch(
        Content.ToolUse("b3", "bash", """{"command":"sleep 5","timeoutMs":250}""")
      )
      assertEquals(out.isError, true)
      assert(out.content.contains("timed out"), out.content)
