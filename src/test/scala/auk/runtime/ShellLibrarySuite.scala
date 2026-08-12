package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}

import gears.async.Async
import gears.async.default.given

import auk.platform.js.ReplArtifacts
import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** End-to-end tests for the shell API of the auk runtime library (`lib.shell`),
  * exercised the way the agent uses it: as Scala evaluated in a real REPL worker
  * with the packed `library.bin` preloaded and `lib` bound by the preamble.
  *
  * These transport the behavioural coverage of the old `bash` tool (output
  * capture, exit codes, stdin EOF, timeout, working directory) onto `lib.shell`,
  * and add the program-validation that the tool never had. Tool-plumbing
  * (approval, registry dispatch, schema) is not retested here — that now belongs
  * to `eval_scala` (see `EvalScalaSuite`).
  *
  * Each snippet ends in a `Boolean` so the assertion is a simple `Boolean = true`
  * check (the REPL truncates long rendered values). Skipped via `assume` when the
  * REPL artifacts are absent (`./mill vendorRepl + packLibraryBin`).
  */
class ShellLibrarySuite extends munit.FunSuite:

  override def munitTimeout: Duration = 120.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight
  private val repl = ScalaRepl()

  override def beforeAll(): Unit =
    if artifactsAvailable then Async.fromSync(setup())

  override def afterAll(): Unit =
    Async.fromSync:
      if artifactsAvailable then
        try { val _ = repl.eval("base.delete()", Some(30_000)) }
        catch { case _: Throwable => () }
      repl.close()

  /** Create the shared temp directory and bind it as the REPL-session val
    * `base`, used by every snippet below. */
  private def setup()(using Async): Unit =
    val r = completed(
      """val osMod = scala.scalajs.js.Dynamic.global.require("node:os")
        |val base = lib.path(osMod.tmpdir().asInstanceOf[String]) / ("auk-shtest-" + System.nanoTime())
        |base.openAsDir.makedir()""".stripMargin
    )
    assert(r.ok, s"shell test setup failed: ${r.error.getOrElse(r.output)}")

  private def completed(code: String)(using Async): ReplProtocol.Response =
    repl.eval(code, Some(30_000)).status match
      case ScalaRepl.Status.Completed(r) => r
      case other                         => fail(s"unexpected REPL status: $other")

  /** Register a test whose snippet must evaluate to `true`. */
  private def check(name: String)(code: String): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `./mill vendorRepl + packLibraryBin`")
      Async.fromSync:
        val r = completed(code)
        assert(r.ok, s"eval errored: ${r.error.getOrElse(r.output)}")
        assert(r.output.contains("Boolean = true"), s"expected `Boolean = true`, got: ${r.output}")

  /** Register a test whose snippet must fail with a message containing `substr`. */
  private def checkError(name: String, substr: String)(code: String): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `./mill vendorRepl + packLibraryBin`")
      Async.fromSync:
        val r = completed(code)
        assert(!r.ok, s"expected an error, got: ${r.output}")
        val msg = r.error.getOrElse("") + r.output + r.stderr
        assert(msg.toLowerCase.contains(substr.toLowerCase), s"expected '$substr' in: $msg")

  // -- run: output capture & exit codes --------------------------------------

  check("run captures stdout and reports a clean exit"):
    """{ val r = lib.shell.run("echo", "hello"); r.stdout.trim == "hello" && r.exitCode == 0 && r.ok }"""

  check("a non-zero exit is reported, not thrown"):
    """{ val r = lib.shell.run("false"); r.exitCode != 0 && !r.ok }"""

  check("arguments are literal — no shell word splitting"):
    """{ lib.shell.run("printf", "%s", "a b").stdout == "a b" }"""

  check("multi-line stdout is preserved verbatim"):
    """{ lib.shell.run("printf", "one\ntwo\n").stdout == "one\ntwo\n" }"""

  check("non-ASCII UTF-8 round-trips"):
    """{ lib.shell.run("printf", "café ✓").stdout == "café ✓" }"""

  check("a program that is not found reports exit 127"):
    """{ lib.shell.run("definitely-not-a-real-program-xyz").exitCode == 127 }"""

  check("a reusable command handle runs repeatedly"):
    """{ val e = lib.shell.command("echo"); e.execute("a").stdout.trim == "a" && e.execute("b").stdout.trim == "b" }"""

  check("stderr is captured separately from stdout on a failing command"):
    """{ val r = lib.shell.run("wc", "/no-such-file-xyz"); r.exitCode != 0 && r.stdout == "" && r.stderr.nonEmpty }"""

  // -- stdin is closed -------------------------------------------------------

  check("stdin is closed so a reader sees EOF instead of hanging"):
    """{ val r = lib.shell.run("wc", "-c"); r.ok && r.stdout.trim == "0" }"""

  // -- working directory -----------------------------------------------------

  check("at runs in the given (absolute) directory"):
    """{ val d = (base / "atabs").openAsDir; d.makedir()
      |  lib.shell.at(d.path).run("pwd").stdout.trim.endsWith("/atabs") }""".stripMargin

  check("at resolves a relative dir against the shell's cwd"):
    """{ val d = (base / "atrel").openAsDir; d.makedir()
      |  lib.shell.at(base).at(lib.path("atrel")).run("pwd").stdout.trim.endsWith("/atrel") }""".stripMargin

  // -- timeout ---------------------------------------------------------------

  check("a command exceeding the timeout is killed and flagged"):
    """{ val r = lib.shell.withTimeout(300).run("sleep", "5"); r.timedOut && !r.ok }"""

  // -- program validation (the denylist) -------------------------------------

  check("every file-op program in the denylist is rejected"):
    """{ List("rm","ls","mkdir","mv","cat","touch").forall { n =>
      |    try { lib.shell.command(n); false } catch { case _: Throwable => true } } }""".stripMargin

  check("an absolute path to a forbidden program is also rejected"):
    """{ try { lib.shell.command("/bin/rm"); false } catch { case _: Throwable => true } }"""

  check("run enforces the denylist too"):
    """{ try { lib.shell.run("ls", "-la"); false } catch { case _: Throwable => true } }"""

  check("a permitted program is not rejected"):
    """{ lib.shell.command("echo"); lib.shell.command("wc"); true }"""

  checkError("the rejection points to the file-system interface", "lib.fs"):
    """lib.shell.command("rm")"""

  // -- CommandResult conveniences --------------------------------------------

  check("output is stderr when stdout is empty"):
    """{ val r = lib.shell.run("wc", "/no-such-file-xyz"); r.output == r.stderr && r.stdout == "" }"""

  check("toString renders the output with an exit footer"):
    """{ val s = lib.shell.run("echo", "hi").toString; s.contains("hi") && s.contains("[exit 0]") }"""
