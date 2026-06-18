package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl

/** End-to-end tests against the real REPL worker process.
  *
  * They need the vendored artifacts (`sbt vendorRepl`, or `AUK_REPL_DIR`) and
  * are skipped via `assume` when those are absent. One worker is shared by the
  * whole suite — REPL state accumulating across tests is part of what is under
  * test — except where a test kills it on purpose.
  */
class EvalScalaSuite extends munit.FunSuite:

  // The first eval pays worker startup plus a JS-hosted compile.
  override def munitTimeout: Duration = 120.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  private val repl = ScalaRepl()
  private val tool = EvalScala(repl)

  override def afterAll(): Unit =
    Async.fromSync(repl.close())

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
      Async.fromSync(body)

  private def run(
      code: String,
      timeoutMs: Option[Int] = None,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using Async): ToolResult =
    given RuntimeContext = RuntimeContext(Platform.cwd(), approvals)
    tool.execute(EvalScalaParams(code, timeoutMs))

  // -- evaluation --------------------------------------------------------------

  asyncTest("evaluates an expression and renders its value"):
    val r = run("21 * 2")
    assert(!r.isError, r.output)
    assert(r.output.contains("42"), r.output)
    assertEquals(r.metadata("timedOut"), "false")

  asyncTest("definitions persist across calls"):
    val r1 = run("val seed = 19")
    assert(!r1.isError, r1.output)
    val r2 = run("seed + 23")
    assert(!r2.isError, r2.output)
    assert(r2.output.contains("42"), r2.output)

  asyncTest("multi-statement blocks evaluate as one entry"):
    val r = run("val a = 10\nval b = 20\na + b")
    assert(!r.isError, r.output)
    assert(r.output.contains("30"), r.output)

  asyncTest("println output is captured and returned"):
    val r = run("""println("hello-from-eval-scala")""")
    assert(!r.isError, r.output)
    assert(r.output.contains("hello-from-eval-scala"), r.output)

  asyncTest("the state version grows with each successful eval"):
    val r1 = run("val v1 = 1")
    val r2 = run("val v2 = 2")
    assert(!r1.isError && !r2.isError)
    assert(
      r2.metadata("stateVersion").toInt > r1.metadata("stateVersion").toInt,
      s"${r1.metadata} -> ${r2.metadata}"
    )

  // -- failures ----------------------------------------------------------------

  asyncTest("a compile error is an error result carrying the diagnostic"):
    val r = run("definitely not scala ((")
    assert(r.isError)
    assert(r.output.nonEmpty)
    // Diagnostics are de-colourised for the model.
    assert(!r.output.contains("\u001b["), r.output)

  asyncTest("the session survives a failed line"):
    val bad = run("nope nope nope")
    assert(bad.isError)
    val r = run("1 + 1")
    assert(!r.isError, r.output)
    assertEquals(r.metadata("restarted"), "false")

  // -- JS interop ----------------------------------------------------------------

  asyncTest("Node APIs are reachable through the injected global require"):
    val r = run(
      """val os = scala.scalajs.js.Dynamic.global.require("node:os")
        |println("platform=" + os.platform())""".stripMargin
    )
    assert(!r.isError, r.output)
    assert(r.output.contains("platform="), r.output)

  // -- preloaded runtime library & preamble ----------------------------------------

  asyncTest("the preamble binds the runtime library as `lib`"):
    val r = run("""lib.path("/tmp/x")""")
    assert(!r.isError, r.output)
    assert(r.output.contains("/tmp/x"), r.output)

  asyncTest("the preamble import puts the library names in scope unqualified"):
    // `import auk.library.*` brings AukInterface, AukImpl and Path into scope.
    val r = run("val mine: AukInterface = new AukImpl()\nval p: Path = mine.path(\"/a\")\np")
    assert(!r.isError, r.output)
    assert(r.output.contains("/a"), r.output)

  asyncTest("lib.fs.cwd reaches Node through JS interop"):
    val r = run("lib.fs.cwd")
    assert(!r.isError, r.output)
    assert(r.output.contains("/"), r.output)

  // -- runtime library: Path -------------------------------------------------------

  asyncTest("a Path renders as its path string"):
    val r = run("""lib.path("/foo/bar")""")
    assert(!r.isError, r.output)
    assert(r.output.contains("Path = /foo/bar"), r.output)

  asyncTest("`/` joins segments onto a path"):
    val r = run("""lib.path("/foo") / "bar" / "baz"""")
    assert(!r.isError, r.output)
    assert(r.output.contains("/foo/bar/baz"), r.output)

  asyncTest("`/` normalizes `.` and `..` segments away"):
    val r = run("""lib.path("/foo/bar") / ".." / "qux"""")
    assert(!r.isError, r.output)
    assert(r.output.contains("/foo/qux"), r.output)
    assert(!r.output.contains(".."), r.output)

  asyncTest("paths compare by value"):
    val r = run("""(lib.path("/a/b") == lib.path("/a/b"), lib.path("/a") == lib.path("/b"))""")
    assert(!r.isError, r.output)
    assert(r.output.contains("(true, false)"), r.output)

  asyncTest("cwd is a directory whose path composes with `/`"):
    val r = run("""(lib.fs.cwd.path / "src" / "main").toString""")
    assert(!r.isError, r.output)
    assert(r.output.contains("/src/main"), r.output)

  asyncTest("a broken preamble fails distinctly, not as the user's code"):
    val broken = ScalaRepl(preamble = "definitely not scala ((")
    try
      val result = broken.eval("1 + 1", Some(30_000))
      result.status match
        case ScalaRepl.Status.Failed(reason) => assert(reason.contains("preamble"), reason)
        case other => fail(s"expected a preamble failure, got $other")
    finally broken.close()

  // -- timeout & restart -----------------------------------------------------------

  asyncTest("a timed-out eval kills the session; the next call starts fresh"):
    val r = run("while true do ()", timeoutMs = Some(1500))
    assert(r.isError)
    assertEquals(r.metadata("timedOut"), "true")
    assert(r.output.contains("timed out"), r.output)

    val r2 = run("1 + 1") // pays a fresh worker spawn
    assert(!r2.isError, r2.output)
    assertEquals(r2.metadata("restarted"), "true")
    assert(r2.output.contains("restarted"), r2.output)

  asyncTest("the preamble is restored on the restarted session"):
    // Runs on the worker respawned by the previous test: user definitions are
    // gone, but `lib` must be back.
    val r = run("""lib.path("/restored")""")
    assert(!r.isError, r.output)
    assert(r.output.contains("/restored"), r.output)

  // -- guards ------------------------------------------------------------------

  asyncTest("denied approval blocks evaluation"):
    val r = run("1 + 1", approvals = ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)

  asyncTest("the approval request carries the full code as its summary"):
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val code = "val approved = true"
    val _ = run(code, approvals = recording)
    assertEquals(seen.map(_.toolName), Some("eval_scala"))
    assertEquals(seen.map(_.summary), Some(code))

  asyncTest("empty code is rejected without consulting the worker"):
    val r = run("   \n ")
    assert(r.isError)
    assertEquals(r.output, "empty code")
