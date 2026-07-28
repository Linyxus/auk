package auk.runtime.skills

import scala.concurrent.duration.{Duration, DurationInt}

import gears.async.Async
import gears.async.default.given

import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl

/** End-to-end tests of the transactional skill reload against the real REPL
  * worker: validate-in-fresh-session, swap-on-success, keep-the-elder-on-failure.
  *
  * Needs the vendored artifacts (`sbt vendorRepl`, or `AUK_REPL_DIR`); skipped
  * via `assume` otherwise. The tests tell one story in declaration order, and
  * one manager is shared throughout — the session state carried across tests is
  * part of what is under test.
  */
class SkillManagerSuite extends munit.FunSuite:

  // Each modification spawns a fresh worker and recompiles the set.
  override def munitTimeout: Duration = 180.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  private val root = s"target/skillmanager-test-${java.lang.System.currentTimeMillis()}"
  private val store = SkillStore(root)
  private val manager = SkillManager(store, () => ScalaRepl())

  override def afterAll(): Unit =
    Async.fromSync(manager.close())
    Platform.fs.removeAll(root)

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
      Async.fromSync(body)

  /** Evaluate on the manager's CURRENT session, returning (ok, rendered). */
  private def eval(code: String)(using Async): (Boolean, String) =
    manager.repl.eval(code, Some(60_000)) match
      case ScalaRepl.EvalResult(ScalaRepl.Status.Completed(r), _) =>
        (r.ok, r.stdout + r.output + r.error.getOrElse(""))
      case ScalaRepl.EvalResult(other, _) => (false, other.toString)

  private val greeter = Skill(
    "Greeter",
    "greets people",
    "object Greeter {\n  def greet(name: String): String = s\"hi, $name\"\n}",
    List("assert(Greeter.greet(\"x\") == \"hi, x\")")
  )

  asyncTest("initial load with an empty store leaves no skills"):
    manager.initialLoad()
    assertEquals(manager.skills, Nil)
    assert(manager.promptSection.contains("No skills are stored yet."), manager.promptSection)

  asyncTest("save validates the skill, persists it, and loads it into the session"):
    manager.save(greeter) match
      case Left(err) => fail(err)
      case Right(msg) =>
        assert(msg.contains("saved"), msg)
        assert(msg.contains("def greet(name: String): String"), msg)
    val (ok, out) = eval("Greeter.greet(\"auk\")")
    assert(ok, out)
    assert(out.contains("hi, auk"), out)
    assert(Platform.fs.isRegularFile(s"$root/Greeter/skill.scala"))
    assert(Platform.fs.isRegularFile(s"$root/Greeter/tests/1.scala"))

  asyncTest("a compile error rejects the save and keeps the elder session"):
    val (ok, _) = eval("val scratch = 41")
    assert(ok)
    val bad = Skill("Bad", "does not compile", "object Bad {\n  def f: Int = \"no\"\n}", Nil)
    manager.save(bad) match
      case Right(msg) => fail(s"expected a rejection, got: $msg")
      case Left(err)  => assert(err.contains("failed to compile"), err)
    // The elder session survived, scratch state intact.
    val (ok2, out2) = eval("scratch + 1")
    assert(ok2, out2)
    assert(out2.contains("42"), out2)
    assert(!Platform.fs.exists(s"$root/Bad"))

  asyncTest("a failing test rejects the save and keeps the elder session"):
    val failing = Skill(
      "Failing",
      "compiles but fails its test",
      "object Failing {\n  def n: Int = 1\n}",
      List("assert(Failing.n == 2, \"expected 2\")")
    )
    manager.save(failing) match
      case Right(msg) => fail(s"expected a rejection, got: $msg")
      case Left(err) =>
        assert(err.contains("tests failed"), err)
        assert(err.contains("expected 2"), err)
    val (ok, out) = eval("scratch + 1") // still the elder session
    assert(ok, out)
    assert(!Platform.fs.exists(s"$root/Failing"))

  asyncTest("a successful save swaps the session: scratch is gone, the set is loaded"):
    val doubler = Skill(
      "Doubler",
      "doubles numbers",
      "object Doubler {\n  def twice(x: Int): Int = x * 2\n}",
      List("assert(Doubler.twice(21) == 42)")
    )
    manager.save(doubler) match
      case Left(err) => fail(err)
      case Right(msg) => assert(msg.contains("session was replaced"), msg)
    val (scratchOk, _) = eval("scratch")
    assert(!scratchOk, "scratch survived the swap — the session was not replaced")
    val (ok, out) = eval("(Doubler.twice(2), Greeter.greet(\"still here\"))")
    assert(ok, out)
    assert(out.contains("hi, still here"), out)

  asyncTest("skills can build on each other; removing a dependency is rejected"):
    val louder = Skill(
      "Louder",
      "shouts a greeting",
      "object Louder {\n  def loud(name: String): String = Greeter.greet(name).toUpperCase\n}",
      List("assert(Louder.loud(\"a\") == \"HI, A\")")
    )
    manager.save(louder) match
      case Left(err) => fail(err)
      case Right(_)  => ()
    manager.remove("Greeter") match
      case Right(msg) => fail(s"expected the removal to be rejected, got: $msg")
      case Left(err) =>
        assert(err.contains("removing 'Greeter' breaks"), err)
    // Nothing changed: both still loaded, still on disk.
    val (ok, out) = eval("Louder.loud(\"b\")")
    assert(ok, out)
    assert(out.contains("HI, B"), out)
    assert(Platform.fs.exists(s"$root/Greeter"))

  asyncTest("removing a leaf skill succeeds and swaps"):
    manager.remove("Louder") match
      case Left(err) => fail(err)
      case Right(msg) => assert(msg.contains("removed"), msg)
    val (gone, _) = eval("Louder.loud(\"x\")")
    assert(!gone, "Louder is still in scope after its removal")
    val (ok, _) = eval("Greeter.greet(\"kept\")")
    assert(ok)
    assert(!Platform.fs.exists(s"$root/Louder"))

  asyncTest("reload picks up hand-written skills from disk"):
    val dir = s"$root/Handy"
    Platform.fs.createDirectories(s"$dir/tests")
    Platform.fs.writeString(
      s"$dir/skill.scala",
      "// description: hand-written on disk\nobject Handy {\n  def two: Int = 2\n}\n"
    )
    Platform.fs.writeString(s"$dir/tests/1.scala", "val expected = 2\nassert(Handy.two == expected)\n")
    manager.reloadFromDisk() match
      case Left(err) => fail(err)
      case Right(msg) => assert(msg.contains("Reloaded 3 skill(s)"), msg)
    val (ok, out) = eval("Handy.two + Doubler.twice(1)")
    assert(ok, out)
    assert(out.contains("4"), out)
    // The test snippet ran wrapped in `locally { … }`: its bindings never leak.
    val (leaked, _) = eval("expected")
    assert(!leaked, "a test binding leaked into the session")

  asyncTest("the prompt section lists every loaded skill with its interface"):
    val section = manager.promptSection
    assert(section.contains("### Doubler — doubles numbers"), section)
    assert(section.contains("def twice(x: Int): Int"), section)
    assert(section.contains("### Greeter"), section)
    assert(section.contains("### Handy"), section)

  asyncTest("initial load skips the tests: a set with a now-failing test still loads clean"):
    // A hand-edited store whose code compiles but whose test would fail. Initial
    // load is the startup fast path — one compile, no test runs (every persisted
    // set already passed them when stored) — so the skill must load, be callable,
    // and carry no ⚠ marker in the prompt report.
    val initRoot = s"$root-initial"
    val dir = s"$initRoot/Broken"
    Platform.fs.createDirectories(s"$dir/tests")
    Platform.fs.writeString(
      s"$dir/skill.scala",
      "// description: compiles fine, test would fail\nobject Broken {\n  def three: Int = 3\n}\n"
    )
    Platform.fs.writeString(s"$dir/tests/1.scala", "assert(Broken.three == 4, \"would fail\")\n")
    val fresh = SkillManager(SkillStore(initRoot), () => ScalaRepl())
    try
      fresh.initialLoad()
      assertEquals(fresh.skills.map(_.id), List("Broken"))
      assert(!fresh.promptSection.contains("⚠"), fresh.promptSection)
      fresh.repl.eval("Broken.three", Some(60_000)) match
        case ScalaRepl.EvalResult(ScalaRepl.Status.Completed(r), _) =>
          assert(r.ok && (r.stdout + r.output).contains("3"), r.toString)
        case other => fail(other.toString)
    finally
      fresh.close()
      Platform.fs.removeAll(initRoot)
