package auk.agent

import gears.async.Async
import gears.async.default.given

import auk.TestFs
import auk.platform.{Process, ProcessResult}

class DynamicSectionSuite extends munit.FunSuite:

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  private def envAt(dir: String, process: Process): PromptEnv =
    PromptEnv(workingDirectory = dir, modelName = "glm-5.2", today = "2026-06-14", process = process)

  // -- ProjectInfo -----------------------------------------------------------

  asyncTest("project info always reports the working directory, model and date"):
    val dir = TestFs.tempDir("auk-info")
    val body = DynamicSection.ProjectInfo.render(envAt(dir, FakeGit.notARepo)).getOrElse("")
    assert(body.contains(s"Working directory: $dir"), body)
    assert(body.contains("Model: glm-5.2"), body)
    assert(body.contains("Today's date: 2026-06-14"), body)

  asyncTest("project info omits git details outside a repository"):
    val body = DynamicSection.ProjectInfo.render(envAt("/tmp/x", FakeGit.notARepo)).getOrElse("")
    assert(!body.contains("Current branch"), body)
    assert(!body.contains("git status"), body)

  asyncTest("project info renders branch, status and recent commits inside a repo"):
    val body = DynamicSection.ProjectInfo.render(envAt("/tmp/x", FakeGit.repo)).getOrElse("")
    assert(body.contains("Current branch: main"), body)
    assert(body.contains("Status:\n M src/Main.scala"), body)
    assert(body.contains("Recent commits:\nabc123 Initial commit"), body)

  asyncTest("a clean working tree is reported as (clean)"):
    val body = DynamicSection.ProjectInfo.render(envAt("/tmp/x", FakeGit.cleanRepo)).getOrElse("")
    assert(body.contains("Status:\n(clean)"), body)

  // -- ProjectInstructions ---------------------------------------------------

  asyncTest("project instructions reads CLAUDE.md when present"):
    val dir = TestFs.tempDir("auk-instr")
    TestFs.write(TestFs.join(dir, "CLAUDE.md"), "# House rules\nAlways run the tests.")
    val body = DynamicSection.ProjectInstructions.render(envAt(dir, FakeGit.notARepo)).getOrElse("")
    assert(body.contains("`CLAUDE.md`"), body)
    assert(body.contains("Always run the tests."), body)

  asyncTest("project instructions prefers CLAUDE.md over AGENTS.md"):
    val dir = TestFs.tempDir("auk-instr")
    TestFs.write(TestFs.join(dir, "CLAUDE.md"), "from claude")
    TestFs.write(TestFs.join(dir, "AGENTS.md"), "from agents")
    val body = DynamicSection.ProjectInstructions.render(envAt(dir, FakeGit.notARepo)).getOrElse("")
    assert(body.contains("from claude"), body)
    assert(!body.contains("from agents"), body)
    assert(body.contains("`CLAUDE.md`"), body)

  asyncTest("project instructions falls back to AGENTS.md"):
    val dir = TestFs.tempDir("auk-instr")
    TestFs.write(TestFs.join(dir, "AGENTS.md"), "from agents")
    val body = DynamicSection.ProjectInstructions.render(envAt(dir, FakeGit.notARepo)).getOrElse("")
    assert(body.contains("`AGENTS.md`"), body)
    assert(body.contains("from agents"), body)

  asyncTest("project instructions is omitted when no file is present"):
    val dir = TestFs.tempDir("auk-instr")
    assertEquals(DynamicSection.ProjectInstructions.render(envAt(dir, FakeGit.notARepo)), None)

  asyncTest("an empty instructions file is omitted rather than rendered blank"):
    val dir = TestFs.tempDir("auk-instr")
    TestFs.write(TestFs.join(dir, "CLAUDE.md"), "   \n  ")
    assertEquals(DynamicSection.ProjectInstructions.render(envAt(dir, FakeGit.notARepo)), None)

/** A [[Process]] seam that answers the handful of git queries the prompt makes
  * from a fixed table, so the git-status rendering is tested without depending
  * on a real repository being present and configured. */
object FakeGit:
  import auk.platform.{Process, ProcessResult}

  private def of(responses: Map[List[String], String], inRepo: Boolean): Process =
    new Process:
      def runCaptured(argv: List[String], cwd: String, timeoutMs: Int, maxOutputBytes: Int)(using
          Async
      ): ProcessResult =
        val args = argv.drop(1) // strip the leading "git"
        if args == List("rev-parse", "--is-inside-work-tree") then
          if inRepo then ProcessResult("true\n", 0, false, false)
          else ProcessResult("fatal: not a git repository", 128, false, false)
        else
          responses.get(args) match
            case Some(out) => ProcessResult(out, 0, false, false)
            case None      => ProcessResult("", 1, false, false)

  val notARepo: Process = of(Map.empty, inRepo = false)

  val repo: Process = of(
    Map(
      List("rev-parse", "--abbrev-ref", "HEAD") -> "main\n",
      List("status", "--short") -> " M src/Main.scala\n",
      List("log", "--oneline", "-n", "5") -> "abc123 Initial commit\n"
    ),
    inRepo = true
  )

  val cleanRepo: Process = of(
    Map(
      List("rev-parse", "--abbrev-ref", "HEAD") -> "main\n",
      List("status", "--short") -> "",
      List("log", "--oneline", "-n", "5") -> "abc123 Initial commit\n"
    ),
    inRepo = true
  )
