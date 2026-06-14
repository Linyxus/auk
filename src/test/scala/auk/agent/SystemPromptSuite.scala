package auk.agent

import gears.async.Async
import gears.async.default.given

import auk.runtime.repl.ReplPreamble

class SystemPromptSuite extends munit.FunSuite:

  test("the default prompt opens with the identity"):
    assert(SystemPrompt.default.startsWith(SystemPrompt.Identity))

  test("the library interface source is embedded"):
    val p = SystemPrompt.default
    assert(p.contains("trait AukInterface"), p)
    assert(p.contains("def cwd: Path"), p)
    assert(p.contains("def Path(p: String): Path"), p)
    // The Path trait travels with it (both live in AukInterface.scala).
    assert(p.contains("def / (sub: String): Path"), p)
    // The doc comments travel with the source — they are written for the model.
    assert(p.contains("The runtime interface for Auk agents"), p)

  test("the session preamble is embedded verbatim"):
    assert(SystemPrompt.default.contains(ReplPreamble.Source))

  test("sections render as markdown headings"):
    assert(SystemPrompt.default.contains("## Scala evaluation"))

  test("the eval_scala section uses the real library API, not stale examples"):
    val p = SystemPrompt.default
    assert(p.contains("lib.fs"), p)
    assert(p.contains("there are no separate"), p)
    // `lib.hello` was a placeholder method that no longer exists.
    assert(!p.contains("lib.hello"), p)

  test("the static default carries no dynamic (environment) sections"):
    // `default` is pure: it must not depend on the live environment.
    val p = SystemPrompt.default
    assert(!p.contains("## Environment"), p)
    assert(!p.contains("## Project instructions"), p)

  test("build appends the dynamic sections after the static ones"):
    Async.fromSync:
      // A directory that is not a git repo and has no instructions file, so the
      // only dynamic section is the always-present environment one.
      val env = PromptEnv(
        workingDirectory = auk.TestFs.tempDir("auk-prompt"),
        modelName = "glm-5.2",
        today = "2026-06-14",
        process = SystemPromptSuite.NoGit
      )
      val p = SystemPrompt.build(env)
      // Static content still present, dynamic content follows it.
      assert(p.startsWith(SystemPrompt.Identity), p)
      assert(p.contains("## Scala evaluation"), p)
      assert(p.contains("## Environment"), p)
      assert(p.indexOf("## Scala evaluation") < p.indexOf("## Environment"), p)
      assert(p.contains("Model: glm-5.2"), p)
      assert(p.contains("Today's date: 2026-06-14"), p)

object SystemPromptSuite:
  import auk.platform.{Process, ProcessResult}

  /** A process seam that reports "not a git repository" for every command. */
  object NoGit extends Process:
    def runCaptured(argv: List[String], cwd: String, timeoutMs: Int, maxOutputBytes: Int)(using
        Async
    ): ProcessResult = ProcessResult("", 128, false, false)
