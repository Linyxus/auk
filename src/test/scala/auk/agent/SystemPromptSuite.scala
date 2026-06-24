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
    assert(p.contains("def cwd: FsDir"), p)
    assert(p.contains("def path(p: String): Path"), p)
    // The Path trait travels with it (both live in AukInterface.scala).
    assert(p.contains("def / (sub: String): Path"), p)
    // The doc comments travel with the source — they are written for the model.
    assert(p.contains("The runtime interface for Auk agents"), p)

  test("the session preamble is embedded verbatim"):
    assert(SystemPrompt.default.contains(ReplPreamble.Source))

  test("sections render as markdown headings"):
    assert(SystemPrompt.default.contains("## Scala Code Execution"))

  test("the workflow section warns that Agent.all is a barrier, with a pipeline example"):
    val p = SystemPrompt.default
    assert(p.contains("## Workflow Orchestration"), p)
    assert(p.contains("BARRIER"), p)
    assert(p.contains("per-item"), p)
    // the concrete writer -> its-own-editor pipeline is shown (not a global join)
    assert(p.contains("writer-$t"), p)
    assert(p.contains("editor-$t"), p)

  test("the workflow section documents the non-blocking WorkflowRun API"):
    val p = SystemPrompt.default
    // `wf.start` is non-blocking now: it returns a handle, not the result.
    assert(p.contains("WorkflowRun"), p)
    // The handle is polled via `status` (a `WorkflowStatus`), which models Paused.
    assert(p.contains("run.status"), p)
    assert(p.contains("WorkflowStatus"), p)
    assert(p.contains("Paused"), p)
    assert(p.contains("getResult"), p)
    // the old blocking contract must be gone (distinctive phrases from it)
    assert(!p.contains("comes back as this eval_scala call's OUTPUT"), p)
    assert(!p.contains("do not bind it to a val"), p)

  test("the workflow section explains that the DSL needs wf.start's implicit context"):
    val p = SystemPrompt.default
    assert(p.contains("implicit context that every workflow operation"), p)
    // It warns that the DSL only resolves inside the wf.start block.
    assert(p.contains("will not compile"), p)

  test("the eval_scala section tells the agent to println long values"):
    val p = SystemPrompt.default
    // The REPL clips a long echoed value; the fix is to print it in full.
    assert(p.contains("println"), p)
    assert(p.contains("complete contents of a long"), p)

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
      assert(p.contains("## Scala Code Execution"), p)
      assert(p.contains("## Environment"), p)
      assert(p.indexOf("## Scala Code Execution") < p.indexOf("## Environment"), p)
      assert(p.contains("Model: glm-5.2"), p)
      assert(p.contains("Today's date: 2026-06-14"), p)

  // -- sub-agent prompts -------------------------------------------------------

  test("the workflow-agent prompt teaches eval_scala and the submit_result contract"):
    val p = SystemPrompt.workflowAgent
    assert(p.startsWith(SystemPrompt.WorkflowAgentIdentity), p)
    // It carries the shared eval_scala action surface (with the live library API).
    assert(p.contains("## Scala Code Execution"), p)
    assert(p.contains("trait AukInterface"), p)
    assert(p.contains("lib.fs"), p)
    // It states the typed-result contract.
    assert(p.contains("## Producing your result"), p)
    assert(p.contains("submit_result"), p)
    assert(p.contains("exactly once"), p)
    // It does NOT teach how to orchestrate workflows (sub-agents cannot recurse).
    assert(!p.contains("## Workflow Orchestration"), p)
    assert(p.contains("no nested `wf.start`"), p)

  test("the plain sub-agent prompt teaches eval_scala and reports in prose"):
    val p = SystemPrompt.subAgent
    assert(p.startsWith(SystemPrompt.SubAgentIdentity), p)
    assert(p.contains("## Scala Code Execution"), p)
    assert(p.contains("lib.fs"), p)
    // It reports back in prose — no submit_result, no workflow orchestration.
    assert(p.contains("reply with a concise report"), p)
    assert(!p.contains("submit_result"), p)
    assert(!p.contains("## Workflow Orchestration"), p)

object SystemPromptSuite:
  import auk.platform.{Process, ProcessResult}

  /** A process seam that reports "not a git repository" for every command. */
  object NoGit extends Process:
    def runCaptured(argv: List[String], cwd: String, timeoutMs: Int, maxOutputBytes: Int)(using
        Async
    ): ProcessResult = ProcessResult("", 128, false, false)
