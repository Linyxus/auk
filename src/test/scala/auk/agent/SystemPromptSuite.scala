package auk.agent

import gears.async.Async
import gears.async.default.given

import auk.generated.LibrarySource
import auk.runtime.repl.ReplPreamble

class SystemPromptSuite extends munit.FunSuite:

  test("the library interface source is embedded verbatim"):
    // The prompt's copy of the API must never drift from the generated source,
    // so compare against it rather than against phrases quoted from it.
    assert(SystemPrompt.default.contains(LibrarySource.interface))

  test("the session preamble is embedded verbatim"):
    assert(SystemPrompt.default.contains(ReplPreamble.Source))

  test("the editing example's newline escape survives the prompt's interpolator"):
    // The eval_scala section is an s-interpolator, and StringContext.s processes
    // escapes even inside triple quotes. So a bare \n in that source would reach
    // the model as a REAL line break inside a quoted string literal — invalid
    // Scala, in the one example teaching it how to write a patch payload.
    val p = SystemPrompt.default
    assert(p.contains("def start(port: Int): Unit =\\n    bind(port)"), "the example lost its literal \\n")
    assert(!p.contains("def start(port: Int): Unit =\n"), "the example's \\n became a real newline")

  test("the static default carries no dynamic (environment) sections"):
    // `default` is pure: it must not depend on the live environment. The titles
    // come from the section list itself, so renaming one cannot silently pass.
    val p = SystemPrompt.default
    SystemPrompt.dynamicSections.foreach(d => assert(!p.contains(s"## ${d.title}"), p))

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

  test("the workflow-agent prompt is composed of the eval_scala and typed-result sections"):
    val p = SystemPrompt.workflowAgent()
    assert(p.startsWith(SystemPrompt.WorkflowAgentIdentity), p)
    assert(p.contains("## Scala Code Execution"), p)
    assert(p.contains("## Producing your result"), p)
    // It does NOT teach orchestration: a sub-agent cannot launch its own workflow.
    assert(!p.contains("## Workflow Orchestration"), p)

  test("the team-member prompt names the member and is composed of the team sections"):
    val p = SystemPrompt.teamMember("tester", "runs the build and reports failures")
    // The id and role are interpolated into the identity.
    assert(p.contains("You are 'tester'"), p)
    assert(p.contains("runs the build and reports failures"), p)
    assert(p.contains("## Scala Code Execution"), p)
    assert(p.contains("## Working in the team"), p)
    // A member is neither a workflow worker nor an orchestrator.
    assert(!p.contains("## Producing your result"), p)
    assert(!p.contains("## Workflow Orchestration"), p)
    // Nor does it read the lead's delegation policy: that section is written to the
    // lead ("hand off", "your judgment"), and a member creates and retires nobody.
    assert(!p.contains("## Agent Team"), p)

  test("the MCP section appears in sub-agent prompts only when MCP servers are configured"):
    // Sub-agents receive the MCP tools when servers are configured, so their
    // prompt must carry the qualifying MCP section then — and must not otherwise.
    assert(!SystemPrompt.workflowAgent().contains("## MCP Tools"), "workflow prompt, unconfigured")
    assert(SystemPrompt.workflowAgent(mcpConfigured = true).contains("## MCP Tools"), "workflow prompt, configured")
    val member = SystemPrompt.teamMember("tester", "runs the build")
    assert(!member.contains("## MCP Tools"), "team prompt, unconfigured")
    assert(SystemPrompt.teamMember("tester", "runs the build", mcpConfigured = true).contains("## MCP Tools"), "team prompt, configured")

  test("the MCP setup section is always present for the interactive agent"):
    // Setting MCP up is most needed when nothing is configured yet, so unlike the
    // tools section this one must not be conditional.
    assert(SystemPrompt.default.contains("## MCP Servers"), "default prompt")
    Async.fromSync:
      val env = PromptEnv(
        workingDirectory = auk.TestFs.tempDir("auk-prompt-mcp"),
        modelName = "glm-5.2",
        today = "2026-06-14",
        process = SystemPromptSuite.NoGit
      )
      assert(SystemPrompt.build(env).contains("## MCP Servers"), "build, unconfigured")
      val configured = SystemPrompt.build(env, mcpConfigured = true)
      assert(configured.contains("## MCP Servers"), "build, configured")
      // With servers configured both MCP sections are present, setup first.
      assert(configured.contains("## MCP Tools"), "build, configured")
      assert(configured.indexOf("## MCP Servers") < configured.indexOf("## MCP Tools"), configured)

  test("the MCP setup section is withheld from sub-agent prompts"):
    // Editing the project's config is the interactive agent's concern.
    assert(!SystemPrompt.workflowAgent().contains("## MCP Servers"), "workflow, unconfigured")
    assert(!SystemPrompt.workflowAgent(mcpConfigured = true).contains("## MCP Servers"), "workflow, configured")
    assert(!SystemPrompt.teamMember("tester", "runs the build").contains("## MCP Servers"), "team, unconfigured")
    assert(
      !SystemPrompt.teamMember("tester", "runs the build", mcpConfigured = true).contains("## MCP Servers"),
      "team, configured"
    )

object SystemPromptSuite:
  import auk.platform.{Process, ProcessResult}

  /** A process seam that reports "not a git repository" for every command. */
  object NoGit extends Process:
    def runCaptured(argv: List[String], cwd: String, timeoutMs: Int, maxOutputBytes: Int)(using
        Async
    ): ProcessResult = ProcessResult("", 128, false, false)
