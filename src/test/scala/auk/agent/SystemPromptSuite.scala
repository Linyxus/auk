package auk.agent

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
