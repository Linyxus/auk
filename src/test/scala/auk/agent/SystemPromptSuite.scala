package auk.agent

import auk.runtime.repl.ReplPreamble

class SystemPromptSuite extends munit.FunSuite:

  test("the default prompt opens with the identity"):
    assert(SystemPrompt.default.startsWith(SystemPrompt.Identity))

  test("the library interface source is embedded"):
    val p = SystemPrompt.default
    assert(p.contains("trait AukInterface"), p)
    assert(p.contains("def hello(name: String): String"), p)
    // The doc comments travel with the source — they are written for the model.
    assert(p.contains("operations auk exposes"), p)

  test("the session preamble is embedded verbatim"):
    assert(SystemPrompt.default.contains(ReplPreamble.Source))

  test("sections render as markdown headings"):
    assert(SystemPrompt.default.contains("## Scala evaluation"))
