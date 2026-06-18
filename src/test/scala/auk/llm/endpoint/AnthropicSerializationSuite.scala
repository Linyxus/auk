package auk.llm.endpoint

import scala.scalajs.js

/** How `AnthropicEndpoint.buildParams` re-serializes a stored assistant turn for
  * replay — the contract that keeps extended-thinking + tool-use conversations
  * valid. Verified live (see scratch/anthropic-thinking-probe.mjs): a thinking
  * block must be replayed with its `signature`; an unsigned one must be dropped
  * (sending it unsigned is a 400); a redacted block must be passed back verbatim.
  */
class AnthropicSerializationSuite extends munit.FunSuite:

  // The lazy SDK client is never touched by buildParams, so no key/network.
  private val ep = AnthropicEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "test-key"))

  /** The serialized content blocks of a single assistant message. */
  private def assistantBlocks(content: List[Content]): List[js.Dynamic] =
    val params = ep.buildParams(List(Message(Role.Assistant, content)), LLMConfig(model = "m"), stream = false)
    val msgs = params.asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]]
    msgs(0).content.asInstanceOf[js.Array[js.Dynamic]].toList

  private def kinds(blocks: List[js.Dynamic]): List[String] =
    blocks.map(b => Dyn.str(b.`type`).getOrElse(""))

  test("a signed thinking block in a tool-use turn replays as a thinking block with its signature, in order"):
    val blocks = assistantBlocks(List(
      Content.Thinking("reasoning", Some("sig-abc")),
      Content.Text("the answer"),
      Content.ToolUse("t1", "read", """{"path":"a.txt"}""")
    ))
    assertEquals(kinds(blocks), List("thinking", "text", "tool_use"))
    assertEquals(Dyn.str(blocks(0).thinking), Some("reasoning"))
    assertEquals(Dyn.str(blocks(0).signature), Some("sig-abc"))

  test("an unsigned thinking block is dropped on replay (an unsigned block is rejected)"):
    val blocks = assistantBlocks(List(
      Content.Thinking("reasoning", None),
      Content.ToolUse("t1", "read", "{}")
    ))
    assertEquals(kinds(blocks), List("tool_use"))

  test("an OpenRouter reasoning block is dropped on Anthropic replay (never mis-serialized as text)"):
    // Cross-provider after a model switch: OpenRouter-shaped reasoning is not an
    // Anthropic block, so it must be dropped — not stringified by the text fallback.
    val blocks = assistantBlocks(List(
      Content.Reasoning(List(ReasoningBlock("reasoning.text", text = Some("ponder"), signature = Some("sig")))),
      Content.ToolUse("t1", "read", "{}")
    ))
    assertEquals(kinds(blocks), List("tool_use"))

  test("a redacted thinking block replays verbatim as redacted_thinking"):
    val blocks = assistantBlocks(List(
      Content.RedactedThinking("enc-data"),
      Content.ToolUse("t1", "read", "{}")
    ))
    assertEquals(kinds(blocks), List("redacted_thinking", "tool_use"))
    assertEquals(Dyn.str(blocks(0).data), Some("enc-data"))

  test("an assistant turn with no tool call sends plain text and never leaks reasoning"):
    // The non-tool branch serializes msg.text only, so thinking is dropped (the
    // API neither needs nor accepts unsigned reasoning outside tool use).
    val params = ep.buildParams(
      List(Message(Role.Assistant, List(Content.Thinking("secret reasoning", Some("sig")), Content.Text("hello")))),
      LLMConfig(model = "m"),
      stream = false
    )
    val msg = params.asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]](0)
    assertEquals(Dyn.str(msg.content), Some("hello"))

  // -- thinking / effort request params ------------------------------------------

  private def reqParams(thinking: ThinkingMode): js.Dynamic =
    ep.buildParams(List(Message.user("hi")), LLMConfig(model = "m", thinking = Some(thinking)), stream = false)
      .asInstanceOf[js.Dynamic]

  test("Auto thinking requests adaptive thinking and no effort"):
    val p = reqParams(ThinkingMode.Auto)
    assertEquals(Dyn.str(p.thinking.`type`), Some("adaptive"))
    assert(js.isUndefined(p.output_config), "Auto must not set output_config")

  test("Effort requests adaptive thinking plus output_config.effort"):
    // Effort alone returns no thinking block (verified live), so the model must
    // run adaptive thinking to produce signed, replayable reasoning.
    val p = reqParams(ThinkingMode.Effort(EffortLevel.Max))
    assertEquals(Dyn.str(p.thinking.`type`), Some("adaptive"))
    assertEquals(Dyn.str(p.output_config.effort), Some("max"))

  test("each effort level maps to its Anthropic string"):
    val cases = List(
      EffortLevel.Low -> "low",
      EffortLevel.Medium -> "medium",
      EffortLevel.High -> "high",
      EffortLevel.XHigh -> "xhigh",
      EffortLevel.Max -> "max"
    )
    for (level, str) <- cases do
      assertEquals(Dyn.str(reqParams(ThinkingMode.Effort(level)).output_config.effort), Some(str), s"$level")
