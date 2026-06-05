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
