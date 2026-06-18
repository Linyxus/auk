package auk.llm.endpoint

import scala.scalajs.js

/** Capturing and replaying OpenRouter `reasoning_details` so the model's reasoning
  * chain (signatures included) survives across tool calls. All synthetic — the
  * helpers parse/serialize `js.Dynamic` literals directly, so no SDK or network. */
class OpenAICompletionReasoningDetailsSuite extends munit.FunSuite:

  // The lazy SDK client is never touched by buildParams/helpers, so no key/network.
  private val ep = OpenAICompletionEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "test-key"))

  /** A wire reasoning_details block. `js.Dictionary[Any]` mirrors how the endpoint
    * itself builds wire objects, and tolerates the mixed String/Int values. */
  private def block(pairs: (String, Any)*): js.Dynamic =
    js.Dictionary[Any](pairs*).asInstanceOf[js.Dynamic]

  /** A message/delta carrying a `reasoning_details` array. */
  private def withDetails(blocks: js.Dynamic*): js.Dynamic =
    js.Dynamic.literal(reasoning_details = js.Array[js.Any](blocks*))

  private def accum() = scala.collection.mutable.Map.empty[Int, OpenAICompletionEndpoint.ReasoningAccum]
  private def merge(a: scala.collection.mutable.Map[Int, OpenAICompletionEndpoint.ReasoningAccum], rd: js.Dynamic): Unit =
    OpenAICompletionEndpoint.mergeReasoningDelta(a, rd)
  private def finalizeR(a: scala.collection.mutable.Map[Int, OpenAICompletionEndpoint.ReasoningAccum]): List[ReasoningBlock] =
    OpenAICompletionEndpoint.finalizeReasoning(a)

  // -- non-stream parse (reasoningBlocks) --------------------------------------

  test("parses a signed reasoning.text block with all fields"):
    val blocks = OpenAICompletionEndpoint.reasoningBlocks(withDetails(
      block("type" -> "reasoning.text", "text" -> "ponder", "signature" -> "sig-1",
        "format" -> "anthropic-claude-v1", "id" -> "r1", "index" -> 0)
    ))
    assertEquals(blocks, List(ReasoningBlock(
      "reasoning.text", text = Some("ponder"), signature = Some("sig-1"),
      format = Some("anthropic-claude-v1"), id = Some("r1"), index = Some(0))))

  test("parses encrypted and summary blocks and keeps array order"):
    val blocks = OpenAICompletionEndpoint.reasoningBlocks(withDetails(
      block("type" -> "reasoning.summary", "summary" -> "the gist"),
      block("type" -> "reasoning.encrypted", "data" -> "BLOB")
    ))
    assertEquals(blocks.map(_.`type`), List("reasoning.summary", "reasoning.encrypted"))
    assertEquals(blocks(0).summary, Some("the gist"))
    assertEquals(blocks(1).data, Some("BLOB"))
    assertEquals(blocks(1).text, None)

  test("an unknown block type is preserved verbatim (forward-compat)"):
    val blocks = OpenAICompletionEndpoint.reasoningBlocks(withDetails(
      block("type" -> "reasoning.future", "text" -> "x")))
    assertEquals(blocks.head.`type`, "reasoning.future")

  test("a content-less block (bare signature, no text/data) is dropped"):
    assertEquals(OpenAICompletionEndpoint.reasoningBlocks(withDetails(
      block("type" -> "reasoning.text", "signature" -> "sig-only"))), Nil)

  test("no reasoning_details yields no blocks (so the flat-string path stays Thinking)"):
    assertEquals(OpenAICompletionEndpoint.reasoningBlocks(js.Dynamic.literal(reasoning = "flat only")), Nil)

  // -- stream accumulation (mergeReasoningDelta / finalizeReasoning) ------------

  test("a signature arriving in a later delta is captured with the text"):
    val a = accum()
    merge(a, block("type" -> "reasoning.text", "text" -> "abc", "index" -> 0))
    merge(a, block("signature" -> "sig", "index" -> 0)) // signature streams separately
    val blocks = finalizeR(a)
    assertEquals(blocks.size, 1)
    assertEquals(blocks.head.text, Some("abc"))
    assertEquals(blocks.head.signature, Some("sig"))

  test("text split across deltas is concatenated in order"):
    val a = accum()
    merge(a, block("type" -> "reasoning.text", "text" -> "Hel", "index" -> 0))
    merge(a, block("text" -> "lo", "index" -> 0))
    assertEquals(finalizeR(a).head.text, Some("Hello"))

  test("two blocks interleaved by index finalize sorted by index"):
    val a = accum()
    merge(a, block("type" -> "reasoning.text", "text" -> "second", "index" -> 1))
    merge(a, block("type" -> "reasoning.text", "text" -> "first", "index" -> 0))
    assertEquals(finalizeR(a).map(_.text), List(Some("first"), Some("second")))

  test("deltas with no index fold into a single block"):
    val a = accum()
    merge(a, block("type" -> "reasoning.text", "text" -> "a"))
    merge(a, block("text" -> "b"))
    assertEquals(finalizeR(a).map(_.text), List(Some("ab")))

  test("an encrypted block accumulated via deltas captures its data"):
    val a = accum()
    merge(a, block("type" -> "reasoning.encrypted", "data" -> "BLOB", "index" -> 2))
    assertEquals(finalizeR(a).head.data, Some("BLOB"))

  test("a summary accumulated via deltas is concatenated"):
    val a = accum()
    merge(a, block("type" -> "reasoning.summary", "summary" -> "the ", "index" -> 0))
    merge(a, block("summary" -> "gist", "index" -> 0))
    assertEquals(finalizeR(a).head.summary, Some("the gist"))

  // -- serialize (serializeReasoning) ------------------------------------------

  test("serializeReasoning emits only the fields a block carries"):
    val arr = OpenAICompletionEndpoint.serializeReasoning(List(
      ReasoningBlock("reasoning.text", text = Some("t"), signature = Some("s"), format = Some("f"), index = Some(0))))
    val b = arr(0).asInstanceOf[js.Dynamic]
    assertEquals(Dyn.str(b.`type`), Some("reasoning.text"))
    assertEquals(Dyn.str(b.text), Some("t"))
    assertEquals(Dyn.str(b.signature), Some("s"))
    assertEquals(Dyn.str(b.format), Some("f"))
    assertEquals(Dyn.num(b.index), Some(0.0))
    assert(js.isUndefined(b.data), "absent data must be omitted")
    assert(js.isUndefined(b.summary), "absent summary must be omitted")
    assert(js.isUndefined(b.id), "absent id must be omitted")

  test("reasoning_details round-trips parse -> serialize -> parse unchanged"):
    val parsed = OpenAICompletionEndpoint.reasoningBlocks(withDetails(
      block("type" -> "reasoning.text", "text" -> "ponder", "signature" -> "sig",
        "format" -> "anthropic-claude-v1", "id" -> "r1", "index" -> 0),
      block("type" -> "reasoning.encrypted", "data" -> "BLOB", "index" -> 1)
    ))
    val reParsed = OpenAICompletionEndpoint.reasoningBlocks(
      js.Dynamic.literal(reasoning_details = OpenAICompletionEndpoint.serializeReasoning(parsed)))
    assertEquals(reParsed, parsed)

  // -- buildParams (WRITE) -----------------------------------------------------

  private def assistantMsg(content: List[Content]): js.Dynamic =
    ep.buildParams(List(Message(Role.Assistant, content)), LLMConfig(model = "m"), stream = false)
      .asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]](0)

  test("buildParams attaches reasoning_details alongside tool_calls"):
    val msg = assistantMsg(List(
      Content.Reasoning(List(ReasoningBlock("reasoning.text", text = Some("why"), signature = Some("sig")))),
      Content.ToolUse("c1", "read", "{}")
    ))
    assert(!js.isUndefined(msg.tool_calls), "tool_calls must be present")
    val details = Dyn.arr(msg.reasoning_details)
    assertEquals(details.size, 1)
    assertEquals(Dyn.str(details.head.signature), Some("sig"))

  test("buildParams attaches reasoning_details on a plain (no tool) assistant turn"):
    val msg = assistantMsg(List(
      Content.Reasoning(List(ReasoningBlock("reasoning.text", text = Some("x")))),
      Content.Text("hi")))
    assertEquals(Dyn.str(msg.content), Some("hi"))
    assertEquals(Dyn.arr(msg.reasoning_details).size, 1)

  test("an assistant turn with no reasoning has no reasoning_details key"):
    val msg = assistantMsg(List(Content.Text("hi")))
    assert(js.isUndefined(msg.reasoning_details), "reasoning_details must be omitted when absent")

  // -- steering: text coalesced after tool results must reach the wire ----------

  private def userMessages(content: List[Content]): List[js.Dynamic] =
    ep.buildParams(List(Message(Role.User, content)), LLMConfig(model = "m"), stream = false)
      .asInstanceOf[js.Dynamic].messages.asInstanceOf[js.Array[js.Dynamic]].toList

  test("a steer coalesced after a tool result is sent as its own user turn (not dropped)"):
    // The bug: a user message holding [ToolResult, Text] serialized ONLY the tool
    // result, dropping the steer text so the model never saw it.
    val msgs = userMessages(List(Content.ToolResult("c1", "result body"), Content.Text("now do X instead")))
    assertEquals(msgs.map(m => Dyn.str(m.role).getOrElse("")), List("tool", "user"))
    assertEquals(Dyn.str(msgs(0).content), Some("result body"))
    assertEquals(Dyn.str(msgs(1).content), Some("now do X instead"))

  test("multiple steers coalesced after a tool result are joined into one user turn"):
    val msgs = userMessages(List(Content.ToolResult("c1", "r"), Content.Text("first"), Content.Text("second")))
    assertEquals(msgs.map(m => Dyn.str(m.role).getOrElse("")), List("tool", "user"))
    assertEquals(Dyn.str(msgs(1).content), Some("first\nsecond"))

  test("a plain tool-results user turn (no steer) still serializes to only tool messages"):
    val msgs = userMessages(List(Content.ToolResult("c1", "r")))
    assertEquals(msgs.map(m => Dyn.str(m.role).getOrElse("")), List("tool"))
