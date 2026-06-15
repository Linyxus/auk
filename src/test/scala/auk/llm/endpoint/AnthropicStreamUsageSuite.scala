package auk.llm.endpoint

import scala.scalajs.js

/** How `AnthropicEndpoint.mergeUsage` reassembles token usage from a streamed
  * response. Anthropic reports `input_tokens` only in `message_start` and the
  * final `output_tokens` only in `message_delta`, so folding just one event
  * dropped the prompt size — which left the TUI's context gauge stuck near 0%. */
class AnthropicStreamUsageSuite extends munit.FunSuite:

  private def usage(fields: (String, js.Any)*): js.Dynamic =
    js.Dynamic.literal(fields*)

  private val absent: js.Dynamic = js.undefined.asInstanceOf[js.Dynamic]

  test("input_tokens from message_start survives a message_delta that omits it"):
    // message_start carries the prompt size; message_delta only the final output.
    val afterStart = AnthropicEndpoint.mergeUsage(None, usage("input_tokens" -> 2095, "output_tokens" -> 4))
    val afterDelta = AnthropicEndpoint.mergeUsage(afterStart, usage("output_tokens" -> 503))
    assertEquals(afterDelta, Some(Usage(inputTokens = 2095, outputTokens = 503)))

  test("usage is absent until any event reports it"):
    assertEquals(AnthropicEndpoint.mergeUsage(None, absent), None)

  test("a later input_tokens (e.g. on message_delta) overrides the earlier one"):
    val start = AnthropicEndpoint.mergeUsage(None, usage("input_tokens" -> 100, "output_tokens" -> 1))
    val delta = AnthropicEndpoint.mergeUsage(start, usage("input_tokens" -> 120, "output_tokens" -> 50))
    assertEquals(delta, Some(Usage(inputTokens = 120, outputTokens = 50)))

  test("an undefined usage object carries the running tally forward unchanged"):
    val start = AnthropicEndpoint.mergeUsage(None, usage("input_tokens" -> 7, "output_tokens" -> 2))
    assertEquals(AnthropicEndpoint.mergeUsage(start, absent), start)
