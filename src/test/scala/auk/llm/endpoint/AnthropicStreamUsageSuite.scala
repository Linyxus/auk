package auk.llm.endpoint

import scala.scalajs.js

/** How `AnthropicEndpoint.mergeUsage` reassembles token usage from a streamed
  * response. Usage is split across `message_start` / `message_delta` and the
  * split is provider-specific — stock Anthropic puts input in `message_start`,
  * z.ai sends zeros there and the real figures (input, output, and cache reads)
  * in `message_delta`. `mergeUsage` folds each event into a per-field
  * `UsageAccum`, carrying every component forward from the last event that
  * reported it; `UsageAccum.usage` sums cached tokens into the prompt size only
  * at the end. Field-wise carry-forward is shape-proof: no event — not even a
  * cache-less `message_delta` — can erase a component another event supplied, so
  * a cache hit (where `input_tokens` collapses and the prefix moves to
  * `cache_read_input_tokens`) never leaves the TUI's context gauge reading ~0%. */
class AnthropicStreamUsageSuite extends munit.FunSuite:

  private def usage(fields: (String, js.Any)*): js.Dynamic =
    js.Dynamic.literal(fields*)

  private val absent: js.Dynamic = js.undefined.asInstanceOf[js.Dynamic]

  private val empty = AnthropicEndpoint.UsageAccum()

  test("input_tokens from message_start survives a message_delta that omits it"):
    // message_start carries the prompt size; message_delta only the final output.
    val afterStart = AnthropicEndpoint.mergeUsage(empty, usage("input_tokens" -> 2095, "output_tokens" -> 4))
    val afterDelta = AnthropicEndpoint.mergeUsage(afterStart, usage("output_tokens" -> 503))
    assertEquals(afterDelta.usage, Some(Usage(inputTokens = 2095, outputTokens = 503)))

  test("usage is absent until any event reports it"):
    assertEquals(AnthropicEndpoint.mergeUsage(empty, absent).usage, None)

  test("a later input_tokens (e.g. on message_delta) overrides the earlier one"):
    val start = AnthropicEndpoint.mergeUsage(empty, usage("input_tokens" -> 100, "output_tokens" -> 1))
    val delta = AnthropicEndpoint.mergeUsage(start, usage("input_tokens" -> 120, "output_tokens" -> 50))
    assertEquals(delta.usage, Some(Usage(inputTokens = 120, outputTokens = 50)))

  test("an undefined usage object carries the running tally forward unchanged"):
    val start = AnthropicEndpoint.mergeUsage(empty, usage("input_tokens" -> 7, "output_tokens" -> 2))
    assertEquals(AnthropicEndpoint.mergeUsage(start, absent), start)

  test("z.ai shape: usage lands in message_delta and cached tokens count toward the prompt"):
    // Observed live: message_start carries zeros; message_delta carries the real
    // figures. On a cache hit input_tokens collapses (61) and the prefix moves to
    // cache_read_input_tokens (14784) — together the true 14,845-token prompt.
    val afterStart = AnthropicEndpoint.mergeUsage(empty, usage("input_tokens" -> 0, "output_tokens" -> 0))
    val afterDelta = AnthropicEndpoint.mergeUsage(
      afterStart,
      usage("input_tokens" -> 61, "output_tokens" -> 22, "cache_read_input_tokens" -> 14784),
    )
    assertEquals(afterDelta.usage, Some(Usage(inputTokens = 14845, outputTokens = 22)))

  test("cache_read and cache_creation both fold into the prompt total"):
    val u = AnthropicEndpoint.mergeUsage(
      empty,
      usage(
        "input_tokens" -> 100,
        "output_tokens" -> 5,
        "cache_read_input_tokens" -> 900,
        "cache_creation_input_tokens" -> 50,
      ),
    )
    assertEquals(u.usage, Some(Usage(inputTokens = 1050, outputTokens = 5)))

  test("a cache-less message_delta does not clobber the message_start cache totals"):
    // The regression this design exists to prevent. message_start carries the
    // full cache-inclusive prompt; a later message_delta reports input_tokens and
    // output_tokens but NO cache fields. The old collapse-at-merge code recomputed
    // the prompt from the delta's fields alone (61), erasing the 14,884 cached
    // tokens; field-wise carry-forward keeps them, so the prompt stays 14,945.
    val afterStart = AnthropicEndpoint.mergeUsage(
      empty,
      usage(
        "input_tokens" -> 61,
        "cache_read_input_tokens" -> 14784,
        "cache_creation_input_tokens" -> 100,
        "output_tokens" -> 1,
      ),
    )
    val afterDelta = AnthropicEndpoint.mergeUsage(afterStart, usage("input_tokens" -> 61, "output_tokens" -> 500))
    assertEquals(afterDelta.usage, Some(Usage(inputTokens = 14945, outputTokens = 500)))
