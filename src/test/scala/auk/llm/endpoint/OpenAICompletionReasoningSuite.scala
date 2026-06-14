package auk.llm.endpoint

import scala.scalajs.js

/** Chat Completions has no standard reasoning field, so providers diverge:
  * OpenRouter sends `reasoning`, z.ai/DeepSeek send `reasoning_content`. The
  * endpoint must read both, or thinking is silently dropped (the GLM-5.2-on-z.ai
  * bug). */
class OpenAICompletionReasoningSuite extends munit.FunSuite:

  private def reasoning(d: js.Dynamic): Option[String] =
    OpenAICompletionEndpoint.reasoningText(d)

  test("reads OpenRouter's `reasoning` field"):
    assertEquals(reasoning(js.Dynamic.literal(reasoning = "thinking...")), Some("thinking..."))

  test("reads z.ai/DeepSeek's `reasoning_content` field"):
    assertEquals(reasoning(js.Dynamic.literal(reasoning_content = "step 1")), Some("step 1"))

  test("absent reasoning yields None"):
    assertEquals(reasoning(js.Dynamic.literal(content = "hello")), None)

  test("empty reasoning yields None"):
    assertEquals(reasoning(js.Dynamic.literal(reasoning_content = "")), None)
