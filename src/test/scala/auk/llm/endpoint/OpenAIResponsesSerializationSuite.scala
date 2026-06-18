package auk.llm.endpoint

import scala.scalajs.js

/** OpenAI Responses API serialization. Real-time steering coalesces a user
  * message after the tool-results turn into `[ToolResult, Text]`; the text (the
  * steer) must still reach the wire. The bug dropped it: a user item with tool
  * results serialized ONLY the function outputs.
  *
  * The lazy SDK client is never touched by buildParams, so no key/network. */
class OpenAIResponsesSerializationSuite extends munit.FunSuite:

  private val ep = OpenAIEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "test-key"))

  private def inputItems(content: List[Content]): List[js.Dynamic] =
    ep.buildParams(List(Message(Role.User, content)), LLMConfig(model = "m"), stream = false)
      .asInstanceOf[js.Dynamic].input.asInstanceOf[js.Array[js.Dynamic]].toList

  test("a steer coalesced after a tool result is sent as its own user turn (not dropped)"):
    val items = inputItems(List(Content.ToolResult("c1", "result body"), Content.Text("now do X instead")))
    assertEquals(items.size, 2)
    assertEquals(Dyn.str(items(0).`type`), Some("function_call_output"))
    assertEquals(Dyn.str(items(0).output), Some("result body"))
    assertEquals(Dyn.str(items(1).role), Some("user"))
    assertEquals(Dyn.str(items(1).content), Some("now do X instead"))

  test("a plain tool-results user turn (no steer) serializes to only the function output"):
    val items = inputItems(List(Content.ToolResult("c1", "r")))
    assertEquals(items.size, 1)
    assertEquals(Dyn.str(items(0).`type`), Some("function_call_output"))
