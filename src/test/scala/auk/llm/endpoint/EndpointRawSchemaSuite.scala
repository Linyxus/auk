package auk.llm.endpoint

import scala.scalajs.js

import auk.llm.tools.Json

/** A `ToolSchema` carrying `rawInputSchema` must reach each endpoint's wire as
  * the tool's input schema VERBATIM — nested structure intact, not flattened by
  * the lossy `ToolSchema.Parameters` conversion. When it is absent, the existing
  * flat path is unchanged. (Same-package access to the `private[endpoint]`
  * `buildParams`.) */
class EndpointRawSchemaSuite extends munit.FunSuite:

  private val anthropic = AnthropicEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "k"))
  private val openai = OpenAIEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "k"))
  private val completion = OpenAICompletionEndpoint(EndpointConfig(baseUrl = "https://example.test", apiKey = "k"))

  /** A deeply nested JSON Schema the flat conversion could not preserve. */
  private val nested: Json = Json.Obj(
    List(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        List(
          "filter" -> Json.Obj(
            List(
              "type" -> Json.Str("object"),
              "properties" -> Json.Obj(
                List("tags" -> Json.Obj(List("type" -> Json.Str("array"), "items" -> Json.Obj(List("type" -> Json.Str("string"))))))
              ),
              "required" -> Json.Arr(List(Json.Str("tags")))
            )
          )
        )
      ),
      "required" -> Json.Arr(List(Json.Str("filter")))
    )
  )

  private val rawTool = ToolSchema("mcp__s__search", "search things", ToolSchema.Parameters(Map.empty, Nil), rawInputSchema = Some(nested))

  private val flatTool =
    ToolSchema("read", "read a file", ToolSchema.Parameters(Map("path" -> ToolSchema.Property("string", "the path")), List("path")))

  private val msgs = List(Message.user("hi"))
  private def cfg(tool: ToolSchema) = LLMConfig(model = "m", tools = List(tool))

  /** The first advertised tool object from a built request body. */
  private def firstTool(params: js.Object): js.Dynamic =
    params.asInstanceOf[js.Dynamic].tools.asInstanceOf[js.Array[js.Dynamic]](0)

  test("Anthropic emits rawInputSchema verbatim as input_schema"):
    val schema = firstTool(anthropic.buildParams(msgs, cfg(rawTool), stream = false)).input_schema
    assertEquals(js.JSON.stringify(schema), nested.render)

  test("Anthropic still flattens a plain ToolSchema (no raw schema)"):
    val schema = firstTool(anthropic.buildParams(msgs, cfg(flatTool), stream = false)).input_schema
    assertEquals(Dyn.str(schema.`type`), Some("object"))
    assertEquals(Dyn.str(schema.properties.path.`type`), Some("string"))
    assertEquals(Dyn.str(schema.properties.path.description), Some("the path"))

  test("OpenAI (Responses) emits rawInputSchema verbatim as parameters"):
    val schema = firstTool(openai.buildParams(msgs, cfg(rawTool), stream = false)).parameters
    assertEquals(js.JSON.stringify(schema), nested.render)

  test("OpenAI (Responses) still flattens a plain ToolSchema"):
    val schema = firstTool(openai.buildParams(msgs, cfg(flatTool), stream = false)).parameters
    assertEquals(Dyn.str(schema.properties.path.`type`), Some("string"))

  test("OpenAI (Chat Completions) emits rawInputSchema verbatim as function.parameters"):
    val schema = firstTool(completion.buildParams(msgs, cfg(rawTool), stream = false)).function.parameters
    assertEquals(js.JSON.stringify(schema), nested.render)

  test("OpenAI (Chat Completions) still flattens a plain ToolSchema"):
    val schema = firstTool(completion.buildParams(msgs, cfg(flatTool), stream = false)).function.parameters
    assertEquals(Dyn.str(schema.properties.path.`type`), Some("string"))
