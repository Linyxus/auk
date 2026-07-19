package auk.llm.endpoint

import auk.llm.tools.Json

case class ToolSchema(
    name: String,
    description: String,
    parameters: ToolSchema.Parameters,
    // When set, this JSON-Schema object is advertised to the model verbatim
    // instead of the flattened `parameters` (see [[auk.runtime.ToolBridge]] and
    // [[auk.llm.tools.Tool.rawParametersSchema]]). Endpoints emit it as the
    // tool's input schema unchanged, so a nested (e.g. MCP-authored) schema
    // survives the round trip; `None` keeps the existing flat path.
    rawInputSchema: Option[Json] = None
)

object ToolSchema:
  case class Parameters(
      properties: Map[String, Property],
      required: List[String] = List.empty
  )

  case class Property(
      `type`: String,
      description: String = "",
      enumValues: List[String] = List.empty,
      items: Option[Property] = None
  )
