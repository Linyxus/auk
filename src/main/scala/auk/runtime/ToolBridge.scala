package auk.runtime

import auk.llm.tools.{Tool, Schema, Json}
import auk.llm.endpoint.ToolSchema

/** Translates a [[auk.llm.tools.Tool]] into the wire-format
  * [[auk.llm.endpoint.ToolSchema]] that endpoints advertise to a model.
  *
  * The two representations differ: a tool's [[Schema]] is full JSON Schema
  * (nested objects, `additionalProperties`, …), while `ToolSchema.Parameters`
  * is the flat subset every provider's function-calling API agrees on — a map
  * of top-level properties, each with a `type`, optional `description`/`enum`,
  * and one level of array `items`. The conversion is therefore lossy for
  * deeply nested parameters, which is fine for the flat parameter objects tools
  * use in practice (paths, flags, strings); nested shapes would silently
  * flatten to their `type` and should be avoided in tool params for now.
  *
  * This logic previously lived, duplicated, inside `auk.cli.ChatLoop`; the
  * runtime now owns the single copy.
  */
object ToolBridge:
  /** The full advertisement for one tool. */
  def toToolSchema(tool: Tool): ToolSchema =
    ToolSchema(
      name = tool.name,
      description = tool.description,
      parameters = toParameters(tool.input.schema),
      // A tool may carry a pre-built schema (MCP) to advertise verbatim rather
      // than the flattened `parameters`; carry it straight through so the
      // endpoint can emit it unchanged.
      rawInputSchema = tool.rawParametersSchema
    )

  private def toParameters(s: Schema): ToolSchema.Parameters =
    ToolSchema.Parameters(
      properties = s.properties.map((k, v) => k -> toProperty(v)).toMap,
      required = s.required
    )

  private def toProperty(s: Schema): ToolSchema.Property =
    ToolSchema.Property(
      `type` = s.`type`.getOrElse("string"),
      description = s.description.getOrElse(""),
      enumValues = s.enumValues.collect { case Json.Str(v) => v },
      items = s.items.map(toProperty)
    )
