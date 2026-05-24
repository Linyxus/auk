package auk.llm.tools

/** A structural model of a JSON Schema fragment, restricted to the subset
  * needed to describe tool parameters.
  *
  * The shape follows JSON Schema (draft 2020-12), which is also what the
  * OpenAI and Anthropic function-calling APIs consume:
  *
  *   - `type`: one of "string", "integer", "number", "boolean", "array",
  *     "object", "null".
  *   - `enum`: the allowed values for a constrained field.
  *   - `items`: the element schema of an array.
  *   - `properties` / `required`: the fields of an object and which of them
  *     must be present.
  *   - `additionalProperties`: whether (and how) keys beyond `properties` are
  *     allowed. `false` closes the object (OpenAI "strict" mode); a schema
  *     describes the value type of an open map.
  */
final case class Schema(
    `type`: Option[String] = None,
    description: Option[String] = None,
    enumValues: List[Json] = Nil,
    items: Option[Schema] = None,
    properties: List[(String, Schema)] = Nil,
    required: List[String] = Nil,
    additionalProperties: Option[Schema.Additional] = None
):
  /** Return a copy carrying the given description. */
  def withDescription(d: String): Schema = copy(description = Some(d))

  /** Render this schema into a [[Json]] value. Field order is stable so that
    * serialized output is deterministic.
    */
  def toJson: Json =
    val b = List.newBuilder[(String, Json)]
    `type`.foreach(t => b += "type" -> Json.Str(t))
    description.foreach(d => b += "description" -> Json.Str(d))
    if enumValues.nonEmpty then b += "enum" -> Json.Arr(enumValues)
    items.foreach(i => b += "items" -> i.toJson)
    if properties.nonEmpty then
      b += "properties" -> Json.Obj(properties.map((k, v) => k -> v.toJson))
    if required.nonEmpty then
      b += "required" -> Json.Arr(required.map(Json.Str(_)))
    additionalProperties.foreach {
      case Schema.Additional.Allow(v)  => b += "additionalProperties" -> Json.Bool(v)
      case Schema.Additional.Of(s)     => b += "additionalProperties" -> s.toJson
    }
    Json.Obj(b.result())

object Schema:
  /** The value of the `additionalProperties` keyword. */
  enum Additional:
    case Allow(value: Boolean)
    case Of(schema: Schema)

  def string(
      description: Option[String] = None,
      enumValues: List[String] = Nil
  ): Schema =
    Schema(
      `type` = Some("string"),
      description = description,
      enumValues = enumValues.map(Json.Str(_))
    )

  def integer(description: Option[String] = None): Schema =
    Schema(`type` = Some("integer"), description = description)

  def number(description: Option[String] = None): Schema =
    Schema(`type` = Some("number"), description = description)

  def boolean(description: Option[String] = None): Schema =
    Schema(`type` = Some("boolean"), description = description)

  def array(items: Schema, description: Option[String] = None): Schema =
    Schema(`type` = Some("array"), description = description, items = Some(items))

  /** A closed object: only the listed `properties` are permitted. */
  def obj(
      properties: List[(String, Schema)],
      required: List[String],
      description: Option[String] = None
  ): Schema =
    Schema(
      `type` = Some("object"),
      description = description,
      properties = properties,
      required = required,
      additionalProperties = Some(Additional.Allow(false))
    )

  /** An open object whose values all conform to `values` (e.g. `Map[String, A]`). */
  def map(values: Schema, description: Option[String] = None): Schema =
    Schema(
      `type` = Some("object"),
      description = description,
      additionalProperties = Some(Additional.Of(values))
    )
