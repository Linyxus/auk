package auk.llm.tools

import auk.llm.tools.ToolInput.{parse, schemaJson}

// ---------------------------------------------------------------------------
// Sample ADTs under test
// ---------------------------------------------------------------------------

@desc("Parameters for the get_weather tool")
case class GetWeatherParam(
    @desc("Name of the city") cityName: String
) derives ToolInput

enum Unit derives ToolInput:
  case Celsius, Fahrenheit

case class Coord(lat: Double, lon: Double) derives ToolInput

case class Forecast(
    @desc("Location to query") city: String,
    coord: Coord,
    @desc("Number of days") days: Int,
    unit: Unit,
    tags: List[String],
    @desc("Optional note") note: Option[String]
) derives ToolInput

class ToolInputSuite extends munit.FunSuite:

  // -- schema generation -----------------------------------------------------

  test("schema for the documented GetWeatherParam example") {
    val schema = ToolInput[GetWeatherParam].schemaJson
    val expected = Json.parse(
      """
      {
        "type": "object",
        "description": "Parameters for the get_weather tool",
        "properties": {
          "cityName": { "type": "string", "description": "Name of the city" }
        },
        "required": ["cityName"],
        "additionalProperties": false
      }
      """
    )
    assertEquals(Right(schema), expected)
  }

  test("primitive field types map to the right JSON schema types") {
    val schema = ToolInput[Forecast].schemaJson.asInstanceOf[Json.Obj]
    val props = schema.get("properties").get.asInstanceOf[Json.Obj]

    def typeOf(field: String): String =
      props.get(field).get.asInstanceOf[Json.Obj].get("type").get
        .asInstanceOf[Json.Str].value

    assertEquals(typeOf("city"), "string")
    assertEquals(typeOf("days"), "integer")
    assertEquals(typeOf("tags"), "array")
    assertEquals(typeOf("coord"), "object")
    assertEquals(typeOf("unit"), "string")
  }

  test("Option fields are described but excluded from required") {
    val schema = ToolInput[Forecast].schemaJson.asInstanceOf[Json.Obj]
    val required = schema.get("required").get.asInstanceOf[Json.Arr]
      .elems.map(_.asInstanceOf[Json.Str].value)
    assert(required.contains("city"))
    assert(required.contains("note") == false, "Option must not be required")

    // The optional field still appears in properties with its inner type.
    val props = schema.get("properties").get.asInstanceOf[Json.Obj]
    val note = props.get("note").get.asInstanceOf[Json.Obj]
    assertEquals(note.get("type"), Some(Json.Str("string")))
    assertEquals(note.get("description"), Some(Json.Str("Optional note")))
  }

  test("nested case class produces a nested object schema") {
    val schema = ToolInput[Forecast].schemaJson.asInstanceOf[Json.Obj]
    val props = schema.get("properties").get.asInstanceOf[Json.Obj]
    val coord = props.get("coord").get
    val expected = Json.parse(
      """
      {
        "type": "object",
        "properties": {
          "lat": { "type": "number" },
          "lon": { "type": "number" }
        },
        "required": ["lat", "lon"],
        "additionalProperties": false
      }
      """
    )
    assertEquals(Right(coord), expected)
  }

  test("array schema carries an items schema") {
    val schema = ToolInput[Forecast].schemaJson.asInstanceOf[Json.Obj]
    val props = schema.get("properties").get.asInstanceOf[Json.Obj]
    val tags = props.get("tags").get.asInstanceOf[Json.Obj]
    assertEquals(tags.get("type"), Some(Json.Str("array")))
    assertEquals(tags.get("items"), Some(Json.Obj(List("type" -> Json.Str("string")))))
  }

  test("simple enum becomes a string schema with enum values") {
    val schema = ToolInput[Unit].schemaJson
    val expected = Json.parse(
      """{ "type": "string", "enum": ["Celsius", "Fahrenheit"] }"""
    )
    assertEquals(Right(schema), expected)
  }

  test("Map[String, A] becomes an open object via additionalProperties") {
    val schema = summon[ToolInput[Map[String, Int]]].schemaJson
    val expected = Json.parse(
      """{ "type": "object", "additionalProperties": { "type": "integer" } }"""
    )
    assertEquals(Right(schema), expected)
  }

  // -- decoding ---------------------------------------------------------------

  test("decode the documented GetWeatherParam example") {
    assertEquals(
      ToolInput[GetWeatherParam].parse("""{"cityName": "Paris"}"""),
      Right(GetWeatherParam("Paris"))
    )
  }

  test("decode a fully populated nested structure") {
    val json =
      """
      {
        "city": "Paris",
        "coord": { "lat": 48.85, "lon": 2.35 },
        "days": 3,
        "unit": "Celsius",
        "tags": ["a", "b"],
        "note": "hello"
      }
      """
    assertEquals(
      ToolInput[Forecast].parse(json),
      Right(
        Forecast("Paris", Coord(48.85, 2.35), 3, Unit.Celsius, List("a", "b"), Some("hello"))
      )
    )
  }

  test("omitted optional field decodes to None") {
    val json =
      """
      {
        "city": "Paris",
        "coord": { "lat": 48.85, "lon": 2.35 },
        "days": 3,
        "unit": "Fahrenheit",
        "tags": []
      }
      """
    assertEquals(
      ToolInput[Forecast].parse(json),
      Right(Forecast("Paris", Coord(48.85, 2.35), 3, Unit.Fahrenheit, Nil, None))
    )
  }

  test("explicit null optional field decodes to None") {
    val json =
      """{ "city": "P", "coord": {"lat":0,"lon":0}, "days": 1, "unit": "Celsius", "tags": [], "note": null }"""
    assertEquals(
      ToolInput[Forecast].parse(json).map(_.note),
      Right(None)
    )
  }

  test("unknown fields are ignored when decoding") {
    assertEquals(
      ToolInput[GetWeatherParam].parse("""{"cityName": "Paris", "extra": 1}"""),
      Right(GetWeatherParam("Paris"))
    )
  }

  test("integers reject fractional values and out-of-range values") {
    assert(summon[ToolInput[Int]].parse("3.5").isLeft)
    assert(summon[ToolInput[Int]].parse("9999999999").isLeft) // beyond Int range
    assertEquals(summon[ToolInput[Long]].parse("9999999999"), Right(9999999999L))
  }

  // -- decode errors ----------------------------------------------------------

  test("missing required field reports the field name") {
    val err = ToolInput[GetWeatherParam].parse("{}").swap.toOption.get
    assert(err.render.contains("cityName"), err.render)
    assert(err.render.contains("missing"), err.render)
  }

  test("type mismatch reports a path into the structure") {
    val json = """{ "city": "P", "coord": {"lat":"oops","lon":0}, "days": 1, "unit": "Celsius", "tags": [] }"""
    val err = ToolInput[Forecast].parse(json).swap.toOption.get
    assertEquals(err.render, "coord.lat: expected number, got string")
  }

  test("array element errors include the index in the path") {
    val err = summon[ToolInput[List[Int]]].parse("[1, \"x\", 3]").swap.toOption.get
    assertEquals(err.render, "[1]: expected integer, got string")
  }

  test("invalid enum value lists the allowed values") {
    val err = ToolInput[Unit].parse("\"Kelvin\"").swap.toOption.get
    assert(err.render.contains("Kelvin"), err.render)
    assert(err.render.contains("Celsius"), err.render)
  }

  test("malformed JSON surfaces as a decode error") {
    val err = ToolInput[GetWeatherParam].parse("{not json").swap.toOption.get
    assert(err.render.contains("invalid JSON"), err.render)
  }

  test("decoding a non-object into a product fails cleanly") {
    val err = ToolInput[GetWeatherParam].parse("42").swap.toOption.get
    assertEquals(err.render, "expected object, got number")
  }
