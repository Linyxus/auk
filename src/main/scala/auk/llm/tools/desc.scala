package auk.llm.tools

import scala.annotation.StaticAnnotation

/** Attaches a human readable description to a tool-input field or type.
  *
  * The text is surfaced as the `description` keyword in the generated JSON
  * schema, which models use to understand each parameter.
  *
  * {{{
  * case class GetWeatherParam(
  *   @desc("Name of the city") cityName: String
  * ) derives ToolInput
  * }}}
  */
final class desc(val value: String) extends StaticAnnotation
