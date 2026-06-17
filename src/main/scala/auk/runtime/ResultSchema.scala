package auk.runtime

import auk.llm.tools.Json

/** Validates a sub-agent's submitted `result` against the JSON-schema fragment
  * the workflow requested for that agent, on the host, *before* the value is sent
  * back over the bridge.
  *
  * It deliberately mirrors the worker-side decoder ([[auk.library.LibToolInput]],
  * which finally turns the value into the typed `R`): every value this accepts the
  * worker can decode, and the error strings use the same vocabulary
  * (`expected string`, `missing required field 'x'`, `label: …`, `[i]: …`,
  * `'v' is not one of …`). That closes the gap where a malformed result used to
  * reach the worker and fail the whole workflow with an opaque "undecodable
  * result"; instead the host can hand the precise error back to the sub-agent so
  * it can try again.
  *
  * Leniency matches the worker on purpose: `integer` accepts any JSON number (the
  * worker truncates), extra object keys are ignored (the worker reads only the
  * declared fields), and a field absent from `required` is optional — absent or
  * JSON `null` is accepted (decodes to `None`). An absent or unrecognized `type`
  * accepts anything, again matching the worker's permissive fallthrough.
  */
object ResultSchema:

  /** `Right(())` if `value` conforms to `schema`; otherwise `Left(message)` with a
    * path-qualified description of the first mismatch found. */
  def validate(schema: Json, value: Json): Either[String, Unit] =
    val obj: Json.Obj = schema match
      case o: Json.Obj => o
      case _           => Json.Obj(Nil)
    obj.get("type") match
      case Some(Json.Str("string"))  => checkString(obj, value)
      case Some(Json.Str("boolean")) =>
        value match
          case _: Json.Bool => Right(())
          case other        => Left(s"expected boolean, got ${other.typeName}")
      case Some(Json.Str("integer")) =>
        value match
          case _: Json.Num => Right(())
          case other       => Left(s"expected integer, got ${other.typeName}")
      case Some(Json.Str("number")) =>
        value match
          case _: Json.Num => Right(())
          case other       => Left(s"expected number, got ${other.typeName}")
      case Some(Json.Str("array"))  => checkArray(obj, value)
      case Some(Json.Str("object")) => checkObject(obj, value)
      case _                        => Right(()) // no/unknown type → accept (permissive, matches worker)

  private def checkString(schema: Json.Obj, value: Json): Either[String, Unit] =
    value match
      case Json.Str(s) =>
        enumLabels(schema) match
          case Nil    => Right(())
          case labels => if labels.contains(s) then Right(()) else Left(s"'$s' is not one of ${labels.mkString(", ")}")
      case other => Left(s"expected string, got ${other.typeName}")

  private def checkArray(schema: Json.Obj, value: Json): Either[String, Unit] =
    value match
      case Json.Arr(elems) =>
        val items = schema.get("items").getOrElse(Json.Obj(Nil))
        var i = 0
        var err: Option[String] = None
        val it = elems.iterator
        while it.hasNext && err.isEmpty do
          validate(items, it.next()) match
            case Right(())   => ()
            case Left(inner) => err = Some(s"[$i]: $inner")
          i += 1
        err.toLeft(())
      case other => Left(s"expected array, got ${other.typeName}")

  private def checkObject(schema: Json.Obj, value: Json): Either[String, Unit] =
    value match
      case obj: Json.Obj =>
        val props: List[(String, Json)] = schema.get("properties") match
          case Some(Json.Obj(ps)) => ps
          case _                  => Nil
        val required: Set[String] = schema.get("required") match
          case Some(Json.Arr(rs)) => rs.collect { case Json.Str(s) => s }.toSet
          case _                  => Set.empty
        var err: Option[String] = None
        val it = props.iterator
        while it.hasNext && err.isEmpty do
          val (label, sub) = it.next()
          val isRequired = required.contains(label)
          obj.get(label) match
            case None =>
              if isRequired then err = Some(s"missing required field '$label'")
            // An optional field present as JSON null decodes to None (worker parity).
            case Some(Json.Null) if !isRequired => ()
            case Some(fieldValue) =>
              validate(sub, fieldValue) match
                case Right(())   => ()
                case Left(inner) => err = Some(s"$label: $inner")
        err.toLeft(())
      case other => Left(s"expected object, got ${other.typeName}")

  private def enumLabels(schema: Json.Obj): List[String] =
    schema.get("enum") match
      case Some(Json.Arr(es)) => es.collect { case Json.Str(s) => s }
      case _                  => Nil
