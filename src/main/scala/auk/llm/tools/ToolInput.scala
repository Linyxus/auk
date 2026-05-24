package auk.llm.tools

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/** A type class providing, for a tool-parameter type `A`, both:
  *
  *   - a JSON [[Schema]] advertising `A` to a model, and
  *   - a decoder turning the model's JSON arguments back into an `A`.
  *
  * Instances exist for the common primitives and collections, and an instance
  * for any case class (or simple enum) can be obtained with `derives ToolInput`:
  *
  * {{{
  * case class GetWeatherParam(
  *   @desc("Name of the city") cityName: String
  * ) derives ToolInput
  *
  * val ti  = summon[ToolInput[GetWeatherParam]]
  * val js  = ti.schema.toJson            // the JSON schema
  * val arg = ti.parse("""{"cityName":"Paris"}""")  // Right(GetWeatherParam("Paris"))
  * }}}
  */
trait ToolInput[A]:
  /** The JSON schema describing values of `A`. */
  def schema: Schema

  /** Decode a JSON value into an `A`. */
  def decode(json: Json): Either[DecodeError, A]

  /** Whether a field of this type may be omitted from its enclosing object.
    * Only `Option[_]` overrides this to `true`.
    */
  def isOptional: Boolean = false

object ToolInput:
  def apply[A](using ti: ToolInput[A]): ToolInput[A] = ti

  extension [A](ti: ToolInput[A])
    /** Parse a JSON document string and decode it into an `A`. */
    def parse(jsonString: String): Either[DecodeError, A] =
      Json.parse(jsonString) match
        case Left(err)   => Left(DecodeError(s"invalid JSON: $err"))
        case Right(json) => ti.decode(json)

    /** The schema rendered as a [[Json]] value. */
    def schemaJson: Json = ti.schema.toJson

  /** Build an instance from a fixed schema and a decode function. */
  def instance[A](s: Schema)(d: Json => Either[DecodeError, A]): ToolInput[A] =
    new ToolInput[A]:
      def schema: Schema = s
      def decode(json: Json): Either[DecodeError, A] = d(json)

  private def wrongType[A](expected: String, got: Json): Either[DecodeError, A] =
    Left(DecodeError(s"expected $expected, got ${got.typeName}"))

  // ---------------------------------------------------------------------------
  // Primitives
  // ---------------------------------------------------------------------------

  given ToolInput[String] = instance(Schema.string()) {
    case Json.Str(s) => Right(s)
    case other       => wrongType("string", other)
  }

  given ToolInput[Boolean] = instance(Schema.boolean()) {
    case Json.Bool(b) => Right(b)
    case other        => wrongType("boolean", other)
  }

  given ToolInput[Int] = instance(Schema.integer()) {
    case Json.Num(n) if n.isValidInt => Right(n.toIntExact)
    case Json.Num(n)                 => Left(DecodeError(s"$n is not a 32-bit integer"))
    case other                       => wrongType("integer", other)
  }

  given ToolInput[Long] = instance(Schema.integer()) {
    case Json.Num(n) if n.isValidLong => Right(n.toLongExact)
    case Json.Num(n)                  => Left(DecodeError(s"$n is not a 64-bit integer"))
    case other                        => wrongType("integer", other)
  }

  given ToolInput[Double] = instance(Schema.number()) {
    case Json.Num(n) => Right(n.toDouble)
    case other       => wrongType("number", other)
  }

  given ToolInput[BigDecimal] = instance(Schema.number()) {
    case Json.Num(n) => Right(n)
    case other       => wrongType("number", other)
  }

  // ---------------------------------------------------------------------------
  // Collections and Option
  // ---------------------------------------------------------------------------

  private def decodeElems[A](
      elems: List[Json],
      inner: ToolInput[A]
  ): Either[DecodeError, List[A]] =
    val buf = List.newBuilder[A]
    var err: Option[DecodeError] = None
    var i = 0
    val it = elems.iterator
    while it.hasNext && err.isEmpty do
      inner.decode(it.next()) match
        case Right(v) => buf += v
        case Left(e)  => err = Some(e.prefix(s"[$i]"))
      i += 1
    err match
      case Some(e) => Left(e)
      case None    => Right(buf.result())

  given listInput[A](using inner: ToolInput[A]): ToolInput[List[A]] =
    instance(Schema.array(inner.schema)) {
      case Json.Arr(elems) => decodeElems(elems, inner)
      case other           => wrongType("array", other)
    }

  given vectorInput[A](using inner: ToolInput[A]): ToolInput[Vector[A]] =
    instance(Schema.array(inner.schema)) {
      case Json.Arr(elems) => decodeElems(elems, inner).map(_.toVector)
      case other           => wrongType("array", other)
    }

  given mapInput[A](using inner: ToolInput[A]): ToolInput[Map[String, A]] =
    instance(Schema.map(inner.schema)) {
      case Json.Obj(fields) =>
        val buf = List.newBuilder[(String, A)]
        var err: Option[DecodeError] = None
        val it = fields.iterator
        while it.hasNext && err.isEmpty do
          val (k, v) = it.next()
          inner.decode(v) match
            case Right(value) => buf += (k -> value)
            case Left(e)      => err = Some(e.prefix(k))
        err match
          case Some(e) => Left(e)
          case None    => Right(buf.result().toMap)
      case other => wrongType("object", other)
    }

  given optionInput[A](using inner: ToolInput[A]): ToolInput[Option[A]] =
    new ToolInput[Option[A]]:
      override def isOptional: Boolean = true
      def schema: Schema = inner.schema
      def decode(json: Json): Either[DecodeError, Option[A]] = json match
        case Json.Null => Right(None)
        case other     => inner.decode(other).map(Some(_))

  // ---------------------------------------------------------------------------
  // Derivation
  // ---------------------------------------------------------------------------

  inline def derived[A](using m: Mirror.Of[A]): ToolInput[A] =
    inline m match
      case p: Mirror.ProductOf[A] => deriveProduct[A](using p)
      case s: Mirror.SumOf[A]     => deriveSum[A](using s)

  private inline def summonAll[T <: Tuple]: List[ToolInput[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[ToolInput[h]] :: summonAll[t]

  private inline def labelsOf[T <: Tuple]: List[String] =
    constValueTuple[T].toList.map(_.toString)

  private inline def deriveProduct[A](using m: Mirror.ProductOf[A]): ToolInput[A] =
    buildProduct[A](
      m,
      labelsOf[m.MirroredElemLabels],
      summonAll[m.MirroredElemTypes],
      Annotations.fieldDescriptions[A],
      Annotations.typeDescription[A]
    )

  /** Public factory referenced by inlined derivation (so the implementation
    * class can stay private).
    */
  def buildProduct[A](
      m: Mirror.ProductOf[A],
      labels: List[String],
      instances: List[ToolInput[?]],
      fieldDescs: Map[String, String],
      typeDesc: Option[String]
  ): ToolInput[A] =
    new ProductToolInput[A](m, labels, instances, fieldDescs, typeDesc)

  /** Singletons of a sum's parameterless cases, in declaration order. The
    * `EmptyTuple` refinement makes a case with parameters a compile error.
    */
  private inline def summonSingletons[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        val mh = summonInline[Mirror.ProductOf[h] { type MirroredElemTypes = EmptyTuple }]
        mh.fromProduct(EmptyTuple) :: summonSingletons[t]

  private inline def deriveSum[A](using m: Mirror.SumOf[A]): ToolInput[A] =
    buildEnum[A](
      labelsOf[m.MirroredElemLabels],
      summonSingletons[m.MirroredElemTypes].asInstanceOf[List[A]],
      Annotations.typeDescription[A]
    )

  /** Public factory referenced by inlined derivation. */
  def buildEnum[A](
      labels: List[String],
      values: List[A],
      typeDesc: Option[String]
  ): ToolInput[A] =
    new EnumToolInput[A](labels, values, typeDesc)

  private final class ProductToolInput[A](
      m: Mirror.ProductOf[A],
      labels: List[String],
      instances: List[ToolInput[?]],
      fieldDescs: Map[String, String],
      typeDesc: Option[String]
  ) extends ToolInput[A]:
    private val fields: List[(String, ToolInput[?])] = labels.zip(instances)

    def schema: Schema =
      val props = fields.map { (label, inst) =>
        val s = inst.schema
        label -> fieldDescs.get(label).fold(s)(s.withDescription)
      }
      val required = fields.collect { case (l, i) if !i.isOptional => l }
      Schema.obj(props, required, typeDesc)

    def decode(json: Json): Either[DecodeError, A] = json match
      case obj @ Json.Obj(_) =>
        val values = new Array[Any](fields.length)
        var err: Option[DecodeError] = None
        var i = 0
        val it = fields.iterator
        while it.hasNext && err.isEmpty do
          val (label, inst) = it.next()
          obj.get(label) match
            case Some(fieldJson) =>
              inst.decode(fieldJson) match
                case Right(v) => values(i) = v
                case Left(e)  => err = Some(e.prefix(label))
            case None =>
              if inst.isOptional then values(i) = None
              else err = Some(DecodeError(s"missing required field '$label'"))
          i += 1
        err match
          case Some(e) => Left(e)
          case None    => Right(m.fromProduct(Tuple.fromArray(values)))
      case other => wrongType("object", other)

  private final class EnumToolInput[A](
      labels: List[String],
      values: List[A],
      typeDesc: Option[String]
  ) extends ToolInput[A]:
    def schema: Schema = Schema.string(typeDesc, labels)

    def decode(json: Json): Either[DecodeError, A] = json match
      case Json.Str(s) =>
        labels.indexOf(s) match
          case -1 =>
            Left(DecodeError(s"'$s' is not one of ${labels.mkString(", ")}"))
          case i => Right(values(i))
      case other => wrongType("string", other)
