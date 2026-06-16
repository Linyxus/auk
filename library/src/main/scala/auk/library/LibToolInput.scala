package auk.library

import scala.scalajs.js
import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/** A worker-side analogue of `auk.llm.tools.ToolInput` (which lives in the root
  * module and is not on the REPL worker's classpath). For a workflow result type
  * `A`, it provides:
  *
  *   - a JSON-Schema fragment (as a live JS value) advertising `A` to a
  *     sub-agent, sent in `agent_call.schema`, and
  *   - a decoder turning the sub-agent's returned JSON value back into an `A`.
  *
  * Schemas are built as JS objects (so they serialize with `js.JSON.stringify`)
  * and decoding consumes a parsed JS value (`js.JSON.parse`). An instance for any
  * case class or simple enum is available via `derives LibToolInput`.
  */
trait LibToolInput[A]:
  /** The JSON-Schema fragment describing values of `A`, as a JS value. */
  def schema: js.Any
  /** Decode a parsed JSON value into an `A`, or a human-readable error path. */
  def decode(json: js.Any): Either[String, A]
  /** Whether a field of this type may be omitted (only `Option[_]` is). */
  def isOptional: Boolean = false

object LibToolInput:
  def apply[A](using ti: LibToolInput[A]): LibToolInput[A] = ti

  /** Build a JS object from string-keyed fields. */
  private[library] def jsObj(fields: (String, js.Any)*): js.Any =
    val o = js.Dynamic.literal()
    for (k, v) <- fields do o.updateDynamic(k)(v)
    o.asInstanceOf[js.Any]

  def instance[A](s: js.Any)(d: js.Any => Either[String, A]): LibToolInput[A] =
    new LibToolInput[A]:
      def schema: js.Any = s
      def decode(json: js.Any): Either[String, A] = d(json)

  private def isNum(j: js.Any): Boolean = js.typeOf(j) == "number"

  // -- primitives -------------------------------------------------------------

  given LibToolInput[String] = instance(jsObj("type" -> "string")) { j =>
    if js.typeOf(j) == "string" then Right(j.asInstanceOf[String]) else Left("expected string")
  }
  given LibToolInput[Boolean] = instance(jsObj("type" -> "boolean")) { j =>
    if js.typeOf(j) == "boolean" then Right(j.asInstanceOf[Boolean]) else Left("expected boolean")
  }
  given LibToolInput[Int] = instance(jsObj("type" -> "integer")) { j =>
    if isNum(j) then Right(j.asInstanceOf[Double].toInt) else Left("expected integer")
  }
  given LibToolInput[Long] = instance(jsObj("type" -> "integer")) { j =>
    if isNum(j) then Right(j.asInstanceOf[Double].toLong) else Left("expected integer")
  }
  given LibToolInput[Double] = instance(jsObj("type" -> "number")) { j =>
    if isNum(j) then Right(j.asInstanceOf[Double]) else Left("expected number")
  }

  // -- collections and Option -------------------------------------------------

  private def decodeAll[A](arr: js.Array[js.Any], inner: LibToolInput[A]): Either[String, List[A]] =
    val buf = List.newBuilder[A]
    var i = 0
    var err: Option[String] = None
    while i < arr.length && err.isEmpty do
      inner.decode(arr(i)) match
        case Right(v) => buf += v
        case Left(e)  => err = Some(s"[$i]: $e")
      i += 1
    err.map(Left(_)).getOrElse(Right(buf.result()))

  given listInput[A](using inner: LibToolInput[A]): LibToolInput[List[A]] =
    instance(jsObj("type" -> "array", "items" -> inner.schema)) { j =>
      if js.Array.isArray(j) then decodeAll(j.asInstanceOf[js.Array[js.Any]], inner)
      else Left("expected array")
    }

  given vectorInput[A](using inner: LibToolInput[A]): LibToolInput[Vector[A]] =
    instance(jsObj("type" -> "array", "items" -> inner.schema)) { j =>
      if js.Array.isArray(j) then decodeAll(j.asInstanceOf[js.Array[js.Any]], inner).map(_.toVector)
      else Left("expected array")
    }

  given optionInput[A](using inner: LibToolInput[A]): LibToolInput[Option[A]] =
    new LibToolInput[Option[A]]:
      override def isOptional: Boolean = true
      def schema: js.Any = inner.schema
      def decode(json: js.Any): Either[String, Option[A]] =
        if json == null || js.isUndefined(json) then Right(None)
        else inner.decode(json).map(Some(_))

  // -- derivation -------------------------------------------------------------

  inline def derived[A](using m: Mirror.Of[A]): LibToolInput[A] =
    inline m match
      case p: Mirror.ProductOf[A] => deriveProduct[A](using p)
      case s: Mirror.SumOf[A]     => deriveSum[A](using s)

  private inline def summonAll[T <: Tuple]: List[LibToolInput[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[LibToolInput[h]] :: summonAll[t]

  private inline def labelsOf[T <: Tuple]: List[String] =
    constValueTuple[T].toList.map(_.toString)

  private inline def deriveProduct[A](using m: Mirror.ProductOf[A]): LibToolInput[A] =
    buildProduct[A](m, labelsOf[m.MirroredElemLabels], summonAll[m.MirroredElemTypes])

  /** Public factory referenced by inlined derivation (so the impl class stays
    * private — the inline expansion at the `derives` site can't `new` it). */
  def buildProduct[A](m: Mirror.ProductOf[A], labels: List[String], instances: List[LibToolInput[?]]): LibToolInput[A] =
    new ProductToolInput[A](m, labels, instances)

  private inline def summonSingletons[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        val mh = summonInline[Mirror.ProductOf[h] { type MirroredElemTypes = EmptyTuple }]
        mh.fromProduct(EmptyTuple) :: summonSingletons[t]

  private inline def deriveSum[A](using m: Mirror.SumOf[A]): LibToolInput[A] =
    buildEnum[A](labelsOf[m.MirroredElemLabels], summonSingletons[m.MirroredElemTypes].asInstanceOf[List[A]])

  /** Public factory referenced by inlined derivation. */
  def buildEnum[A](labels: List[String], values: List[A]): LibToolInput[A] =
    new EnumToolInput[A](labels, values)

  private final class ProductToolInput[A](
      m: Mirror.ProductOf[A],
      labels: List[String],
      instances: List[LibToolInput[?]]
  ) extends LibToolInput[A]:
    private val fields: List[(String, LibToolInput[?])] = labels.zip(instances)

    def schema: js.Any =
      val props = js.Dynamic.literal()
      for (label, inst) <- fields do props.updateDynamic(label)(inst.schema)
      val required: js.Array[js.Any] =
        js.Array(fields.collect { case (l, i) if !i.isOptional => l.asInstanceOf[js.Any] }*)
      jsObj("type" -> "object", "properties" -> props.asInstanceOf[js.Any], "required" -> required)

    def decode(json: js.Any): Either[String, A] =
      if json == null || js.typeOf(json) != "object" || js.Array.isArray(json) then Left("expected object")
      else
        val d = json.asInstanceOf[js.Dynamic]
        val values = new Array[Any](fields.length)
        var i = 0
        var err: Option[String] = None
        val it = fields.iterator
        while it.hasNext && err.isEmpty do
          val (label, inst) = it.next()
          val fieldJson = d.selectDynamic(label).asInstanceOf[js.Any]
          if js.isUndefined(fieldJson) then
            if inst.isOptional then values(i) = None
            else err = Some(s"missing required field '$label'")
          else
            inst.decode(fieldJson) match
              case Right(v) => values(i) = v
              case Left(e)  => err = Some(s"$label: $e")
          i += 1
        err match
          case Some(e) => Left(e)
          case None    => Right(m.fromProduct(Tuple.fromArray(values)))

  private final class EnumToolInput[A](labels: List[String], values: List[A]) extends LibToolInput[A]:
    def schema: js.Any =
      jsObj("type" -> "string", "enum" -> js.Array(labels.map(_.asInstanceOf[js.Any])*))
    def decode(json: js.Any): Either[String, A] =
      if js.typeOf(json) == "string" then
        labels.indexOf(json.asInstanceOf[String]) match
          case -1 => Left(s"'${json.asInstanceOf[String]}' is not one of ${labels.mkString(", ")}")
          case i  => Right(values(i))
      else Left("expected string")
