package auk.config

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/** A type class describing how to parse a single config *leaf* value (a
  * [[RawValue]]) into an `A`, plus a human-readable [[describe]] of the type for
  * error messages.
  *
  * Instances exist for the common scalars and for `List[A]`. An instance for a
  * simple `enum` (a sum of parameterless cases) can be obtained with
  * `derives ConfigType`, matching values against case labels case-insensitively:
  *
  * {{{
  * enum Provider derives ConfigType:
  *   case OpenAI, Anthropic, OpenRouter
  * }}}
  *
  * Records (case classes) are NOT leaves — they are handled by [[ConfigSchema]].
  * Union values use the explicit [[ConfigType.oneOf]] combinator (a
  * `given ConfigType[A | B]` is not derivable; see the project plan).
  */
trait ConfigType[A]:
  def parse(raw: RawValue): Either[ConfigError, A]
  def describe: String

object ConfigType:
  def apply[A](using ct: ConfigType[A]): ConfigType[A] = ct

  /** Build an instance from a description and a parse function. */
  def instance[A](desc: String)(f: RawValue => Either[ConfigError, A]): ConfigType[A] =
    new ConfigType[A]:
      def describe: String = desc
      def parse(raw: RawValue): Either[ConfigError, A] = f(raw)

  /** A scalar leaf: accepts only an inline value, rejecting a block (list). The
    * source line is stamped onto any error the body produces.
    */
  private def scalar[A](desc: String)(f: String => Either[ConfigError, A]): ConfigType[A] =
    instance(desc) {
      case RawValue.Inline(t, ln) => f(t).left.map(_.at(ln))
      case RawValue.Block(_, ln) =>
        Left(ConfigError(s"expected $desc, got a list", line = Some(ln)))
    }

  // ---------------------------------------------------------------------------
  // String helpers
  // ---------------------------------------------------------------------------

  private def unescape(s: String): String =
    val sb = new StringBuilder
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if c == '\\' && i + 1 < s.length then
        val n = s.charAt(i + 1)
        if n == '"' || n == '\\' then
          sb.append(n)
          i += 2
        else
          sb.append(c)
          i += 1
      else
        sb.append(c)
        i += 1
    sb.toString

  /** A string value: a surrounding pair of double quotes is optional and, when
    * present, is stripped (with `\"`/`\\` unescaped). Otherwise the whole
    * (already trimmed) remainder is taken verbatim, spaces included.
    */
  private def parseString(t: String): String =
    if t.length >= 2 && t.startsWith("\"") && t.endsWith("\"") then
      unescape(t.substring(1, t.length - 1))
    else t

  /** Quote-aware whitespace split, used for inline lists. A `"`-quoted run keeps
    * its spaces and may contain `\"`/`\\`; an unterminated quote is an error.
    */
  def splitInline(s: String, line: Int): Either[ConfigError, List[String]] =
    val items = List.newBuilder[String]
    val sb = new StringBuilder
    var i = 0
    var inQuote = false
    var hasToken = false
    while i < s.length do
      val c = s.charAt(i)
      if inQuote then
        if c == '\\' && i + 1 < s.length then
          val n = s.charAt(i + 1)
          if n == '"' || n == '\\' then
            sb.append(n)
            i += 2
          else
            sb.append(c)
            i += 1
        else if c == '"' then
          inQuote = false
          i += 1
        else
          sb.append(c)
          i += 1
      else if c == '"' then
        inQuote = true
        hasToken = true
        i += 1
      else if c == ' ' || c == '\t' then
        if hasToken then
          items += sb.toString
          sb.clear()
          hasToken = false
        i += 1
      else
        sb.append(c)
        hasToken = true
        i += 1
    if inQuote then Left(ConfigError("unterminated quote in value", line = Some(line)))
    else
      if hasToken then items += sb.toString
      Right(items.result())

  // ---------------------------------------------------------------------------
  // Scalar givens
  // ---------------------------------------------------------------------------

  given ConfigType[String] = scalar("string")(t => Right(parseString(t)))

  given ConfigType[Int] =
    scalar("integer")(t => t.toIntOption.toRight(ConfigError(s"'$t' is not an integer")))

  given ConfigType[Long] =
    scalar("integer")(t => t.toLongOption.toRight(ConfigError(s"'$t' is not an integer")))

  given ConfigType[Double] =
    scalar("number")(t => t.toDoubleOption.toRight(ConfigError(s"'$t' is not a number")))

  given ConfigType[Boolean] =
    scalar("boolean")(t => t.toBooleanOption.toRight(ConfigError(s"'$t' is not a boolean")))

  // ---------------------------------------------------------------------------
  // List
  // ---------------------------------------------------------------------------

  given listConfigType[A](using inner: ConfigType[A]): ConfigType[List[A]] =
    def parseElems(raws: List[RawValue]): Either[ConfigError, List[A]] =
      val buf = List.newBuilder[A]
      var err: Option[ConfigError] = None
      var i = 0
      val it = raws.iterator
      while it.hasNext && err.isEmpty do
        inner.parse(it.next()) match
          case Right(v) => buf += v
          case Left(e)  => err = Some(e.prefix(s"[$i]"))
        i += 1
      err.toLeft(buf.result())

    instance(s"list of ${inner.describe}") {
      case RawValue.Block(lines, ln) =>
        parseElems(lines.map(l => RawValue.Inline(l, ln)))
      case RawValue.Inline(t, ln) =>
        splitInline(t, ln).flatMap(parts => parseElems(parts.map(p => RawValue.Inline(p, ln))))
    }

  // ---------------------------------------------------------------------------
  // Union (explicit, ordered)
  // ---------------------------------------------------------------------------

  /** Try each alternative in order, returning the first success. On total
    * failure the error lists every alternative's [[describe]]. Put the more
    * specific type first (e.g. `oneOf(int, string)`) so overlapping values
    * resolve deterministically.
    */
  def oneOf[A](first: ConfigType[? <: A], rest: ConfigType[? <: A]*): ConfigType[A] =
    val all = (first +: rest).toList
    instance(all.map(_.describe).mkString(" | ")) { raw =>
      def loop(cs: List[ConfigType[? <: A]]): Either[ConfigError, A] = cs match
        case Nil =>
          Left(ConfigError(
            s"value did not match any of: ${all.map(_.describe).mkString(", ")}",
            line = Some(raw.line)
          ))
        case c :: tl =>
          c.parse(raw) match
            case Right(v) => Right(v)
            case Left(_)  => loop(tl)
      loop(all)
    }

  // ---------------------------------------------------------------------------
  // Enum derivation (sum of parameterless cases only)
  // ---------------------------------------------------------------------------

  inline def derived[A](using m: Mirror.SumOf[A]): ConfigType[A] =
    buildEnum[A](
      labelsOf[m.MirroredElemLabels],
      summonSingletons[m.MirroredElemTypes].asInstanceOf[List[A]]
    )

  private inline def labelsOf[T <: Tuple]: List[String] =
    constValueTuple[T].toList.map(_.toString)

  /** Singletons of a sum's parameterless cases, in declaration order. The
    * `EmptyTuple` refinement makes a case with parameters a compile error.
    */
  private inline def summonSingletons[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        val mh = summonInline[Mirror.ProductOf[h] { type MirroredElemTypes = EmptyTuple }]
        mh.fromProduct(EmptyTuple) :: summonSingletons[t]

  /** Public factory referenced by the inlined derivation. */
  def buildEnum[A](labels: List[String], values: List[A]): ConfigType[A] =
    instance(labels.mkString("|")) {
      case RawValue.Inline(t, ln) =>
        labels.indexWhere(_.equalsIgnoreCase(t)) match
          case -1 =>
            Left(ConfigError(s"'$t' is not one of ${labels.mkString(", ")}", line = Some(ln)))
          case i => Right(values(i))
      case RawValue.Block(_, ln) =>
        Left(ConfigError(s"expected one of ${labels.mkString(", ")}, got a list", line = Some(ln)))
    }
