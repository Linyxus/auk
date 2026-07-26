package auk.config

import scala.collection.immutable.VectorMap
import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/** Which paths a schema consumes, for unknown-key detection and (later) template
  * generation. `Subtree` is the extension point for open `Map`-valued sections.
  */
enum PathClaim:
  case Leaf(path: Path)
  case Subtree(prefix: Path)
  case Many(claims: List[PathClaim])

  /** Whether `p` is consumed by this claim. */
  def covers(p: Path): Boolean = this match
    case Leaf(path)      => p == path
    case Subtree(prefix) => p.startsWith(prefix)
    case Many(cs)        => cs.exists(_.covers(p))

/** A type class locating and decoding an `A` within a parsed [[RawConfig]],
  * rooted at a dotted path. A leaf (anything with a [[ConfigType]]) occupies a
  * single key; a case class (`derives ConfigSchema`) is a record whose fields
  * recurse under an extended path prefix — so nested case classes correspond to
  * `[scope]`s. `Option[_]` makes a field optional; a `VectorMap[String, _]` is an
  * open section whose child names come from the document. Errors accumulate
  * across the whole record tree.
  */
trait ConfigSchema[A]:
  def decode(raw: RawConfig, at: Path): Either[List[ConfigError], A]
  def isOptional: Boolean = false
  def claims(at: Path): PathClaim

object ConfigSchema:
  def apply[A](using s: ConfigSchema[A]): ConfigSchema[A] = s

  /** Bridge: any leaf [[ConfigType]] becomes a single-key schema. The decode
    * error is path-less; enclosing records prepend the field path as they unwind.
    */
  given leafSchema[A](using ct: ConfigType[A]): ConfigSchema[A] with
    def decode(raw: RawConfig, at: Path): Either[List[ConfigError], A] =
      raw.get(at) match
        case Some(rv) => ct.parse(rv).left.map(e => List(e.at(rv.line)))
        case None     => Left(List(ConfigError("missing required value")))
    def claims(at: Path): PathClaim = PathClaim.Leaf(at)

  /** An optional field. A nested record counts as present if any key exists under
    * its prefix; an absent field decodes to `None`.
    */
  given optionSchema[A](using inner: ConfigSchema[A]): ConfigSchema[Option[A]] with
    override def isOptional: Boolean = true
    def decode(raw: RawConfig, at: Path): Either[List[ConfigError], Option[A]] =
      if !raw.paths.exists(_.startsWith(at)) then Right(None)
      else inner.decode(raw, at).map(Some(_))
    def claims(at: Path): PathClaim = inner.claims(at)

  /** An open section: every key one segment below `at` names a child, decoded by
    * the inner schema. [[VectorMap]] is the carrier so that the *on-disk order*
    * of the children is part of the decoded value's type, not an accident of a
    * hash map's iteration.
    *
    * Contract:
    *   - children are the distinct segments immediately under `at`, in
    *     first-appearance order in the source;
    *   - an absent section decodes to the empty map, never an error, so a field
    *     of this type needs no `Option`;
    *   - a value sitting exactly AT `at` (e.g. `mcp.servers = x`) is an error:
    *     the name is a section, not a key;
    *   - child errors carry the child's name as their outermost path segment,
    *     and accumulate across all children.
    *
    * The section is open in its child NAMES only; each child is still closed.
    * Since the claim is a whole [[PathClaim.Subtree]], [[Config.parse]] cannot
    * see inside it, so this schema runs the unknown-key check for its children
    * itself: a misspelled field (`arg` for `args`) is an error, not a key that
    * silently does nothing.
    */
  given vectorMapSchema[A](using inner: ConfigSchema[A]): ConfigSchema[VectorMap[String, A]] with
    def decode(raw: RawConfig, at: Path): Either[List[ConfigError], VectorMap[String, A]] =
      raw.get(at) match
        case Some(rv) =>
          Left(List(ConfigError("expected a section, found a value", line = Some(rv.line))))
        case None =>
          val names = raw.paths.collect {
            case p if p.startsWith(at) && p.length > at.length => p(at.length)
          }.distinct
          val errs = List.newBuilder[ConfigError]
          var acc = VectorMap.empty[String, A]
          names.foreach { name =>
            val childAt = at :+ name
            inner.decode(raw, childAt) match
              case Right(v) => acc = acc.updated(name, v)
              case Left(es) => errs ++= es.map(_.prefix(name))
            val childClaim = inner.claims(childAt)
            raw.paths.foreach { p =>
              if p.startsWith(childAt) && !childClaim.covers(p) then
                errs += ConfigError(
                  "unknown key",
                  path = name :: p.drop(childAt.length),
                  line = raw.get(p).map(_.line)
                )
            }
          }
          val e = errs.result()
          if e.nonEmpty then Left(e) else Right(acc)
    def claims(at: Path): PathClaim = PathClaim.Subtree(at)

  // ---------------------------------------------------------------------------
  // Record derivation
  // ---------------------------------------------------------------------------

  inline def derived[A](using m: Mirror.ProductOf[A]): ConfigSchema[A] =
    buildProduct[A](m, labelsOf[m.MirroredElemLabels], summonAll[m.MirroredElemTypes])

  private inline def labelsOf[T <: Tuple]: List[String] =
    constValueTuple[T].toList.map(_.toString)

  private inline def summonAll[T <: Tuple]: List[ConfigSchema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[ConfigSchema[h]] :: summonAll[t]

  /** Public factory referenced by the inlined derivation (so the impl class can
    * stay private).
    */
  def buildProduct[A](
      m: Mirror.ProductOf[A],
      labels: List[String],
      schemas: List[ConfigSchema[?]]
  ): ConfigSchema[A] =
    new ProductConfigSchema[A](m, labels, schemas)

  private final class ProductConfigSchema[A](
      m: Mirror.ProductOf[A],
      labels: List[String],
      schemas: List[ConfigSchema[?]]
  ) extends ConfigSchema[A]:
    private val fields: List[(String, ConfigSchema[?])] = labels.zip(schemas)

    def decode(raw: RawConfig, at: Path): Either[List[ConfigError], A] =
      val errs = List.newBuilder[ConfigError]
      val values = new Array[Any](fields.length)
      var i = 0
      val it = fields.iterator
      while it.hasNext do
        val (label, sch) = it.next()
        sch.decode(raw, at :+ label) match
          case Right(v) => values(i) = v
          case Left(es) => errs ++= es.map(_.prefix(label))
        i += 1
      val e = errs.result()
      if e.nonEmpty then Left(e)
      else Right(m.fromProduct(Tuple.fromArray(values)))

    def claims(at: Path): PathClaim =
      PathClaim.Many(fields.map((label, sch) => sch.claims(at :+ label)))

/** Entry point: parse config text and decode it into `A`. */
object Config:
  /** Parse `text` and decode it into `A`. Parse errors short-circuit; otherwise
    * decode errors and unknown-key errors are reported together.
    */
  def parse[A](text: String)(using sch: ConfigSchema[A]): Either[List[ConfigError], A] =
    ConfigParser.parse(text) match
      case Left(errs) => Left(errs)
      case Right(raw) =>
        val claim = sch.claims(Nil)
        val unknown = raw.paths.iterator
          .filterNot(p => claim.covers(p))
          .map(p => ConfigError(s"unknown key '${p.dotted}'", line = raw.get(p).map(_.line)))
          .toList
        sch.decode(raw, Nil) match
          case Left(errs)               => Left(errs ++ unknown)
          case Right(_) if unknown.nonEmpty => Left(unknown)
          case Right(a)                 => Right(a)
