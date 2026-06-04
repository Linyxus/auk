package auk.config

/** A dot-separated config key as a list of segments, e.g. `List("model","id")`. */
type Path = List[String]

extension (p: Path)
  /** Render a path as a dotted string, e.g. `model.id`. */
  def dotted: String = p.mkString(".")

/** The raw, un-typed value captured for a key by the syntactic parser.
  *
  * The two shapes are kept distinct so the *type* (a [[ConfigType]]) decides how
  * to interpret a value — a scalar type accepts only [[Inline]], while a list
  * type accepts either form. The parser itself stays type-agnostic.
  */
enum RawValue:
  /** RHS text after `=` on the same line, trimmed. May be empty. */
  case Inline(text: String, line: Int)

  /** Indented continuation lines following a `key =` with an empty inline RHS. */
  case Block(lines: List[String], line: Int)

object RawValue:
  extension (rv: RawValue)
    /** The 1-based source line where this value began. */
    def line: Int = rv match
      case RawValue.Inline(_, l) => l
      case RawValue.Block(_, l)  => l

/** An insertion-ordered map from dotted [[Path]] to its [[RawValue]]. */
final class RawConfig private (val entries: Vector[(Path, RawValue)]):
  private val byPath: Map[Path, RawValue] = entries.toMap

  /** The raw value at `path`, if present. */
  def get(path: Path): Option[RawValue] = byPath.get(path)

  /** All keys, in the order they appeared in the source. */
  def paths: Vector[Path] = entries.map(_._1)

object RawConfig:
  def apply(entries: Vector[(Path, RawValue)]): RawConfig = new RawConfig(entries)
  val empty: RawConfig = new RawConfig(Vector.empty)
