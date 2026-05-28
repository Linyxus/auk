package auk.tui.render

/** A run of text sharing one visual [[Style]]. Text is raw (no embedded escape
  * sequences); the style is applied at render time. */
final case class Span(text: String, style: Style)

/** One physical terminal row as a sequence of styled spans.
  *
  * The layout engine emits `Vector[StyledLine]`. The renderer turns the live
  * region's lines into cells (per-cell diff), while committed lines are turned
  * into a one-shot ANSI string via [[render]] and printed once into scrollback.
  */
final case class StyledLine(spans: Vector[Span]):
  /** The display width of the whole line in columns. */
  def width: Int = spans.iterator.map(s => Width.stringWidth(s.text)).sum

  /** The line's text with styling stripped. */
  def plain: String = spans.iterator.map(_.text).mkString

  def isBlank: Boolean = spans.forall(_.text.isEmpty)

  /** A self-contained ANSI string: each span sets its style from a reset, and
    * the line ends with a reset so nothing leaks into the next line. */
  def render: String =
    val sb = new StringBuilder
    var i = 0
    val arr = spans
    while i < arr.length do
      val sp = arr(i)
      if sp.text.nonEmpty then sb.append(sp.style.setSequence).append(sp.text)
      i += 1
    sb.append(Ansi.Reset)
    sb.toString

object StyledLine:
  val empty: StyledLine = StyledLine(Vector.empty)

  def text(s: String, style: Style = Style.Default): StyledLine =
    StyledLine(Vector(Span(s, style)))
