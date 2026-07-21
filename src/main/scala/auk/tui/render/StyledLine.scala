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

  /** Return `line` with `f` applied to the style of every glyph whose display
    * cells intersect `[fromCell, untilCell)` (0-based display columns). Spans are
    * split at the boundaries and adjacent same-style runs re-coalesced, so the
    * result is display-width identical to the input. A wide glyph straddling
    * either boundary is restyled whole (its cells are treated as one unit). An
    * empty or inverted range (`untilCell <= fromCell`) returns `line` unchanged. */
  def restyleCells(line: StyledLine, fromCell: Int, untilCell: Int, f: Style => Style): StyledLine =
    if untilCell <= fromCell then line
    else
      val out = Vector.newBuilder[Span]
      val cur = new StringBuilder
      var curStyle: Style = Style.Default
      var have = false
      var cell = 0
      for sp <- line.spans do
        var i = 0
        val n = sp.text.length
        while i < n do
          val cp = sp.text.codePointAt(i)
          val cc = Character.charCount(cp)
          val w = Width.displayWidth(cp)
          // The glyph spans cells `[cell, cell + max(w, 1))`; it intersects the
          // range when that span overlaps `[fromCell, untilCell)`. `max(w, 1)`
          // keeps a zero-width mark from being silently dropped at a boundary.
          val inRange = cell < untilCell && cell + math.max(w, 1) > fromCell
          val st = if inRange then f(sp.style) else sp.style
          if have && st != curStyle then { out += Span(cur.toString, curStyle); cur.setLength(0) }
          curStyle = st
          have = true
          cur.append(new String(Character.toChars(cp)))
          cell += w
          i += cc
      if have then out += Span(cur.toString, curStyle)
      StyledLine(out.result())
