package auk.tui.render

/** A rectangular grid of packed cells — the renderer's snapshot of the live
  * region for one frame. Row-major; `width * height` cells. */
final class Surface(val width: Int, val height: Int, val cells: Array[Long]):
  /** The cell at `(row, col)`, or [[Cell.Blank]] if out of bounds (so a diff
    * against a smaller previous frame reads cleanly). */
  def at(row: Int, col: Int): Long =
    if row >= 0 && row < height && col >= 0 && col < width then cells(row * width + col)
    else Cell.Blank

object Surface:
  val Empty: Surface = new Surface(0, 0, Array.emptyLongArray)

  /** Lay styled lines into a `width`-column cell grid (one row per line).
    *
    * Each codepoint becomes a cell: width-2 glyphs take a [[Cell.Wide]] lead +
    * [[Cell.Spacer]] tail; zero-width marks are dropped (a documented
    * limitation — our content uses precomposed characters); content past
    * `width` is clipped (the live region is sized to fit). Style ids are
    * interned once per span via `pool`.
    */
  def build(width: Int, lines: Vector[StyledLine], pool: StylePool): Surface =
    val height = lines.length
    if width <= 0 || height == 0 then return new Surface(math.max(width, 0), height, Array.fill(math.max(width, 0) * height)(Cell.Blank))

    val cells = Array.fill(width * height)(Cell.Blank)
    var row = 0
    while row < height do
      val base = row * width
      var col = 0
      val spans = lines(row).spans
      var si = 0
      while si < spans.length && col < width do
        val sp = spans(si)
        val sid = pool.intern(sp.style)
        val text = sp.text
        val n = text.length
        var i = 0
        while i < n && col < width do
          val cp = text.codePointAt(i)
          i += Character.charCount(cp)
          Width.displayWidth(cp) match
            case 0 => () // zero-width mark: dropped
            case 2 =>
              if col + 2 <= width then
                cells(base + col) = Cell.pack(cp, sid, Cell.Wide)
                cells(base + col + 1) = Cell.pack(' '.toInt, sid, Cell.Spacer)
                col += 2
              else
                // a wide glyph won't fit the last column: pad and stop the line
                cells(base + col) = Cell.pack(' '.toInt, sid, Cell.Narrow)
                col = width
            case _ =>
              cells(base + col) = Cell.pack(cp, sid, Cell.Narrow)
              col += 1
        si += 1
      row += 1
    new Surface(width, height, cells)
