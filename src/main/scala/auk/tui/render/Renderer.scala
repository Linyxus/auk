package auk.tui.render

import java.util.Arrays

/** The stateful rendering core.
  *
  * Drives an inline (non-alt-screen) "static + live" model: each frame may
  * commit zero or more finalized lines into the terminal's native scrollback
  * (printed once, never touched again) and then repaints a small *live region*
  * below them by diffing a packed cell grid against the previous frame and
  * emitting the minimal cursor moves + style-coalesced writes. The whole frame
  * goes out as one atomic write wrapped in DEC-2026 synchronized output, so the
  * user never sees a half-drawn frame.
  *
  * Not thread-safe: [[render]] is called only from the runtime's frame step.
  *
  * @param out the sink for the single per-frame write (e.g. `terminal.write`).
  * @param altScreenSetup sequence embedded right after `?1049h` on every entry into
  *   the alternate screen buffer, and `altScreenTeardown` right before `?1049l` on
  *   every exit. Sourced from `Terminal.altScreenSetup/altScreenTeardown` (the kitty
  *   keyboard push/pop on capable TTYs, empty otherwise) because that enhancement is
  *   tracked per screen buffer and so must be re-pushed inside the alt buffer and
  *   popped as it is left. Empty defaults keep headless/test streams byte-identical.
  */
final class Renderer(out: String => Unit, altScreenSetup: String = "", altScreenTeardown: String = ""):
  private val pool = StylePool()
  private var prev: Surface = Surface.Empty
  private var lastWidth: Int = -1
  private var started: Boolean = false
  // Physical row of the cursor, within the live region, between frames. We end
  // every frame at the live region's bottom-left, so the next frame normalizes
  // to the origin with one `cursorUp`.
  private var prevCursorRow: Int = 0

  // Alternate-screen (fullscreen) state, kept entirely separate from the inline
  // state above: an episode never touches `prev`/`lastWidth`/`started`/
  // `prevCursorRow`, so returning to inline is an ordinary incremental frame.
  // `?1049` saves and restores the cursor, so the restored inline cursor is
  // exactly where it was parked before the episode.
  private var altActive: Boolean = false
  private var altPrev: Surface = Surface.Empty
  private var altWidth: Int = -1
  private var altHeight: Int = -1
  // One reusable frame buffer: `setLength(0)` (after the no-op fast path) keeps
  // its high-water capacity, so heavy paints don't re-grow it every frame.
  private val frameSb = new StringBuilder(1024)

  /** Mutable virtual cursor threaded through one frame's emission. */
  private final class Cursor(var row: Int, var col: Int)

  /** Render one frame: commit `committed` lines, then repaint the `live` region
    * at `width` columns.
    *
    * Normally `committed` is just the new tail (the caller tracks what's already
    * flushed) and is appended incrementally. On a terminal resize the caller
    * sets `hardReset` and passes the *whole* committed transcript: the renderer
    * clears the screen and scrollback and reprints everything re-laid at the new
    * width (committed lines were printed once at the old width and the terminal
    * does not reflow them, so the history must be re-emitted). `overlay`, when
    * present, is composited over the live region before diffing. */
  def render(
      width: Int,
      committed: Vector[StyledLine],
      live: Vector[StyledLine],
      hardReset: Boolean = false,
      overlay: Option[Vector[StyledLine]] = None
  ): Unit =
    val base = Surface.build(width, live, pool)
    val next =
      overlay match
        case Some(lines) if lines.nonEmpty => composeOverlay(base, Surface.build(width, lines, pool))
        case _                            => base

    // Returning from a fullscreen episode: leave the alt buffer this frame. The
    // primary buffer still matches `prev` (untouched during the episode, and
    // `?1049l` restores the cursor), so the inline paint below is an ordinary
    // incremental frame — we just force it (skip the no-op fast path) so the exit
    // sequence is always emitted, even when the live grid is unchanged.
    val leavingAlt = altActive
    if leavingAlt then
      altActive = false
      altPrev = Surface.Empty

    // No-op fast path: nothing to commit and the live grid is byte-identical.
    if !leavingAlt && !hardReset && committed.isEmpty && started && width == lastWidth && sameGrid(prev, next) then
      return

    val sb = frameSb
    sb.setLength(0) // reuse the buffer; cleared only past the no-op return above
    // Pop the alt buffer's keyboard enhancement before `?1049l`, both inside this
    // one frame write, so the enhancement stack is balanced as we leave the buffer.
    if leavingAlt then sb.append(altScreenTeardown).append(Ansi.AltScreenExit)
    sb.append(Ansi.SyncBegin).append(Ansi.HideCursor)
    val cur = Cursor(prevCursorRow, 0)

    if !started then firstPaint(sb, cur, committed, next)
    else if hardReset then hardResetRepaint(sb, cur, committed, next)
    else if committed.nonEmpty then commitThenRepaint(sb, cur, committed, next)
    else if width != lastWidth then resizeRepaint(sb, cur, next)
    else diffInPlace(sb, cur, next)

    parkCursor(sb, cur, next)
    sb.append(Ansi.Reset).append(Ansi.SyncEnd)
    out(sb.toString)

    prev = next
    lastWidth = width
    started = true
    prevCursorRow = math.max(0, next.height - 1)

  /** Paint a fullscreen frame into the alternate screen buffer at `rows` × `width`.
    * The first call switches to the alt buffer (`?1049h`, saving the primary
    * cursor) and clears it; subsequent same-dimension frames diff against the
    * previous alt frame and emit a minimal patch, re-anchored each frame at the
    * absolute screen origin; a dimension change (resize) clears and fully repaints.
    * The inline state is never touched, so the eventual return to inline is a
    * clean incremental frame. `lines` is clipped/padded to exactly `rows` rows. */
  def renderFullscreen(width: Int, rows: Int, lines: Vector[StyledLine]): Unit =
    val next = fullscreenSurface(width, rows, lines)
    val sameDims = altActive && width == altWidth && rows == altHeight

    // No-op fast path: an established alt frame whose grid is byte-identical.
    if sameDims && sameGrid(altPrev, next) then return

    val sb = frameSb
    sb.setLength(0)
    sb.append(Ansi.SyncBegin).append(Ansi.HideCursor)
    val cur = Cursor(0, 0)

    if !altActive then
      // Enter the alt buffer (saving the primary cursor), push its keyboard
      // enhancement (kitty tracks it per buffer), clear it, full paint. The setup
      // rides inside this sync-wrapped frame, immediately after `?1049h`.
      sb.append(Ansi.AltScreenEnter).append(altScreenSetup).append(Ansi.Reset).append(Ansi.ClearScreen).append(Ansi.CursorHome)
      altActive = true
      altPrev = Surface.Empty
      paintFresh(sb, cur, next)
    else if !sameDims then
      // Resized while fullscreen: clear and repaint the whole alt buffer.
      sb.append(Ansi.Reset).append(Ansi.CursorHome).append(Ansi.ClearScreen)
      paintFresh(sb, cur, next)
    else
      // Steady state: re-anchor at the absolute origin, then per-cell diff.
      sb.append(Ansi.CursorHome)
      diffCells(sb, cur, next, altPrev)

    sb.append(Ansi.Reset).append(Ansi.SyncEnd)
    out(sb.toString)

    altPrev = next
    altWidth = width
    altHeight = rows

  /** Leave the alt buffer if active (idempotent) — for teardown, so quitting from
    * a fullscreen view never strands the terminal in the alternate buffer. */
  def exitFullscreen(): Unit =
    if altActive then
      // Pop the alt buffer's keyboard enhancement before `?1049l`, in one write.
      out(altScreenTeardown + Ansi.AltScreenExit)
      altActive = false
      altPrev = Surface.Empty

  /** Lay `lines` into a surface of exactly `rows` × `width` — clipped if the app
    * produced too many lines (a safety net; it aims to produce exactly `rows`)
    * and padded with blank rows if too few, so the alt diff always compares
    * like-sized grids. */
  private def fullscreenSurface(width: Int, rows: Int, lines: Vector[StyledLine]): Surface =
    val clipped = if lines.length > rows then lines.take(rows) else lines
    val base = Surface.build(width, clipped, pool)
    if base.width == width && base.height == rows then base
    else
      val cells = new Array[Long](math.max(0, width) * math.max(0, rows))
      Arrays.fill(cells, Cell.Blank)
      if base.cells.nonEmpty then System.arraycopy(base.cells, 0, cells, 0, math.min(base.cells.length, cells.length))
      new Surface(math.max(0, width), math.max(0, rows), cells)

  /* ---- Frame strategies ---- */

  /** First ever paint: print committed lines (if any) onto a fresh line, then
    * draw the live region below. */
  private def firstPaint(sb: StringBuilder, cur: Cursor, committed: Vector[StyledLine], next: Surface): Unit =
    sb.append(Ansi.CarriageReturn) // ensure column 0 of the current line
    cur.row = 0; cur.col = 0
    printCommitted(sb, committed)
    paintFresh(sb, cur, next)

  /** Commit new scrollback lines, then repaint the live region fresh below them.
    * The old live region is erased first (inside the sync wrap) so committed
    * text lands at the right place with no flash. Also the correct path for a
    * commit that coincides with a resize. */
  private def commitThenRepaint(sb: StringBuilder, cur: Cursor, committed: Vector[StyledLine], next: Surface): Unit =
    moveTo(sb, cur, 0, 0)          // to the old live region's top-left
    sb.append(Ansi.Reset).append(Ansi.EraseToEos) // wipe the old live region
    printCommitted(sb, committed) // scroll committed lines into history
    cur.row = 0; cur.col = 0       // the line after them is the new origin
    paintFresh(sb, cur, next)

  /** Terminal resized: clear the screen and scrollback, then reprint the whole
    * transcript (all committed lines, re-laid at the new width by the caller)
    * followed by the live region. This re-wraps committed history correctly and
    * re-establishes a known cursor anchor (the prior relative-move state is
    * invalid once the terminal reflows). */
  private def hardResetRepaint(sb: StringBuilder, cur: Cursor, committed: Vector[StyledLine], next: Surface): Unit =
    sb.append(Ansi.Reset).append(Ansi.CursorHome).append(Ansi.ClearScreen).append(Ansi.ClearScrollback)
    cur.row = 0; cur.col = 0
    printCommitted(sb, committed)
    paintFresh(sb, cur, next)

  /** Width changed (without a full reset): wipe and repaint the live region
    * fresh at the new width. */
  private def resizeRepaint(sb: StringBuilder, cur: Cursor, next: Surface): Unit =
    moveTo(sb, cur, 0, 0)
    sb.append(Ansi.Reset).append(Ansi.EraseToEos)
    cur.row = 0; cur.col = 0
    paintFresh(sb, cur, next)

  /** The common case: per-cell diff of `next` against `prev` in place. */
  private def diffInPlace(sb: StringBuilder, cur: Cursor, next: Surface): Unit =
    moveTo(sb, cur, 0, 0)

    // Grow taller: CursorDown clamps at the viewport bottom, so create the new
    // rows by scrolling with real newlines before addressing them.
    if next.height > prev.height then
      moveTo(sb, cur, math.max(0, prev.height - 1), 0)
      var k = math.max(prev.height, 1)
      while k < next.height do { sb.append("\r\n"); k += 1 }
      cur.row = next.height - 1; cur.col = 0
      moveTo(sb, cur, 0, 0)

    diffCells(sb, cur, next, prev)

    // Shrink: erase the rows that no longer exist.
    if next.height < prev.height then
      moveTo(sb, cur, next.height, 0)
      sb.append(Ansi.Reset).append(Ansi.EraseToEos)

  /** Per-cell diff of `next` against `prevS`, emitting minimal cursor moves and
    * style-coalesced runs. Assumes `cur` already points at the grid origin and
    * the two surfaces share dimensions (or that height changes are handled by the
    * caller). Shared by the inline in-place diff and the fullscreen alt diff. */
  private def diffCells(sb: StringBuilder, cur: Cursor, next: Surface, prevS: Surface): Unit =
    var curStyle = -1 // unknown on-screen SGR; first emit forces a full set
    var row = 0
    while row < next.height do
      var col = 0
      while col < next.width do
        if next.at(row, col) == prevS.at(row, col) then col += 1
        else
          moveTo(sb, cur, row, col)
          curStyle = emitRun(sb, cur, next, prevS, row, col, curStyle)
          col = cur.col
      row += 1

  /* ---- Emission helpers ---- */

  /** Print committed lines, each scrolling into native scrollback. Leaves the
    * cursor at column 0 of a fresh line (the new live-region origin). */
  private def printCommitted(sb: StringBuilder, committed: Vector[StyledLine]): Unit =
    var i = 0
    while i < committed.length do
      sb.append(committed(i).render).append("\r\n")
      i += 1

  /** Draw the whole live region from the current line down, scrolling with
    * `\r\n` between rows. Assumes the cursor is at column 0 of row 0. */
  private def paintFresh(sb: StringBuilder, cur: Cursor, next: Surface): Unit =
    sb.append(Ansi.Reset)
    var curStyle = 0
    var row = 0
    while row < next.height do
      if row > 0 then { sb.append("\r\n"); cur.row = row; cur.col = 0 }
      curStyle = emitFullRow(sb, cur, next, row, curStyle)
      row += 1

  /** Emit a full row (used by fresh paints): all cells up to the last
    * non-default one, coalescing style runs and skipping wide-glyph spacers. */
  private def emitFullRow(sb: StringBuilder, cur: Cursor, s: Surface, row: Int, startStyle: Int): Int =
    var last = -1
    var c = 0
    while c < s.width do
      if s.at(row, c) != Cell.Blank then last = c
      c += 1
    var curStyle = startStyle
    var col = 0
    while col <= last do
      val cell = s.at(row, col)
      if Cell.isSpacer(cell) then col += 1
      else
        val sid = Cell.styleId(cell)
        if sid != curStyle then { sb.append(pool.transition(curStyle, sid)); curStyle = sid }
        sb.appendAll(Character.toChars(Cell.codePoint(cell)))
        Cell.widthClass(cell) match
          case Cell.Wide      => cur.col += 2; col += 2
          case Cell.ZeroWidth => col += 1
          case _              => cur.col += 1; col += 1
    curStyle

  /** Emit a contiguous run of changed cells starting at `(row, startCol)`,
    * stopping at the first cell equal to `prevS`. Returns the SGR style id left
    * active; advances `cur.col` to one past the run. */
  private def emitRun(sb: StringBuilder, cur: Cursor, next: Surface, prevS: Surface, row: Int, startCol: Int, startStyle: Int): Int =
    var curStyle = startStyle
    var col = startCol
    while col < next.width && next.at(row, col) != prevS.at(row, col) do
      val cell = next.at(row, col)
      if Cell.isSpacer(cell) then
        // Unreachable in normal operation (a dirty spacer's wide lead is always
        // dirty too and consumes it); skip defensively without emitting.
        col += 1
      else
        val sid = Cell.styleId(cell)
        if sid != curStyle then
          sb.append(if curStyle < 0 then pool.setSequence(sid) else pool.transition(curStyle, sid))
          curStyle = sid
        sb.appendAll(Character.toChars(Cell.codePoint(cell)))
        Cell.widthClass(cell) match
          case Cell.Wide      => cur.col += 2; col += 2
          case Cell.ZeroWidth => col += 1
          case _              => cur.col += 1; col += 1
    curStyle

  /** Move the cursor below the live region's bottom-left so the next frame has
    * a deterministic anchor. The real cursor stays hidden; the visible caret is
    * the reverse-video cell the app draws. */
  private def parkCursor(sb: StringBuilder, cur: Cursor, next: Surface): Unit =
    moveTo(sb, cur, math.max(0, next.height - 1), 0)

  /** Relative cursor move to `(toRow, toCol)`, updating `cur`. Horizontal moves
    * go through `\r` (+ right) so they are immune to pending-wrap state. */
  private def moveTo(sb: StringBuilder, cur: Cursor, toRow: Int, toCol: Int): Unit =
    // Append the CSI parts directly. The deltas here are always strictly
    // non-zero (dy) / positive (column), so this matches Ansi.cursorUp/Down/Right
    // exactly without their `n <= 0` guard or the intermediate String each built.
    val dy = toRow - cur.row
    if dy < 0 then sb.append(Ansi.CSI).append(-dy).append('A')
    else if dy > 0 then sb.append(Ansi.CSI).append(dy).append('B')
    cur.row = toRow
    if toCol == 0 then { sb.append(Ansi.CarriageReturn); cur.col = 0 }
    else if toCol > cur.col then { sb.append(Ansi.CSI).append(toCol - cur.col).append('C'); cur.col = toCol }
    else if toCol < cur.col then { sb.append(Ansi.CarriageReturn).append(Ansi.CSI).append(toCol).append('C'); cur.col = toCol }

  private def sameGrid(a: Surface, b: Surface): Boolean =
    a.width == b.width && a.height == b.height && Arrays.equals(a.cells, b.cells)

  /** Center a sparse overlay surface over the live surface. Default blank cells
    * in the overlay are transparent; styled spaces are opaque, so callers can
    * create a rectangular panel with a background colour. */
  private def composeOverlay(base: Surface, overlay: Surface): Surface =
    if base.width <= 0 || overlay.height == 0 then base
    else
      visibleBounds(overlay) match
        case None => base
        case Some((minRow, maxRow, minCol, maxCol)) =>
          val overlayHeight = maxRow - minRow + 1
          val overlayWidth = maxCol - minCol + 1
          val height = if base.height == 0 then overlayHeight else base.height
          val cells = new Array[Long](base.width * height)
          Arrays.fill(cells, Cell.Blank)
          if base.cells.nonEmpty then System.arraycopy(base.cells, 0, cells, 0, base.cells.length)
          val top = math.max(0, (height - overlayHeight) / 2)
          val left = math.max(0, (base.width - overlayWidth) / 2)

          var row = minRow
          while row <= maxRow do
            var col = minCol
            while col <= maxCol do
              val cell = overlay.at(row, col)
              val dstRow = top + row - minRow
              val dstCol = left + col - minCol
              if cell != Cell.Blank && dstRow >= 0 && dstRow < height && dstCol >= 0 && dstCol < base.width then
                cells(dstRow * base.width + dstCol) = cell
              col += 1
            row += 1

          new Surface(base.width, height, cells)

  private def visibleBounds(s: Surface): Option[(Int, Int, Int, Int)] =
    var minRow = Int.MaxValue
    var maxRow = -1
    var minCol = Int.MaxValue
    var maxCol = -1
    var row = 0
    while row < s.height do
      var col = 0
      while col < s.width do
        if s.at(row, col) != Cell.Blank then
          if row < minRow then minRow = row
          if row > maxRow then maxRow = row
          if col < minCol then minCol = col
          if col > maxCol then maxCol = col
        col += 1
      row += 1

    if maxRow < 0 then None else Some((minRow, maxRow, minCol, maxCol))
