package auk.tui.render

import scala.collection.mutable

/** A minimal ANSI terminal emulator that understands exactly the sequences the
  * [[Renderer]] emits (cursor moves, CR/LF with scrolling, erase-to-EOS/EOL,
  * and printable text; SGR / sync / cursor-visibility are no-ops for the text
  * grid). Lets the renderer's output be asserted as visible text + scrollback,
  * which is the only tractable way to catch cursor-drift bugs. */
final class TermEmu(cols: Int, rows: Int):
  private val grid = Array.fill(rows, cols)(" ")
  val scrollback = mutable.ArrayBuffer.empty[String]
  var row = 0
  var col = 0

  def viewport: Vector[String] = (0 until rows).map(r => rstrip(grid(r).mkString)).toVector
  def line(r: Int): String = rstrip(grid(r).mkString)

  private def rstrip(s: String): String = s.replaceAll("\\s+$", "")

  def feed(s: String): Unit =
    var i = 0
    while i < s.length do
      val ch = s.charAt(i)
      if ch == '' then
        i += 1
        if i < s.length && s.charAt(i) == '[' then
          i += 1
          val params = new StringBuilder
          while i < s.length && !Character.isLetter(s.charAt(i)) do { params.append(s.charAt(i)); i += 1 }
          val fin = if i < s.length then s.charAt(i) else ' '
          i += 1
          applyCsi(params.toString, fin)
      else if ch == '\r' then { col = 0; i += 1 }
      else if ch == '\n' then { lineFeed(); i += 1 }
      else
        val cp = s.codePointAt(i)
        i += Character.charCount(cp)
        putChar(cp)

  private def num(p: String, default: Int): Int =
    val digits = p.filter(_.isDigit)
    if digits.isEmpty then default else digits.toInt

  private def applyCsi(params: String, fin: Char): Unit =
    if params.startsWith("?") then () // sync / cursor visibility: ignore
    else
      fin match
        case 'A' => row = math.max(0, row - num(params, 1))
        case 'B' => row = math.min(rows - 1, row + num(params, 1))
        case 'C' => col = math.min(cols, col + num(params, 1))
        case 'D' => col = math.max(0, col - num(params, 1))
        case 'm' => () // SGR: ignored for the text grid
        case 'H' => row = 0; col = 0 // home (we only emit the bare form)
        case 'J' =>
          num(params, 0) match
            case 0 => eraseToEos()
            case 2 => clearScreen()
            case 3 => scrollback.clear()
            case _ => ()
        case 'K' => if num(params, 0) == 0 then eraseToEol()
        case _   => ()

  private def lineFeed(): Unit =
    row += 1
    if row >= rows then
      scrollback += rstrip(grid(0).mkString)
      var r = 1
      while r < rows do { grid(r - 1) = grid(r); r += 1 }
      grid(rows - 1) = Array.fill(cols)(" ")
      row = rows - 1

  private def putChar(cp: Int): Unit =
    val w = Width.displayWidth(cp)
    if w == 0 then return
    if col + w > cols then { col = 0; lineFeed() }
    grid(row)(col) = new String(Character.toChars(cp))
    if w == 2 && col + 1 < cols then grid(row)(col + 1) = ""
    col += w

  private def clearScreen(): Unit =
    var r = 0
    while r < rows do { grid(r) = Array.fill(cols)(" "); r += 1 }

  private def eraseToEol(): Unit =
    var c = col
    while c < cols do { grid(row)(c) = " "; c += 1 }

  private def eraseToEos(): Unit =
    eraseToEol()
    var r = row + 1
    while r < rows do { grid(r) = Array.fill(cols)(" "); r += 1 }

class RendererSuite extends munit.FunSuite:

  private def sl(s: String): StyledLine = StyledLine.text(s)
  private def lines(ss: String*): Vector[StyledLine] = ss.toVector.map(sl)

  /** Wire a renderer to a fresh emulator; also count writes and keep the last. */
  private def setup(cols: Int = 24, rows: Int = 8): (Renderer, TermEmu, () => Int, () => String) =
    val emu = TermEmu(cols, rows)
    var writes = 0
    var last = ""
    val r = Renderer(s => { writes += 1; last = s; emu.feed(s) })
    (r, emu, () => writes, () => last)

  test("first paint: committed line then live region appear in order") {
    val (r, emu, _, _) = setup()
    r.render(24, lines("hello"), lines("> input", "---"))
    assertEquals(emu.line(0), "hello")
    assertEquals(emu.line(1), "> input")
    assertEquals(emu.line(2), "---")
    assert(emu.scrollback.isEmpty)
  }

  test("diff: only the changed live row updates, others untouched") {
    val (r, emu, _, _) = setup()
    r.render(24, lines("hello"), lines("> input", "---"))
    r.render(24, Vector.empty, lines("> input!", "---"))
    assertEquals(emu.line(0), "hello") // committed line untouched
    assertEquals(emu.line(1), "> input!")
    assertEquals(emu.line(2), "---")
  }

  test("no-op frame writes nothing") {
    val (r, emu, writes, _) = setup()
    r.render(24, lines("hello"), lines("> input", "---"))
    val before = writes()
    r.render(24, Vector.empty, lines("> input", "---"))
    assertEquals(writes(), before) // fast path: no write at all
  }

  test("commit: new line prints into scrollback region, live repaints below") {
    val (r, emu, _, _) = setup()
    r.render(24, lines("hello"), lines("> input", "---"))
    r.render(24, lines("committed!"), lines("> ", "---"))
    assertEquals(emu.line(0), "hello")
    assertEquals(emu.line(1), "committed!")
    assertEquals(emu.line(2), ">") // trailing space is stripped by the emulator
    assertEquals(emu.line(3), "---")
  }

  test("grow: live region gains rows") {
    val (r, emu, _, _) = setup()
    r.render(24, Vector.empty, lines("a", "b"))
    r.render(24, Vector.empty, lines("a", "b", "c", "d"))
    assertEquals(emu.line(0), "a")
    assertEquals(emu.line(1), "b")
    assertEquals(emu.line(2), "c")
    assertEquals(emu.line(3), "d")
  }

  test("shrink: vacated rows are cleared") {
    val (r, emu, _, _) = setup()
    r.render(24, Vector.empty, lines("a", "b", "c", "d"))
    r.render(24, Vector.empty, lines("a", "b"))
    assertEquals(emu.line(0), "a")
    assertEquals(emu.line(1), "b")
    assertEquals(emu.line(2), "")
    assertEquals(emu.line(3), "")
  }

  test("resize: full repaint at the new width") {
    val (r, emu, _, _) = setup()
    r.render(24, Vector.empty, lines("hello world", "---"))
    r.render(10, Vector.empty, lines("hi", "==="))
    assertEquals(emu.line(0), "hi")
    assertEquals(emu.line(1), "===")
  }

  test("styled change renders correct text and a single change is a small write") {
    val (r, emu, _, last) = setup()
    r.render(24, Vector.empty, Vector(StyledLine.text("count: 0", Style.fg(Color.Green))))
    r.render(24, Vector.empty, Vector(StyledLine.text("count: 1", Style.fg(Color.Green))))
    assertEquals(emu.line(0), "count: 1")
    // only the last cell changed; the write must not repaint the unchanged prefix
    assert(!last().contains("count"), s"expected an incremental write, got: ${last()}")
  }

  test("resize hard-reset clears screen/scrollback and reprints the whole transcript") {
    val (r, emu, _, last) = setup(cols = 24, rows = 8)
    r.render(24, lines("alpha"), lines("> ", "--"))
    r.render(24, lines("beta"), lines("> ", "--")) // committed grows incrementally
    // Resize: the runtime re-passes the whole transcript with hardReset at the new width.
    r.render(12, lines("alpha", "beta"), lines("> ", "--"), hardReset = true)
    assert(last().contains(Ansi.ClearScreen), "screen not cleared on resize")
    assert(last().contains(Ansi.ClearScrollback), "scrollback not cleared on resize")
    assertEquals(emu.line(0), "alpha")
    assertEquals(emu.line(1), "beta")
    assertEquals(emu.line(2), ">")
    assertEquals(emu.line(3), "--")
  }

  test("commit pushes old content into native scrollback when viewport fills") {
    val (r, emu, _, _) = setup(cols = 24, rows = 3)
    r.render(24, lines("one"), lines("> ", "--"))
    // commit several lines so the viewport overflows and early lines scroll off
    r.render(24, lines("two", "three", "four"), lines("> ", "--"))
    assert(emu.scrollback.nonEmpty, "expected lines to have scrolled into history")
    // the live region's last lines remain visible at the bottom
    assertEquals(emu.line(emu.viewport.length - 2), ">") // trailing space stripped
    assertEquals(emu.line(emu.viewport.length - 1), "--")
  }
