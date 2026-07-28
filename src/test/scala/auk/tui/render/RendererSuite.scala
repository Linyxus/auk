package auk.tui.render

import scala.collection.mutable

/** A minimal ANSI terminal emulator that understands exactly the sequences the
  * [[Renderer]] emits (cursor moves, CR/LF with scrolling, erase-to-EOS/EOL,
  * and printable text; SGR / sync / cursor-visibility are no-ops for the text
  * grid). Lets the renderer's output be asserted as visible text + scrollback,
  * which is the only tractable way to catch cursor-drift bugs.
  *
  * It additionally MODELS two pieces of state the text grid ignores, so the whole
  * class of per-screen-buffer bugs is mechanically caught: which screen buffer is
  * active (`?1049h`/`?1049l`) and a Kitty keyboard-enhancement stack PER buffer
  * (`CSI >…u` pushes onto the active buffer's stack, `CSI <u` pops it). The text
  * grid stays single (unaffected by the buffer switch), matching what these tests
  * assert about painted text; only [[enhancementActive]]/[[enhancementDepth]]
  * observe the per-buffer stacks.
  *
  * `termWidth` is this emulator's OWN width opinion, defaulting to
  * [[Width.displayWidth]]. Passing a diverging function simulates a terminal
  * whose Unicode tables disagree with ours — the bug class a shared-table
  * emulator is structurally blind to. */
final class TermEmu(cols: Int, rows: Int, termWidth: Int => Int = Width.displayWidth):
  private val grid = Array.fill(rows, cols)(" ")
  val scrollback = mutable.ArrayBuffer.empty[String]
  var row = 0
  var col = 0

  // false = primary (main) screen buffer active, true = alternate buffer. Kitty
  // pushes/pops apply to the active buffer's stack, so the two are balanced
  // independently; a main-screen push does not affect the alt buffer and vice versa.
  private var onAltBuffer = false
  private val mainEnhancements = mutable.Stack[Int]()
  private val altEnhancements = mutable.Stack[Int]()
  private def activeEnhancements: mutable.Stack[Int] = if onAltBuffer then altEnhancements else mainEnhancements

  /** Whether the ACTIVE screen buffer currently has a Kitty keyboard enhancement
    * pushed (a non-empty stack). This is the property the alt-screen fix restores:
    * it must be true while a fullscreen view is displayed. */
  def enhancementActive: Boolean = activeEnhancements.nonEmpty

  /** Depth of the ACTIVE buffer's enhancement stack — lets a test prove a cycle
    * stays balanced (exactly one push in the alt buffer, none leaked on return). */
  def enhancementDepth: Int = activeEnhancements.size

  // DECAWM (`?7h`/`?7l`). The renderer disables it inside the alt screen so a
  // width-disagreeing terminal clips an overflowing row instead of wrapping it
  // into the row below; tests assert both the containment and the restore.
  private var autowrap = true
  def autowrapEnabled: Boolean = autowrap

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
    if params.startsWith("?") then
      // DEC private modes. Only the alt-screen switch changes modelled state — it
      // selects the ACTIVE buffer for the enhancement stacks. Sync (`?2026`), cursor
      // visibility (`?25`), and mouse (`?1000`/`?1006`) don't affect the text grid.
      if params == "?1049" then onAltBuffer = (fin == 'h')
      else if params == "?7" then autowrap = (fin == 'h')
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
        case 'u' =>
          // Kitty keyboard protocol on the ACTIVE buffer's stack: `CSI >…u` pushes
          // the flag, `CSI <u` pops. A pop on an empty stack is ignored (as a real
          // terminal would), so an unbalanced teardown surfaces as a stuck stack.
          if params.startsWith(">") then activeEnhancements.push(num(params, 0))
          else if params.startsWith("<") && activeEnhancements.nonEmpty then { activeEnhancements.pop(); () }
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
    val w = termWidth(cp)
    if w == 0 then return
    if col + w > cols then
      if autowrap then { col = 0; lineFeed() }
      else col = math.max(0, cols - w) // wrap off: pin at the margin, overwrite
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

  /** Like [[setup]] but with a kitty-capable terminal's alt-screen setup/teardown
    * (the keyboard enhancement push/pop), so the emulator's per-buffer enhancement
    * stacks can be observed across fullscreen transitions. */
  private def setupKitty(cols: Int = 24, rows: Int = 8): (Renderer, TermEmu, () => String) =
    val emu = TermEmu(cols, rows)
    var last = ""
    val r = Renderer(s => { last = s; emu.feed(s) }, Ansi.PushKeyboardEnhancement, Ansi.PopKeyboardEnhancement)
    (r, emu, () => last)

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
    assert(last().indexOf(Ansi.Reset) < last().indexOf(Ansi.ClearScreen), "hard reset should reset style before clearing")
    assertEquals(emu.line(0), "alpha")
    assertEquals(emu.line(1), "beta")
    assertEquals(emu.line(2), ">")
    assertEquals(emu.line(3), "--")
  }

  test("frames end with a style reset so panel backgrounds cannot leak") {
    val (r, _, _, last) = setup(cols = 20, rows = 8)
    val panelStyle = Style(fg = Color.White, bg = Color.Indexed(236))

    r.render(20, Vector.empty, Vector(
      StyledLine(Vector(Span("panel", panelStyle)))
    ))

    val output = last()
    assert(output.endsWith(Ansi.Reset + Ansi.SyncEnd), "frame should reset style before leaving synchronized output")
  }

  test("overlay floats over live rows and clears on the next frame") {
    val (r, emu, _, _) = setup(cols = 20, rows = 8)
    val live = lines(
      "00000000000000000000",
      "11111111111111111111",
      "22222222222222222222",
      "33333333333333333333",
      "44444444444444444444"
    )
    val panelStyle = Style(fg = Color.White, bg = Color.Indexed(236))
    val overlay = Vector(
      StyledLine(Vector(Span(" KEY    ", panelStyle))),
      StyledLine(Vector(Span(" c exit ", panelStyle)))
    )

    r.render(20, Vector.empty, live, overlay = Some(overlay))
    assertEquals(emu.line(0), "00000000000000000000")
    assert(emu.line(1).contains(" KEY "), emu.line(1))
    assert(emu.line(2).contains(" c exit "), emu.line(2))
    assertEquals(emu.line(3), "33333333333333333333")

    r.render(20, Vector.empty, live)
    assertEquals(emu.line(1), "11111111111111111111")
    assertEquals(emu.line(2), "22222222222222222222")
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

  /* ---- fullscreen (alternate screen buffer) ---- */

  test("fullscreen: the first frame enters the alt buffer and paints every row") {
    val (r, emu, _, last) = setup(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("HEADER", "body", "more", "FOOTER"))
    assert(last().contains(Ansi.AltScreenEnter), s"expected alt-screen enter: ${last()}")
    assertEquals(emu.line(0), "HEADER")
    assertEquals(emu.line(1), "body")
    assertEquals(emu.line(3), "FOOTER")
  }

  test("fullscreen: invalidateFullscreen forces the next same frame to clear and repaint fully") {
    val (r, emu, writes, last) = setup(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("HEADER", "body", "more", "FOOTER"))
    // An identical frame is normally a no-op...
    val before = writes()
    r.renderFullscreen(20, 4, lines("HEADER", "body", "more", "FOOTER"))
    assertEquals(writes(), before, "identical frame must be a no-op")
    // ...but after invalidation it clears the buffer and repaints every row,
    // without re-entering the alt screen.
    r.invalidateFullscreen()
    r.renderFullscreen(20, 4, lines("HEADER", "body", "more", "FOOTER"))
    assert(writes() > before, "invalidation must force a repaint")
    assert(!last().contains(Ansi.AltScreenEnter), s"already in the alt buffer: ${last()}")
    assert(last().contains(Ansi.ClearScreen), s"expected a clear + full repaint: ${last()}")
    assertEquals(emu.line(0), "HEADER")
    assertEquals(emu.line(3), "FOOTER")
  }

  test("fullscreen: a same-dims second frame patches only the changed cell") {
    val (r, emu, _, last) = setup(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("HEADER", "body", "more", "FOOTER"))
    r.renderFullscreen(20, 4, lines("HEADER", "bodX", "more", "FOOTER"))
    assertEquals(emu.line(1), "bodX")
    assert(!last().contains(Ansi.AltScreenEnter), s"already in the alt buffer: ${last()}")
    assert(!last().contains(Ansi.ClearScreen), s"an incremental frame must not clear: ${last()}")
    // Unchanged rows are not repainted, so their text is absent from the patch.
    assert(!last().contains("HEADER"), s"unchanged rows should not be repainted: ${last()}")
  }

  test("fullscreen: returning inline emits the exit sequence and is an ordinary incremental frame") {
    val (r, _, _, last) = setup(cols = 20, rows = 6)
    r.render(20, Vector.empty, lines("> input", "---")) // establish the inline prev
    r.renderFullscreen(20, 6, lines("A", "B", "C", "D", "E", "F"))
    r.render(20, Vector.empty, lines("> input!", "---")) // one live cell changed
    assert(last().contains(Ansi.AltScreenExit), s"expected alt-screen exit: ${last()}")
    assert(!last().contains(Ansi.ClearScreen), s"return must not hard-reset: ${last()}")
    assert(!last().contains(Ansi.ClearScrollback), s"return must not clear scrollback: ${last()}")
  }

  test("fullscreen: exitFullscreen is idempotent and a no-op when inactive") {
    val (r, _, writes, last) = setup()
    val before = writes()
    r.exitFullscreen() // inactive: nothing to do
    assertEquals(writes(), before, "exitFullscreen must not write when not in the alt buffer")
    r.renderFullscreen(24, 4, lines("a", "b", "c", "d"))
    r.exitFullscreen()
    assertEquals(last(), Ansi.WrapOn + Ansi.AltScreenExit, s"expected a bare wrap-restore + alt-screen exit, got: ${last()}")
    val after = writes()
    r.exitFullscreen()
    assertEquals(writes(), after, "a second exitFullscreen is a no-op")
  }

  /* ---- fullscreen: per-buffer kitty keyboard enhancement (stateful emulation) ---- */

  test("fullscreen: entering the alt buffer pushes the kitty enhancement onto that buffer's stack") {
    val (r, emu, _) = setupKitty(cols = 20, rows = 4)
    assert(!emu.enhancementActive, "the main buffer starts with no keyboard enhancement")
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    assert(emu.enhancementActive, "the alt buffer must have the keyboard enhancement active while fullscreen")
    assertEquals(emu.enhancementDepth, 1, "exactly one enhancement is pushed on entry")
  }

  test("fullscreen: same-dims frames don't re-push, so the alt stack stays at depth one") {
    val (r, emu, _) = setupKitty(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    r.renderFullscreen(20, 4, lines("A", "X", "C", "D")) // incremental patch, no re-entry
    assertEquals(emu.enhancementDepth, 1, "an incremental fullscreen frame must not push again")
  }

  test("fullscreen: exitFullscreen pops the alt buffer's enhancement, leaving both buffers balanced") {
    val (r, emu, _) = setupKitty(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    r.exitFullscreen()
    // Back on the main buffer, which never had a push in this harness.
    assert(!emu.enhancementActive, "the main buffer has no enhancement after returning from fullscreen")
    assertEquals(emu.enhancementDepth, 0, "the (now active) main buffer's stack is empty")
  }

  test("fullscreen: an enter → exit → re-enter cycle keeps the alt enhancement stack balanced") {
    val (r, emu, _) = setupKitty(cols = 20, rows = 4)
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    assertEquals(emu.enhancementDepth, 1)
    r.exitFullscreen()
    assertEquals(emu.enhancementDepth, 0)
    // Re-entering must push exactly once more, not accumulate onto a stale alt stack:
    // if the previous exit had failed to pop (or popped the wrong buffer) this reads 2.
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    assertEquals(emu.enhancementDepth, 1, "re-entry must not accumulate a second unbalanced push")
  }

  test("fullscreen: the inline-return frame pops the alt enhancement before leaving the buffer") {
    val (r, emu, _) = setupKitty(cols = 20, rows = 6)
    r.render(20, Vector.empty, lines("> input", "---")) // establish the inline prev
    r.renderFullscreen(20, 6, lines("A", "B", "C", "D", "E", "F"))
    assertEquals(emu.enhancementDepth, 1)
    r.render(20, Vector.empty, lines("> input!", "---")) // return to inline in one frame
    // The return path (Renderer.render's leavingAlt branch) must pop the alt stack
    // as part of the same frame that leaves the buffer, not just exitFullscreen.
    assert(!emu.enhancementActive, "the main buffer has no enhancement after the inline return")
    assertEquals(emu.enhancementDepth, 0, "the alt push is balanced on the inline return")
  }

  test("fullscreen: a width-disputed row is rewritten whole, so a narrow-emoji terminal shows no residue") {
    // A terminal whose (old) tables render ✅ as ONE column while Width says two.
    // Frame 2 shifts content after the emoji; a cell-level diff would emit only
    // the changed cells and rely on relative moves that this terminal lands one
    // column short, leaving frame-1 glyphs behind ("✅b✅c"). The full-row
    // rewrite + erase-to-EOL must yield exactly the compacted new content.
    val emu = TermEmu(24, 8, cp => if cp == 0x2705 then 1 else Width.displayWidth(cp))
    val r = Renderer(emu.feed(_))
    r.renderFullscreen(24, 3, lines("ab✅c", "plain", ""))
    assertEquals(emu.line(0), "ab✅c")
    r.renderFullscreen(24, 3, lines("✅✅c", "plain", ""))
    assertEquals(emu.line(0), "✅✅c", "stale cells must not survive a shifted emoji row")
    assertEquals(emu.line(1), "plain", "containment must stay inside the risky row")
    // A repeat of the same frame is still a clean no-op (stable, not oscillating).
    r.renderFullscreen(24, 3, lines("✅✅c", "plain", ""))
    assertEquals(emu.line(0), "✅✅c")
  }

  test("fullscreen: a row going emoji → plain erases the terminal's wider stale tail") {
    // The terminal draws 🖥 (Width: 1 column) as TWO columns, so the on-screen
    // row is wider than the model believes. When the row is replaced by plain
    // text, the rewrite must erase past the model's own extent (the EL), or the
    // old tail would linger.
    val emu = TermEmu(24, 8, cp => if cp == 0x1f5a5 then 2 else Width.displayWidth(cp))
    val r = Renderer(emu.feed(_))
    r.renderFullscreen(24, 3, lines("🖥xyz", "plain", ""))
    r.renderFullscreen(24, 3, lines("ok", "plain", ""))
    assertEquals(emu.line(0), "ok", "the wider-than-model stale tail must be erased")
  }

  test("fullscreen: autowrap is off, so an under-counted full row clips instead of shifting the frame") {
    // Model width of the top row is exactly `cols`; the terminal sizes 🖥 one
    // column wider, so writing the row would wrap without ?7l and push every
    // following row down (paintFresh's \r\n would then land one row too low).
    val emu = TermEmu(10, 4, cp => if cp == 0x1f5a5 then 2 else Width.displayWidth(cp))
    val r = Renderer(emu.feed(_))
    r.renderFullscreen(10, 4, lines("🖥" + "a" * 9, "next", "", ""))
    assert(!emu.autowrapEnabled, "the alt screen must run with DECAWM off")
    assertEquals(emu.line(1), "next", "the overflow must clip in-row, not wrap into the next row")
  }

  test("fullscreen: adopted probe widths bring the model into lockstep with a narrow-emoji terminal") {
    // Once the startup probe measures ✅/😀 as ONE column and Width adopts it,
    // layout, diff, and terminal all agree — content after an emoji sits at its
    // exact intended column even on the old terminal.
    val emu = TermEmu(24, 8, cp => if cp == 0x2705 || cp == 0x1f600 then 1 else Width.displayWidth(cp))
    val r = Renderer(emu.feed(_))
    try
      Width.adopt(emojiBmp = 1, emojiAstral = 1, ambiguous = 1)
      r.renderFullscreen(24, 3, lines("✅ok😀!", "plain", ""))
      assertEquals(emu.line(0), "✅ok😀!")
      r.renderFullscreen(24, 3, lines("✅ok😀?", "plain", ""))
      assertEquals(emu.line(0), "✅ok😀?")
    finally Width.resetAdopted()
  }

  test("fullscreen: autowrap is restored on both exit paths") {
    val (r, emu, _, _) = setup(cols = 20, rows = 4)
    r.render(20, Vector.empty, lines("inline")) // establish the inline prev
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    assert(!emu.autowrapEnabled)
    r.render(20, Vector.empty, lines("inline")) // leavingAlt path
    assert(emu.autowrapEnabled, "the inline-return frame must restore DECAWM")
    r.renderFullscreen(20, 4, lines("A", "B", "C", "D"))
    assert(!emu.autowrapEnabled)
    r.exitFullscreen() // teardown path
    assert(emu.autowrapEnabled, "exitFullscreen must restore DECAWM")
  }
