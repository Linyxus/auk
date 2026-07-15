package auk.tui.app

import auk.tui.render.{Attr, Color, Span, Style, StyledLine, Width}
import Element.*

/** Turns the view DSL into styled lines for the renderer. Also the single place
  * that re-tokenizes embedded ANSI in `Text` strings back into structured spans
  * (so the app can keep building strings from sub-elements' `.render`). */
object Layout:
  private val SpinnerFrames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

  /** Lay an element into physical rows at `width` columns. */
  def lay(e: Element, width: Int): Vector[StyledLine] = e match
    case TextNode(value, style) =>
      value.split("\n", -1).toVector.map(seg => StyledLine(tokenize(seg, style)))
    case Stack(children) =>
      children.flatMap(lay(_, width))
    case LineBreak =>
      Vector(StyledLine.empty)
    case Blank =>
      Vector.empty
    case RuleNode(ch, style) =>
      Vector(StyledLine(Vector(Span(ch.toString * math.max(width, 1), style))))
    case SpinnerNode(label, frame, style) =>
      val glyph = SpinnerFrames.charAt(math.floorMod(frame, SpinnerFrames.length))
      Vector(StyledLine(Vector(Span(s"$glyph $label", style))))
    case StyledNode(inner, style) =>
      lay(inner, width).map(line => StyledLine(line.spans.map(sp => Span(sp.text, style ++ sp.style))))
    case WrappedTextNode(firstPrefix, nextPrefix, value, style, mode) =>
      layWrapped(firstPrefix, nextPrefix, tokenize(value, style), width, mode)
    case MemoNode(inner, memo) =>
      if memo.laidWidth == width then memo.laid
      else
        val lines = lay(inner, width)
        memo.laidWidth = width
        memo.laid = lines
        lines
    case TableNode(firstPrefix, nextPrefix, align, header, rows, border) =>
      layTable(firstPrefix, nextPrefix, align, header, rows, border, width)

  /** Render an element to a newline-joined ANSI string for inline composition. */
  def renderInline(e: Element): String =
    lay(e, 80).map(_.render).mkString("\n")

  /** Split text into styled spans, interpreting any embedded SGR sequences. The
    * fast path (no escapes) is a single span. */
  private def tokenize(text: String, base: Style): Vector[Span] =
    if text.indexOf('') < 0 then Vector(Span(text, base))
    else
      val spans = Vector.newBuilder[Span]
      val run = new StringBuilder
      var style = base
      var i = 0
      val n = text.length
      while i < n do
        val ch = text.charAt(i)
        if ch.toInt == 27 && i + 1 < n && text.charAt(i + 1) == '[' then
          if run.nonEmpty then { spans += Span(run.toString, style); run.setLength(0) }
          var j = i + 2
          val params = new StringBuilder
          while j < n && !Character.isLetter(text.charAt(j)) do { params.append(text.charAt(j)); j += 1 }
          val finalByte = if j < n then text.charAt(j) else 'm'
          j += 1
          if finalByte == 'm' then style = applySgr(params.toString, base, style)
          i = j
        else
          run.append(ch)
          i += 1
      if run.nonEmpty then spans += Span(run.toString, style)
      spans.result()

  /** Soft-wrap styled text under a first-line and a continuation prefix. */
  private def layWrapped(
      firstPrefix: String,
      nextPrefix: String,
      spans: Vector[Span],
      width: Int,
      mode: Wrap
  ): Vector[StyledLine] =
    val first = tokenize(firstPrefix, Style.Default)
    val next = tokenize(nextPrefix, Style.Default)
    val fullWidth = math.max(width, 1)
    val firstWidth = math.max(1, fullWidth - prefixWidth(first))
    val restWidth = math.max(1, fullWidth - prefixWidth(next))
    wrapSpans(spans, firstWidth, restWidth, mode).zipWithIndex.map: (content, i) =>
      StyledLine((if i == 0 then first else next) ++ content)

  /** The shared wrapping engine: reflow styled spans into content lines (no
    * prefix), each no wider than the line's budget (`firstWidth` for line 0,
    * `restWidth` thereafter). [[Wrap.Word]] breaks between words (splitting a word
    * only when it alone overflows); [[Wrap.Char]] breaks at any code point. Style
    * boundaries and code-point widths are preserved; an explicit `\n` always
    * breaks. */
  private def wrapSpans(spans: Vector[Span], firstWidth: Int, restWidth: Int, mode: Wrap): Vector[Vector[Span]] =
    // Flatten to parallel code-point / style arrays for index-based scanning.
    val cps = scala.collection.mutable.ArrayBuffer.empty[Int]
    val sts = scala.collection.mutable.ArrayBuffer.empty[Style]
    for sp <- spans do
      var k = 0
      while k < sp.text.length do
        val cp = sp.text.codePointAt(k)
        cps += cp; sts += sp.style
        k += Character.charCount(cp)
    val n = cps.length

    val out = Vector.newBuilder[Vector[Span]]
    val line = scala.collection.mutable.ArrayBuffer.empty[Span]
    var col = 0
    var limit = firstWidth

    def emit(): Unit =
      out += line.toVector
      line.clear()
      col = 0
      limit = restWidth

    def appendOne(idx: Int): Unit =
      val st = sts(idx)
      val text = new String(Character.toChars(cps(idx)))
      if line.nonEmpty && line.last.style == st then
        line.update(line.length - 1, Span(line.last.text + text, st))
      else line += Span(text, st)
      col += Width.displayWidth(cps(idx))

    def addRange(a: Int, b: Int): Unit =
      var i = a
      while i < b do { appendOne(i); i += 1 }

    def rangeWidth(a: Int, b: Int): Int =
      var i = a; var w = 0
      while i < b do { w += Width.displayWidth(cps(i)); i += 1 }
      w

    def charBreak(a: Int, b: Int): Unit =
      var i = a
      while i < b do
        val w = Width.displayWidth(cps(i))
        if w > 0 && col > 0 && col + w > limit then emit()
        appendOne(i); i += 1

    // A styled (non-default) space is atomic — it is the input box's cursor
    // cell, which must survive even when it sits at the end of a wrapped line.
    def gapHasStyledSpace(a: Int, b: Int): Boolean =
      var i = a; var styled = false
      while i < b do { if sts(i) != Style.Default then styled = true; i += 1 }
      styled

    mode match
      case Wrap.Char =>
        var i = 0
        while i < n do
          if cps(i) == '\n' then { emit(); i += 1 }
          else { charBreak(i, i + 1); i += 1 }

      case Wrap.Word =>
        var i = 0
        var gapA = 0; var gapB = 0; var gapW = 0 // a pending collapsible space run
        while i < n do
          val cp = cps(i)
          if cp == '\n' then
            // Collapse plain trailing whitespace at a hard break, but keep a
            // styled space — the cursor cell parked at the end of this line.
            if gapW > 0 && gapHasStyledSpace(gapA, gapB) then addRange(gapA, gapB)
            gapW = 0; emit(); i += 1
          else if cp == ' ' then
            gapA = i
            while i < n && cps(i) == ' ' do i += 1
            gapB = i; gapW = gapB - gapA
          else
            val a = i
            while i < n && cps(i) != ' ' && cps(i) != '\n' do i += 1
            val ww = rangeWidth(a, i)
            if col == 0 then
              if ww <= limit then addRange(a, i) else charBreak(a, i)
            else if col + gapW + ww <= limit then
              addRange(gapA, gapB); addRange(a, i)
            else
              emit()
              if ww <= limit then addRange(a, i) else charBreak(a, i)
            gapW = 0
        // Flush a trailing space run (e.g. the input box's cursor cell).
        if gapW > 0 then addRange(gapA, gapB)

    out += line.toVector
    out.result()

  /* ---- tables ---- */

  /** Lay a table at the layout width: columns size to content when they fit, else
    * share the width per [[TableLayout]] and cells word-wrap. */
  private def layTable(
      firstPrefix: String,
      nextPrefix: String,
      align: Vector[ColumnAlign],
      header: Vector[Vector[Span]],
      rows: Vector[Vector[Vector[Span]]],
      border: Style,
      width: Int
  ): Vector[StyledLine] =
    val cols = align.length
    if cols == 0 then return Vector.empty
    val firstPfx = tokenize(firstPrefix, Style.Default)
    val nextPfx = tokenize(nextPrefix, Style.Default)
    val pfxW = math.max(prefixWidth(firstPfx), prefixWidth(nextPfx))
    val sepW = 3 // " │ "
    val avail = math.max(1, math.max(width, 1) - pfxW - (cols - 1) * sepW)
    val all = header +: rows

    def cellAt(r: Vector[Vector[Span]], c: Int): Vector[Span] = if c < r.length then r(c) else Vector.empty
    def cellWidth(cell: Vector[Span]): Int = cell.iterator.map(s => Width.stringWidth(s.text)).sum

    val cap = math.max(1, avail / 2)
    val natural = (0 until cols).map(c => math.max(1, all.iterator.map(r => cellWidth(cellAt(r, c))).maxOption.getOrElse(0))).toVector
    val mins = (0 until cols).map: c =>
      val longest = all.iterator.map(r => longestWord(cellAt(r, c))).maxOption.getOrElse(0)
      math.max(1, math.min(longest, cap))
    .toVector
    val widths = TableLayout.columnWidths(natural, mins, avail)

    val sep = Span(" │ ", border)
    val out = Vector.newBuilder[StyledLine]
    var firstPhysical = true

    def emitRow(cells: Vector[Vector[Span]]): Unit =
      val wrapped = (0 until cols).map(c => wrapSpans(cellAt(cells, c), widths(c), widths(c), Wrap.Word))
      val height = wrapped.map(_.length).max
      for li <- 0 until height do
        val spans = Vector.newBuilder[Span]
        spans ++= (if firstPhysical then firstPfx else nextPfx)
        for c <- 0 until cols do
          if c > 0 then spans += sep
          val cellLine = if li < wrapped(c).length then wrapped(c)(li) else Vector.empty
          spans ++= padAlign(cellLine, widths(c), align(c))
        out += StyledLine(spans.result())
        firstPhysical = false

    emitRow(header)
    // The header rule.
    val rule = Vector.newBuilder[Span]
    rule ++= (if firstPhysical then firstPfx else nextPfx)
    for c <- 0 until cols do
      if c > 0 then rule += Span("─┼─", border)
      rule += Span("─" * widths(c), border)
    out += StyledLine(rule.result())
    firstPhysical = false
    rows.foreach(emitRow)
    out.result()

  /** The widest whitespace-delimited word in a cell, in display columns. */
  private def longestWord(cell: Vector[Span]): Int =
    var best = 0; var cur = 0
    for sp <- cell do
      var k = 0
      while k < sp.text.length do
        val cp = sp.text.codePointAt(k)
        if cp == ' ' || cp == '\n' then { best = math.max(best, cur); cur = 0 }
        else cur += Width.displayWidth(cp)
        k += Character.charCount(cp)
    math.max(best, cur)

  /** Pad a cell line (already no wider than `width`) to `width` columns per its
    * column alignment. */
  private def padAlign(spans: Vector[Span], width: Int, align: ColumnAlign): Vector[Span] =
    val w = spans.iterator.map(s => Width.stringWidth(s.text)).sum
    val pad = math.max(0, width - w)
    val (lp, rp) = align match
      case ColumnAlign.Right  => (pad, 0)
      case ColumnAlign.Center => (pad / 2, pad - pad / 2)
      case ColumnAlign.Left   => (0, pad)
    val b = Vector.newBuilder[Span]
    if lp > 0 then b += Span(" " * lp, Style.Default)
    b ++= spans
    if rp > 0 then b += Span(" " * rp, Style.Default)
    b.result()

  private def prefixWidth(spans: Vector[Span]): Int =
    spans.iterator.map(sp => Width.stringWidth(sp.text)).sum

  /** Apply one SGR parameter string (`"0;1;36"`) to a style. Code `0` resets to
    * `base` so an element's own style stays the baseline. */
  private def applySgr(params: String, base: Style, current: Style): Style =
    if params.isEmpty || params == "0" then base
    else
      val codes = params.split(";").toVector.map(s => if s.isEmpty then 0 else s.toIntOption.getOrElse(0))
      var st = current
      var k = 0
      while k < codes.length do
        codes(k) match
          case 0                          => st = base
          case 1                          => st = st.withAttr(Attr.Bold)
          case 2                          => st = st.withAttr(Attr.Dim)
          case 3                          => st = st.withAttr(Attr.Italic)
          case 4                          => st = st.withAttr(Attr.Underline)
          case 7                          => st = st.withAttr(Attr.Reverse)
          case 39                         => st = st.withFg(Color.Default)
          case 49                         => st = st.withBg(Color.Default)
          case x if x >= 30 && x <= 37    => st = st.withFg(Color.Named(x - 30))
          case x if x >= 90 && x <= 97    => st = st.withFg(Color.Named(8 + x - 90))
          case x if x >= 40 && x <= 47    => st = st.withBg(Color.Named(x - 40))
          case x if x >= 100 && x <= 107  => st = st.withBg(Color.Named(8 + x - 100))
          case 38 =>
            if k + 2 < codes.length && codes(k + 1) == 5 then { st = st.withFg(Color.Indexed(codes(k + 2))); k += 2 }
            else if k + 4 < codes.length && codes(k + 1) == 2 then { st = st.withFg(Color.True(codes(k + 2), codes(k + 3), codes(k + 4))); k += 4 }
          case 48 =>
            if k + 2 < codes.length && codes(k + 1) == 5 then { st = st.withBg(Color.Indexed(codes(k + 2))); k += 2 }
            else if k + 4 < codes.length && codes(k + 1) == 2 then { st = st.withBg(Color.True(codes(k + 2), codes(k + 3), codes(k + 4))); k += 4 }
          case _ => ()
        k += 1
      st
