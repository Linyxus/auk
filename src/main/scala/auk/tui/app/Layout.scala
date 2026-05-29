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
    case WrappedTextNode(firstPrefix, nextPrefix, value, style) =>
      layWrapped(firstPrefix, nextPrefix, tokenize(value, style), width)

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

  /** Soft-wrap styled text, preserving style boundaries and codepoint widths. */
  private def layWrapped(
      firstPrefix: String,
      nextPrefix: String,
      spans: Vector[Span],
      width: Int
  ): Vector[StyledLine] =
    val first = tokenize(firstPrefix, Style.Default)
    val next = tokenize(nextPrefix, Style.Default)
    val fullWidth = math.max(width, 1)
    val firstWidth = prefixWidth(first)
    val nextWidth = prefixWidth(next)
    val lines = Vector.newBuilder[StyledLine]
    var line = scala.collection.mutable.ArrayBuffer.empty[Span]
    var prefix = first
    var contentWidth = math.max(1, fullWidth - firstWidth)
    var col = 0

    def appendText(style: Style, text: String): Unit =
      if text.nonEmpty then
        if line.nonEmpty && line.last.style == style then
          val last = line.last
          line.update(line.length - 1, Span(last.text + text, style))
        else line += Span(text, style)

    def emitLine(): Unit =
      lines += StyledLine(prefix ++ line.toVector)
      line.clear()
      prefix = next
      contentWidth = math.max(1, fullWidth - nextWidth)
      col = 0

    for sp <- spans do
      val text = sp.text
      var i = 0
      while i < text.length do
        val cp = text.codePointAt(i)
        if cp == '\n' then emitLine()
        else
          val w = Width.displayWidth(cp)
          if w > 0 && col > 0 && col + w > contentWidth then emitLine()
          appendText(sp.style, new String(Character.toChars(cp)))
          col += w
        i += Character.charCount(cp)

    emitLine()
    lines.result()

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
