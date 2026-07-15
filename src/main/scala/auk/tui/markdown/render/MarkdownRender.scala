package auk.tui.markdown.render

import auk.tui.app.*
import auk.tui.render.{Ansi, Attr, Color, Span, Style, Width}
import auk.tui.markdown.{Align, Block, Inline, ListItem, MarkdownDocument}
import auk.tui.Glow

/** Incremental render cache for one answer document, fed frame after frame
  * while the answer streams.
  *
  * [[MarkdownDocument]] guarantees its finalised blocks are immutable and
  * append-only, so their Elements are rendered once and replayed from here;
  * each is wrapped in an [[Element.MemoNode]] so the (dominant) wrap/layout
  * cost is also paid once per width instead of every frame. Only the open tail
  * re-renders per frame.
  *
  * Safe by construction: the cache keys itself on the identity of the last
  * finalised block it rendered, so if it is handed a *different* document (a
  * new turn reusing the slot, two answers sharing one cache) the key mismatches
  * and it rebuilds from scratch — degrading to the uncached cost, never to
  * stale output. Owned by the view (single-threaded), so plain vars. */
final class AnswerRenderCache:
  private[render] var elems: Vector[Element] = Vector.empty
  private[render] var lastFinal: Block | Null = null

/** Renders a parsed Markdown document into the view DSL [[Element]].
  *
  * Block-level constructs become [[wrapText]] nodes whose wrapping is deferred to
  * the terminal width; inline styling is emitted as embedded SGR (re-tokenised by
  * the layout). Container prefixes (list markers, blockquote rails) are threaded
  * as first/continuation strings, so nesting composes without post-layout surgery.
  *
  * For a streaming answer, the finalised blocks render plain (they are fully
  * "cooled") and only the trailing open block carries the reveal glow on its
  * newest code points plus the breathing cursor — see [[answerBlock]].
  */
object MarkdownRender:

  /** The two-space base indent, matching the plain answer rendering. */
  private val Indent = "  "

  private val FrameBlue: Color = Color.True(135, 206, 235)
  private val CodeFg: Color = Color.True(198, 204, 220)
  private val InlineCodeStyle: Style = Style.fg(Color.True(214, 182, 122))
  private val LinkStyle: Style = Style(fg = Color.Cyan, attrs = Attr.Underline)

  // Constant SGR sequences hoisted out of per-block builders (declared after the
  // colour vals so those init first). Each equals its old inline expression.
  private val DimSeq: String = Style.Dim.setSequence
  private val QuoteBar: String = DimSeq + "▌ " + Ansi.Reset
  private val CodeRail: String = DimSeq + "▏ " + Ansi.Reset
  private val CodeSeq: String = Style.fg(CodeFg).setSequence

  /** Render a fully-settled document (no glow) — committed answers, resumed
    * history, reasoning that happens to be Markdown, tests. */
  def render(doc: MarkdownDocument): Element = render(doc.blocks)

  def render(blocks: Vector[Block]): Element =
    joinSpaced(renderList(blocks, Indent, Indent), spaced = true)

  /** Render a streaming answer. `glow` carries `(hot, frame)`: how many trailing
    * code points of the open block still glow, and the animation frame for the
    * breathing cursor. `None` renders everything plain (settled / committed). */
  def answerBlock(doc: MarkdownDocument, glow: Option[(Int, Int)]): Element =
    val blocks = doc.blocks
    glow match
      case None => render(blocks)
      case Some((hot, frame)) =>
        if blocks.isEmpty then Text(Indent + Glow.cursor(frame))
        else
          val plain = renderList(blocks.init, Indent, Indent)
          val open = renderOpen(blocks.last, Indent, Indent, hot, frame)
          joinSpaced(plain :+ open, spaced = true)

  /** Cached variant of [[answerBlock]] for the per-frame streaming path:
    * identical output, but the finalised blocks' Elements come from `cache`
    * (rendered once, layout-memoized) and only the open tail is re-rendered. */
  def answerBlock(doc: MarkdownDocument, glow: Option[(Int, Int)], cache: AnswerRenderCache): Element =
    val fin = finalizedElems(doc, cache)
    val open = doc.openBlocks
    glow match
      case None =>
        joinSpaced(fin ++ open.map(renderBlock(_, Indent, Indent)), spaced = true)
      case Some((hot, frame)) =>
        if fin.isEmpty && open.isEmpty then Text(Indent + Glow.cursor(frame))
        else if open.nonEmpty then
          val provisional = open.init.map(renderBlock(_, Indent, Indent))
          val openElem = renderOpen(open.last, Indent, Indent, hot, frame)
          joinSpaced((fin ++ provisional) :+ openElem, spaced = true)
        else
          // Everything is finalised (a closed doc still glowing — rare): the
          // glowing open block is the last finalised one; keep the rest cached.
          val openElem = renderOpen(doc.finalized.last, Indent, Indent, hot, frame)
          joinSpaced(fin.init :+ openElem, spaced = true)

  /** The finalised blocks' Elements, grown incrementally through `cache`:
    * append-only growth renders only the new tail; an identity mismatch on the
    * last shared block (a different document) resets and re-renders fully. */
  private def finalizedElems(doc: MarkdownDocument, cache: AnswerRenderCache): Vector[Element] =
    val fin = doc.finalized
    val n = fin.length
    if n == 0 then Vector.empty
    else
      val have = cache.elems.length
      val lf = cache.lastFinal
      val kept =
        if have > 0 && have <= n && lf != null && (lf eq fin(have - 1)) then cache.elems
        else Vector.empty
      if kept.length == n then kept
      else
        var elems = kept
        var i = elems.length
        while i < n do
          elems = elems :+ Element.MemoNode(renderBlock(fin(i), Indent, Indent), LayMemo())
          i += 1
        cache.elems = elems
        cache.lastFinal = fin(n - 1)
        elems

  /* ---- block sequences ---- */

  /** One element per block; the first block uses `first`, the rest use `cont`. */
  private def renderList(blocks: Vector[Block], first: String, cont: String): Vector[Element] =
    blocks.zipWithIndex.map((b, i) => renderBlock(b, if i == 0 then first else cont, cont))

  private def joinSpaced(elems: Vector[Element], spaced: Boolean): Element =
    if elems.isEmpty then Empty
    else if !spaced then layout(elems*)
    else layout(elems.zipWithIndex.flatMap((e, i) => if i == 0 then Vector(e) else Vector(br, e))*)

  /* ---- one block ---- */

  private def renderBlock(b: Block, first: String, cont: String): Element = b match
    case Block.Paragraph(content) =>
      wrapText(first, cont, serialize(inlineRuns(content, Style.Default)))

    case Block.Heading(level, content) =>
      val styled = wrapText(first, cont, serialize(inlineRuns(content, headingStyle(level))))
      if level == 1 then layout(styled, hr('─', FrameBlue)) else styled

    case Block.Code(_, lang, code, _) =>
      renderCode(lang, code, first, cont)

    case Block.Quote(inner) =>
      joinSpaced(renderList(inner, first + QuoteBar, cont + QuoteBar), spaced = true)

    case Block.ThematicBreak =>
      hr('─', Color.Default).style(Style.Dim)

    case lb: Block.ListBlock =>
      renderListBlock(lb, first, cont)

    case t: Block.Table =>
      renderTable(t, first, cont)

  /** The trailing, still-streaming block: glow its newest `hot` code points and
    * ride the breathing cursor on the tail. Prose (paragraph/heading) glows;
    * other open blocks fall back to a plain render. */
  private def renderOpen(b: Block, first: String, cont: String, hot: Int, frame: Int): Element = b match
    case Block.Paragraph(content) =>
      val runs = glowRuns(inlineRuns(content, Style.Default), hot)
      wrapText(first, cont, serialize(runs) + Glow.cursor(frame))
    case Block.Heading(level, content) =>
      val runs = glowRuns(inlineRuns(content, headingStyle(level)), hot)
      val styled = wrapText(first, cont, serialize(runs) + Glow.cursor(frame))
      if level == 1 then layout(styled, hr('─', FrameBlue)) else styled
    case other =>
      renderBlock(other, first, cont)

  private def headingStyle(level: Int): Style =
    if level <= 2 then Style(fg = FrameBlue, attrs = Attr.Bold) else Style(attrs = Attr.Bold)

  /* ---- code blocks ---- */

  private def renderCode(lang: Option[String], code: String, first: String, cont: String): Element =
    val rail = CodeRail
    val lines = if code.isEmpty then Vector("") else code.split("\n", -1).toVector
    // First rendered row uses `first`; the rest use `cont`. The language tag, if
    // present, takes the first row, pushing the code body to `cont`.
    val bodyFirst = if lang.isDefined then cont else first
    val body = lines.zipWithIndex.map: (l, i) =>
      val pfx = (if i == 0 then bodyFirst else cont) + rail
      // Code is verbatim: char-wrap (never break between words / collapse spaces).
      wrapText(pfx, cont + rail, CodeSeq + l + Ansi.Reset, Wrap.Char)
    val tagElem = lang.map(l => Text(first + dimText(l))).toVector
    layout((tagElem ++ body)*)

  /* ---- lists ---- */

  private def renderListBlock(lb: Block.ListBlock, first: String, cont: String): Element =
    val itemElems = lb.items.zipWithIndex.map: (item, idx) =>
      val markerVisible = listMarker(lb, idx, item)
      val marker = DimSeq + markerVisible + Ansi.Reset
      val itemFirst = (if idx == 0 then first else cont) + marker
      val itemCont = cont + (" " * Width.stringWidth(markerVisible))
      joinSpaced(renderList(item.blocks, itemFirst, itemCont), spaced = !lb.tight)
    joinSpaced(itemElems, spaced = !lb.tight)

  private def listMarker(lb: Block.ListBlock, idx: Int, item: ListItem): String =
    val bullet = if lb.ordered then s"${lb.start + idx}. " else "• "
    val box = item.task match
      case Some(true)  => "☑ "
      case Some(false) => "☐ "
      case None        => ""
    bullet + box

  /* ---- tables ---- */

  /** Build a width-agnostic [[Element.TableNode]]; the actual column distribution
    * and cell wrapping happen in [[auk.tui.app.Layout]], which knows the width. */
  private def renderTable(t: Block.Table, first: String, cont: String): Element =
    if t.align.isEmpty then Empty
    else
      val header = t.header.map(c => runsToSpans(inlineRuns(c, Style.Bold)))
      val rows = t.rows.map(_.map(c => runsToSpans(inlineRuns(c, Style.Default))))
      table(first, cont, t.align.map(columnAlign), header, rows, Style.Dim)

  private def columnAlign(a: Align): ColumnAlign = a match
    case Align.Right  => ColumnAlign.Right
    case Align.Center => ColumnAlign.Center
    case _            => ColumnAlign.Left

  private def runsToSpans(runs: Vector[Run]): Vector[Span] =
    runs.map(r => Span(r.text, r.style))

  /* ---- inline → styled runs → SGR string ---- */

  private final case class Run(text: String, style: Style)

  private def inlineRuns(content: Vector[Inline], base: Style): Vector[Run] =
    val out = Vector.newBuilder[Run]
    def go(ins: Vector[Inline], style: Style): Unit = ins.foreach:
      case Inline.Text(v)          => out += Run(v, style)
      case Inline.SoftBreak        => out += Run(" ", style)
      case Inline.HardBreak        => out += Run("\n", style)
      case Inline.Emph(c)          => go(c, style.withAttr(Attr.Italic))
      case Inline.Strong(c)        => go(c, style.withAttr(Attr.Bold))
      case Inline.Strikethrough(c) =>
        val inner = inlineRuns(c, style)
        inner.foreach(r => out += Run(strike(r.text), r.style))
      case Inline.Code(v)          => out += Run(v, style ++ InlineCodeStyle)
      case Inline.Link(c, _, _)    => go(c, style ++ LinkStyle)
      case Inline.Image(alt, _, _) => out += Run(alt, style.withAttr(Attr.Dim))
      case Inline.Autolink(uri, _) => out += Run(uri, style ++ LinkStyle)
    go(content, base)
    out.result()

  private def serialize(runs: Vector[Run]): String =
    if runs.isEmpty then ""
    else
      val sb = new StringBuilder
      runs.foreach(r => if r.text.nonEmpty then sb.append(r.style.setSequence).append(r.text))
      sb.append(Ansi.Reset)
      sb.toString

  /** Overlay the reveal glow on the trailing `hot` code points of `runs`: a
    * gradient from the cool end (just past the settled text) to the hot end (the
    * newest code point), composed onto each run's own style — bold/italic survive,
    * only the foreground is blended. */
  private def glowRuns(runs: Vector[Run], hot: Int): Vector[Run] =
    if hot <= 0 then runs
    else
      // Expand to (codePoint, style), then recolour the last `hot` of them.
      val cps = scala.collection.mutable.ArrayBuffer.empty[(Int, Style)]
      runs.foreach: r =>
        var k = 0
        while k < r.text.length do
          val cp = r.text.codePointAt(k)
          cps += ((cp, r.style))
          k += Character.charCount(cp)
      val total = cps.length
      val start = math.max(0, total - hot)
      val zone = total - start
      val out = Vector.newBuilder[Run]
      val buf = new StringBuilder
      var curStyle: Style = if cps.isEmpty then Style.Default else cps.head._2
      def flush(): Unit =
        if buf.nonEmpty then { out += Run(buf.toString, curStyle); buf.setLength(0) }
      var idx = 0
      while idx < total do
        val (cp, st) = cps(idx)
        val style =
          if idx >= start then st.withFg(mix(Glow.AnswerCool, Glow.AnswerHot, (idx - start + 1).toDouble / zone))
          else st
        if buf.nonEmpty && style != curStyle then flush()
        if buf.isEmpty then curStyle = style
        buf.appendAll(Character.toChars(cp))
        idx += 1
      flush()
      out.result()

  /* ---- helpers ---- */

  private def dimText(s: String): String = Style.Dim.setSequence + s + Ansi.Reset

  /** Overlay a combining long stroke on each code point — our terminal-friendly
    * strikethrough (the [[Style]] has no SGR-9 strikethrough attribute). */
  private def strike(text: String): String =
    val sb = new StringBuilder
    var k = 0
    while k < text.length do
      val cp = text.codePointAt(k)
      sb.appendAll(Character.toChars(cp))
      sb.append('\u0336')
      k += Character.charCount(cp)
    sb.toString

  private def mix(cool: Glow.Rgb, hot: Glow.Rgb, t: Double): Color =
    def lerp(a: Int, b: Int): Int = math.max(0, math.min(255, math.round(a + (b - a) * t).toInt))
    Color.True(lerp(cool._1, hot._1), lerp(cool._2, hot._2), lerp(cool._3, hot._3))
