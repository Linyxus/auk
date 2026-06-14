package auk.tui.markdown

/** Parses a sequence of lines into the block [[Block]] AST by recursive descent.
  *
  * The grammar is a GFM subset: ATX + setext headings, paragraphs, fenced and
  * indented code, blockquotes, bullet/ordered lists (tight/loose, task items),
  * tables, and thematic breaks. Inline content is parsed by [[InlineParser]] at
  * block construction, so a finalised block never re-parses.
  *
  * [[parseSpans]] reports the line span of each top-level block; the incremental
  * [[MarkdownDocument]] uses it to finalise everything but the trailing block and
  * re-parse only the open tail.
  */
object BlockParser:

  /** A top-level block together with the line index (exclusive) just past it in
    * the parsed input — i.e. where the next block begins. */
  private[markdown] final case class BlockSpan(block: Block, endLine: Int)

  def parse(text: String): Vector[Block] =
    parseSpans(splitLines(text)).map(_.block)

  def splitLines(text: String): Vector[String] =
    // Normalise CRLF; a trailing newline yields no spurious empty final line.
    val t = text.replace("\r\n", "\n").replace('\r', '\n')
    if t.isEmpty then Vector.empty
    else
      val parts = t.split("\n", -1).toVector
      if parts.nonEmpty && parts.last.isEmpty then parts.init else parts

  private[markdown] def parseBlocks(lines: Vector[String]): Vector[Block] =
    parseSpans(lines).map(_.block)

  private[markdown] def parseSpans(lines: Vector[String]): Vector[BlockSpan] =
    val out = Vector.newBuilder[BlockSpan]
    var i = 0
    val n = lines.length
    while i < n do
      if isBlank(lines(i)) then i += 1
      else
        val (block, next) = parseOne(lines, i)
        out += BlockSpan(block, next)
        i = next
    out.result()

  /* ---- one block starting at `i`; returns (block, nextIndex) ---- */

  private def parseOne(lines: Vector[String], i: Int): (Block, Int) =
    val line = lines(i)
    val indent = indentWidth(line)
    if indent >= 4 then parseIndentedCode(lines, i)
    else
      fenceInfo(line) match
        case Some((ch, len, info)) => parseFenced(lines, i, ch, len, info)
        case None =>
          if isAtxHeading(line) then (parseAtx(line), i + 1)
          else if isThematicBreak(line) then (Block.ThematicBreak, i + 1)
          else if startsBlockquote(line) then parseQuote(lines, i)
          else
            listMarker(line) match
              case Some(m) => parseList(lines, i, m)
              case None =>
                if i + 1 < lines.length && isDelimiterRow(lines(i + 1)) && looksLikeTable(line, lines(i + 1)) then
                  parseTable(lines, i)
                else parseParagraph(lines, i)

  /* ---- ATX heading ---- */

  private def isAtxHeading(line: String): Boolean =
    val s = line.drop(indentChars(line))
    var h = 0
    while h < s.length && s.charAt(h) == '#' do h += 1
    h >= 1 && h <= 6 && (h == s.length || s.charAt(h) == ' ' || s.charAt(h) == '\t')

  private def parseAtx(line: String): Block =
    val s = line.drop(indentChars(line))
    var h = 0
    while h < s.length && s.charAt(h) == '#' do h += 1
    var content = s.substring(h).trim
    // Strip an optional closing run of #'s (must be preceded by a space, or be all).
    if content.nonEmpty then
      val stripped = content.replaceAll("\\s+#+\\s*$", "")
      if stripped.nonEmpty || content.matches("#+\\s*") then content = stripped
    Block.Heading(h, InlineParser.parse(content))

  /* ---- thematic break ---- */

  private def isThematicBreak(line: String): Boolean =
    val s = line.drop(indentChars(line)).filter(_ != ' ').filter(_ != '\t')
    s.length >= 3 && (s.forall(_ == '-') || s.forall(_ == '*') || s.forall(_ == '_'))

  /* ---- setext underline ---- */

  private def setextLevel(line: String): Int =
    if indentWidth(line) >= 4 then 0
    else
      val s = line.drop(indentChars(line)).replaceAll("\\s+$", "")
      if s.nonEmpty && s.forall(_ == '=') then 1
      else if s.nonEmpty && s.forall(_ == '-') then 2
      else 0

  /* ---- fenced code ---- */

  private def fenceInfo(line: String): Option[(Char, Int, String)] =
    val start = indentChars(line)
    if start >= line.length then None
    else
      val ch = line.charAt(start)
      if ch != '`' && ch != '~' then None
      else
        var len = 0
        while start + len < line.length && line.charAt(start + len) == ch do len += 1
        if len < 3 then None
        else
          val info = line.substring(start + len).trim
          // A backtick info string may not contain a backtick.
          if ch == '`' && info.contains('`') then None
          else Some((ch, len, info))

  private def parseFenced(lines: Vector[String], i: Int, ch: Char, len: Int, info: String): (Block, Int) =
    val openIndent = indentWidth(lines(i))
    var j = i + 1
    val body = Vector.newBuilder[String]
    var closed = false
    while j < lines.length && !closed do
      val l = lines(j)
      if isClosingFence(l, ch, len) then { closed = true; j += 1 }
      else { body += stripUpTo(l, openIndent); j += 1 }
    val infoOpt = Option(info).filter(_.nonEmpty)
    val lang = infoOpt.map(_.takeWhile(c => c != ' ' && c != '\t')).filter(_.nonEmpty)
    (Block.Code(infoOpt, lang, body.result().mkString("\n"), fenced = true), j)

  private def isClosingFence(line: String, ch: Char, openLen: Int): Boolean =
    if indentWidth(line) >= 4 then false
    else
      val s = line.drop(indentChars(line)).replaceAll("\\s+$", "")
      s.length >= openLen && s.forall(_ == ch)

  /* ---- indented code ---- */

  private def parseIndentedCode(lines: Vector[String], i: Int): (Block, Int) =
    val body = Vector.newBuilder[String]
    var j = i
    var lastNonBlank = i
    while j < lines.length && (indentWidth(lines(j)) >= 4 || isBlank(lines(j))) do
      if !isBlank(lines(j)) then lastNonBlank = j
      j += 1
    // Trim trailing blank lines from the block (they belong outside).
    val end = lastNonBlank + 1
    var k = i
    while k < end do
      body += (if isBlank(lines(k)) then "" else stripUpTo(lines(k), 4))
      k += 1
    (Block.Code(None, None, body.result().mkString("\n"), fenced = false), end)

  /* ---- blockquote ---- */

  private def startsBlockquote(line: String): Boolean =
    val start = indentChars(line)
    start < line.length && line.charAt(start) == '>'

  private def parseQuote(lines: Vector[String], i: Int): (Block, Int) =
    val inner = Vector.newBuilder[String]
    var j = i
    while j < lines.length && startsBlockquote(lines(j)) do
      val start = indentChars(lines(j))
      var rest = lines(j).substring(start + 1)
      if rest.startsWith(" ") then rest = rest.substring(1)
      inner += rest
      j += 1
    (Block.Quote(parseBlocks(inner.result())), j)

  /* ---- lists ---- */

  private final case class Marker(ordered: Boolean, start: Int, bullet: Char, markerEnd: Int, contentIndent: Int)

  private def listMarker(line: String): Option[Marker] =
    val start = indentChars(line)
    if start >= line.length then None
    else
      val c = line.charAt(start)
      if c == '-' || c == '+' || c == '*' then
        afterMarker(line, start + 1).map { case (spaces, contentIndent) =>
          Marker(ordered = false, start = 1, bullet = c, markerEnd = start + 1, contentIndent = indentWidth(line.take(start)) + 1 + spaces)
        }
      else if c.isDigit then
        var k = start
        while k < line.length && line.charAt(k).isDigit && (k - start) < 9 do k += 1
        if k < line.length && (line.charAt(k) == '.' || line.charAt(k) == ')') then
          afterMarker(line, k + 1).map { case (spaces, _) =>
            Marker(ordered = true, start = line.substring(start, k).toInt, bullet = line.charAt(k),
              markerEnd = k + 1, contentIndent = indentWidth(line.take(start)) + (k + 1 - start) + spaces)
          }
        else None
      else None

  /** After the marker punctuation: requires a space/tab or end-of-line. Returns
    * the number of spaces consumed (1 if >4, the indentation folds into content)
    * and the resulting content indentation column. */
  private def afterMarker(line: String, at: Int): Option[(Int, Int)] =
    if at >= line.length then Some((1, 0)) // empty item
    else if line.charAt(at) == ' ' || line.charAt(at) == '\t' then
      var spaces = 0
      var k = at
      while k < line.length && (line.charAt(k) == ' ' || line.charAt(k) == '\t') do { spaces += 1; k += 1 }
      if k >= line.length then Some((1, 0)) // blank after marker → one-space convention
      else Some((math.min(spaces, 4), 0))
    else None

  private def sameListType(a: Marker, b: Marker): Boolean =
    a.ordered == b.ordered && (if a.ordered then a.bullet == b.bullet else a.bullet == b.bullet)

  private def parseList(lines: Vector[String], i: Int, first: Marker): (Block, Int) =
    val items = Vector.newBuilder[ListItem]
    var j = i
    var loose = false
    var sawTrailingBlank = false
    var firstItem = true
    while j < lines.length && listMarker(lines(j)).exists(m => sameListType(m, first)) do
      if sawTrailingBlank && !firstItem then loose = true
      val m = listMarker(lines(j)).get
      val (itemLines, next, innerBlank) = gatherItem(lines, j, m)
      if innerBlank then loose = true
      val blocks = parseBlocks(itemLines)
      items += toListItem(blocks)
      // Detect a blank line separating this item from the next marker.
      var b = next
      var blanks = false
      while b < lines.length && isBlank(lines(b)) do { blanks = true; b += 1 }
      sawTrailingBlank = blanks
      j = b
      firstItem = false
    (Block.ListBlock(first.ordered, first.start, tight = !loose, items.result()), j)

  /** Gather the lines of one item, dedented to its content column. Returns the
    * dedented lines, the next line index, and whether an internal blank line
    * appeared between content (a looseness signal). */
  private def gatherItem(lines: Vector[String], i: Int, m: Marker): (Vector[String], Int, Boolean) =
    val out = Vector.newBuilder[String]
    // First line: drop the marker itself (not just whitespace), so the content
    // doesn't re-parse as the same list and recurse forever.
    out += dropColumns(lines(i), m.contentIndent)
    var j = i + 1
    var innerBlank = false
    var continue = true
    while j < lines.length && continue do
      val l = lines(j)
      if isBlank(l) then
        // A blank run stays internal only if the item resumes after it; otherwise
        // it separates this item from the next and is left for the caller (so
        // list looseness is detected there).
        var k = j
        while k < lines.length && isBlank(lines(k)) do k += 1
        if k < lines.length && indentWidth(lines(k)) >= m.contentIndent then
          innerBlank = true
          for _ <- j until k do out += ""
          j = k
        else continue = false
      else if indentWidth(l) >= m.contentIndent then
        out += stripUpTo(l, m.contentIndent)
        j += 1
      else if !startsNewBlock(l) then
        out += l.drop(indentChars(l)) // lazy paragraph continuation
        j += 1
      else continue = false
    (out.result(), j, innerBlank)

  private def startsNewBlock(line: String): Boolean =
    isAtxHeading(line) || isThematicBreak(line) || fenceInfo(line).isDefined ||
      startsBlockquote(line) || listMarker(line).isDefined || indentWidth(line) >= 4

  private def toListItem(blocks: Vector[Block]): ListItem =
    blocks.headOption match
      case Some(Block.Paragraph(content)) =>
        taskMarker(content) match
          case Some((checked, rest)) =>
            ListItem(Block.Paragraph(rest) +: blocks.tail, Some(checked))
          case None => ListItem(blocks, None)
      case _ => ListItem(blocks, None)

  /** A leading `[ ]`/`[x]`/`[X]` followed by a space turns an item into a task. */
  private def taskMarker(content: Vector[Inline]): Option[(Boolean, Vector[Inline])] =
    content.headOption match
      case Some(Inline.Text(t)) =>
        val checkedBox = t.startsWith("[x] ") || t.startsWith("[X] ")
        val uncheckedBox = t.startsWith("[ ] ")
        if checkedBox || uncheckedBox then
          val rest = t.substring(4)
          val newHead: Vector[Inline] = if rest.isEmpty then Vector.empty else Vector(Inline.Text(rest))
          Some((checkedBox, newHead ++ content.tail))
        else None
      case _ => None

  /* ---- tables ---- */

  private def isDelimiterRow(line: String): Boolean =
    val cells = splitTableRow(line)
    cells.nonEmpty && cells.forall { c =>
      val t = c.trim
      t.nonEmpty && t.matches(":?-+:?")
    }

  private def looksLikeTable(header: String, delim: String): Boolean =
    // A pipe must appear somewhere, else a single-column "---" delimiter would
    // shadow a thematic break or setext underline.
    !isBlank(header) && indentWidth(header) < 4 &&
      (header.contains('|') || delim.contains('|')) &&
      splitTableRow(header).length == splitTableRow(delim).length

  private def parseTable(lines: Vector[String], i: Int): (Block, Int) =
    val headerCells = splitTableRow(lines(i)).map(c => InlineParser.parse(c.trim))
    val aligns = splitTableRow(lines(i + 1)).map(alignOf)
    val rows = Vector.newBuilder[Vector[Vector[Inline]]]
    var j = i + 2
    while j < lines.length && !isBlank(lines(j)) && lines(j).contains('|') && indentWidth(lines(j)) < 4 do
      val cells = splitTableRow(lines(j)).map(c => InlineParser.parse(c.trim))
      rows += rectangular(cells, aligns.length)
      j += 1
    (Block.Table(aligns, rectangular(headerCells, aligns.length), rows.result()), j)

  /** Index of a run of exactly `len` backticks at/after `from`, or -1. */
  private def closingBacktick(s: String, from: Int, len: Int): Int =
    var j = from
    while j < s.length do
      if s.charAt(j) == '`' then
        var m = 0
        while j + m < s.length && s.charAt(j + m) == '`' do m += 1
        if m == len then return j else j += m
      else j += 1
    -1

  private def alignOf(cell: String): Align =
    val t = cell.trim
    val left = t.startsWith(":")
    val right = t.endsWith(":")
    if left && right then Align.Center
    else if right then Align.Right
    else if left then Align.Left
    else Align.None

  private def rectangular(cells: Vector[Vector[Inline]], cols: Int): Vector[Vector[Inline]] =
    if cells.length == cols then cells
    else if cells.length > cols then cells.take(cols)
    else cells ++ Vector.fill(cols - cells.length)(Vector.empty)

  /** Split a table row on unescaped `|`, dropping the optional leading/trailing
    * pipe. Backslash-escaped pipes stay in the cell. */
  private def splitTableRow(line: String): Vector[String] =
    val s = line.drop(indentChars(line))
    val cells = Vector.newBuilder[String]
    val cur = new StringBuilder
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      if c == '\\' && k + 1 < s.length then { cur.append(c).append(s.charAt(k + 1)); k += 2 }
      else if c == '`' then
        // Copy a code span verbatim so its pipes don't split the row.
        var run = 0
        while k + run < s.length && s.charAt(k + run) == '`' do run += 1
        val end = closingBacktick(s, k + run, run)
        val stop = if end < 0 then k + run else end + run
        cur.append(s.substring(k, stop))
        k = stop
      else if c == '|' then { cells += cur.toString; cur.setLength(0); k += 1 }
      else { cur.append(c); k += 1 }
    cells += cur.toString
    var result = cells.result()
    // Drop the empty cell produced by a leading / trailing pipe.
    if result.nonEmpty && result.head.trim.isEmpty && s.startsWith("|") then result = result.tail
    if result.nonEmpty && result.last.trim.isEmpty && s.replaceAll("\\s+$", "").endsWith("|") then result = result.init
    result

  /* ---- paragraph (with setext) ---- */

  private def parseParagraph(lines: Vector[String], i: Int): (Block, Int) =
    val buf = Vector.newBuilder[String]
    buf += lines(i).drop(indentChars(lines(i)))
    var j = i + 1
    var result: Option[(Block, Int)] = None
    while j < lines.length && result.isEmpty do
      val l = lines(j)
      val sx = setextLevel(l)
      if isBlank(l) then result = Some((para(buf.result()), j))
      else if sx > 0 then
        result = Some((Block.Heading(sx, InlineParser.parse(buf.result().mkString("\n"))), j + 1))
      else if interruptsParagraph(l, j, lines) then result = Some((para(buf.result()), j))
      else { buf += l.drop(indentChars(l)); j += 1 }
    result.getOrElse((para(buf.result()), j))

  private def para(buf: Vector[String]): Block =
    Block.Paragraph(InlineParser.parse(buf.mkString("\n")))

  /** Block starts that interrupt an open paragraph. */
  private def interruptsParagraph(line: String, idx: Int, lines: Vector[String]): Boolean =
    isAtxHeading(line) || isThematicBreak(line) || fenceInfo(line).isDefined ||
      startsBlockquote(line) ||
      // Only a bullet or an ordered list starting at 1 interrupts a paragraph,
      // and only with non-empty content (so a stray "1." in prose doesn't).
      listMarker(line).exists(m => (!m.ordered || m.start == 1) && line.drop(m.markerEnd).trim.nonEmpty) ||
      (idx + 1 < lines.length && isDelimiterRow(lines(idx + 1)) && looksLikeTable(line, lines(idx + 1)))

  /* ---- shared helpers ---- */

  def isBlank(line: String): Boolean = line.forall(c => c == ' ' || c == '\t')

  /** The number of leading whitespace *characters* (not columns). */
  private def indentChars(line: String): Int =
    var k = 0
    while k < line.length && (line.charAt(k) == ' ' || line.charAt(k) == '\t') do k += 1
    k

  /** The leading indentation in display columns, tabs expanded to stops of 4. */
  private def indentWidth(line: String): Int =
    var col = 0
    var k = 0
    while k < line.length && (line.charAt(k) == ' ' || line.charAt(k) == '\t') do
      col += (if line.charAt(k) == '\t' then 4 - (col % 4) else 1)
      k += 1
    col

  /** Drop exactly `cols` display columns from the start, whatever the characters
    * (used to remove a list marker from an item's first line). */
  private def dropColumns(line: String, cols: Int): String =
    var col = 0
    var k = 0
    while k < line.length && col < cols do
      col += (if line.charAt(k) == '\t' then 4 - (col % 4) else 1)
      k += 1
    line.substring(k)

  /** Strip up to `cols` columns of leading whitespace. */
  private def stripUpTo(line: String, cols: Int): String =
    var col = 0
    var k = 0
    while k < line.length && col < cols && (line.charAt(k) == ' ' || line.charAt(k) == '\t') do
      col += (if line.charAt(k) == '\t' then 4 - (col % 4) else 1)
      k += 1
    line.substring(k)
