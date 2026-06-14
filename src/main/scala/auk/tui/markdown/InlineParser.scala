package auk.tui.markdown

import scala.collection.mutable

/** Parses the inline content of a single block: a `String` into a
  * `Vector[Inline]`.
  *
  * Block-local by construction (it never crosses a block boundary), which is
  * what makes the whole renderer incremental — a finalised block parses its
  * inlines exactly once, and only the still-open trailing block re-parses.
  *
  * The algorithm is the CommonMark two-pass: a left-to-right scan that emits a
  * linked list of nodes and pushes emphasis runs onto a delimiter stack and
  * link/image brackets onto a bracket stack, then a resolution pass that turns
  * matched delimiters into [[Inline.Strong]]/[[Inline.Emph]]/[[Inline.Strikethrough]]
  * and matched brackets into [[Inline.Link]]/[[Inline.Image]]. Unmatched
  * delimiters and brackets stay literal — which is exactly the streaming
  * behaviour we want: a half-typed `**bold` shows the markers until its closer
  * arrives, then snaps to bold.
  */
object InlineParser:

  def parse(input: String): Vector[Inline] =
    if input.isEmpty then Vector.empty
    else Parser(input).run()

  /** Flatten inline content back to its plain text — used for image alt text. */
  def plain(content: Vector[Inline]): String =
    val sb = new StringBuilder
    def go(ins: Vector[Inline]): Unit = ins.foreach:
      case Inline.Text(v)            => sb.append(v)
      case Inline.Code(v)            => sb.append(v)
      case Inline.Emph(c)            => go(c)
      case Inline.Strong(c)          => go(c)
      case Inline.Strikethrough(c)   => go(c)
      case Inline.Link(c, _, _)      => go(c)
      case Inline.Image(alt, _, _)   => sb.append(alt)
      case Inline.Autolink(uri, _)   => sb.append(uri)
      case Inline.HardBreak          => sb.append(' ')
      case Inline.SoftBreak          => sb.append(' ')
    go(content)
    sb.toString

  /* ---- linked-list nodes built during the scan ---- */

  private sealed abstract class Node:
    var prev: Node | Null = null
    var next: Node | Null = null

  /** Accumulated literal text. */
  private final class TextNode(var text: String) extends Node

  /** A resolved inline (code span, autolink, break, or a wrapped emphasis/link). */
  private final class DoneNode(val inline: Inline) extends Node

  /** A run of `*`, `_`, or `~`; may still be partly consumed during resolution. */
  private final class DelimNode(
      val ch: Char,
      var length: Int,
      val canOpen: Boolean,
      val canClose: Boolean
  ) extends Node:
    var inStack: Boolean = true

  /** A `[` or `![` opener; resolved (or abandoned) when its `]` is seen. */
  private final class BracketNode(val image: Boolean) extends Node:
    var active: Boolean = true

  private final class Parser(s: String):
    private val n = s.length
    // Sentinel head simplifies splicing; real nodes follow it.
    private val head = new TextNode("")
    private var tail: Node = head
    private val delims = mutable.ArrayBuffer.empty[DelimNode]
    private val brackets = mutable.ArrayBuffer.empty[BracketNode]
    private var i = 0
    // True at a position where the previous char was a line start or break, so
    // leading spaces of a continuation line are swallowed.
    private var atLineStart = true

    private def append(node: Node): Unit =
      tail.next = node
      node.prev = tail
      node.next = null
      tail = node

    private def appendText(t: String): Unit =
      if t.isEmpty then ()
      else
        tail match
          case tn: TextNode if !tn.eq(head) => tn.text += t
          case _                            => append(new TextNode(t))

    def run(): Vector[Inline] =
      scan()
      resolveEmphasis(0)
      flatten(head.next, null)

    /* ---- phase 1: scan ---- */

    private def scan(): Unit =
      while i < n do
        val c = s.charAt(i)
        c match
          case '\\'                  => scanBackslash()
          case '`'                   => scanCode()
          case '<'                   => scanAutolink()
          case '\n'                  => scanNewline()
          case '*' | '_' | '~'       => scanDelim(c)
          case '!' if peek(1) == '[' => append(mkBracket(image = true)); i += 2; atLineStart = false
          case '['                   => append(mkBracket(image = false)); i += 1; atLineStart = false
          case ']'                   => scanCloseBracket()
          case _                     => scanText(c)

    private def mkBracket(image: Boolean): BracketNode =
      val b = new BracketNode(image)
      brackets += b
      b

    private def peek(d: Int): Char = if i + d < n then s.charAt(i + d) else ' '

    private def scanText(c: Char): Unit =
      if atLineStart && (c == ' ' || c == '\t') then { i += 1; return }
      atLineStart = false
      appendText(c.toString)
      i += 1

    private def scanBackslash(): Unit =
      val nx = peek(1)
      if nx == '\n' then
        append(new DoneNode(Inline.HardBreak)); i += 2; atLineStart = true
      else if isAsciiPunct(nx) then
        appendText(nx.toString); i += 2; atLineStart = false
      else
        appendText("\\"); i += 1; atLineStart = false

    private def scanCode(): Unit =
      var k = 0
      while i + k < n && s.charAt(i + k) == '`' do k += 1
      // Find a closing run of exactly k backticks.
      var j = i + k
      var found = -1
      while j < n && found < 0 do
        if s.charAt(j) == '`' then
          var m = 0
          while j + m < n && s.charAt(j + m) == '`' do m += 1
          if m == k then found = j else j += m
        else j += 1
      if found < 0 then
        appendText("`" * k); i += k; atLineStart = false
      else
        var content = s.substring(i + k, found).replace('\n', ' ')
        // Strip one leading and trailing space when the content isn't all spaces.
        if content.length >= 2 && content.startsWith(" ") && content.endsWith(" ") && content.exists(_ != ' ') then
          content = content.substring(1, content.length - 1)
        append(new DoneNode(Inline.Code(content)))
        i = found + k
        atLineStart = false

    private def scanAutolink(): Unit =
      val close = s.indexOf('>', i + 1)
      val inner = if close > 0 then s.substring(i + 1, close) else ""
      if close > 0 && isUriAutolink(inner) then
        append(new DoneNode(Inline.Autolink(inner, isEmail = false))); i = close + 1; atLineStart = false
      else if close > 0 && isEmailAutolink(inner) then
        append(new DoneNode(Inline.Autolink(inner, isEmail = true))); i = close + 1; atLineStart = false
      else
        appendText("<"); i += 1; atLineStart = false

    private def scanNewline(): Unit =
      // A hard break is two+ trailing spaces (backslash-newline handled in scanBackslash).
      var hard = false
      tail match
        case tn: TextNode if tn.text.endsWith("  ") =>
          hard = true
          tn.text = tn.text.replaceAll("[ \t]+$", "")
        case tn: TextNode =>
          tn.text = tn.text.replaceAll("[ \t]+$", "")
        case _ => ()
      append(new DoneNode(if hard then Inline.HardBreak else Inline.SoftBreak))
      i += 1
      atLineStart = true

    private def scanDelim(ch: Char): Unit =
      atLineStart = false
      var len = 0
      while i + len < n && s.charAt(i + len) == ch do len += 1
      val before = if i == 0 then ' ' else s.charAt(i - 1)
      val after = if i + len >= n then ' ' else s.charAt(i + len)
      val beforeWs = isWhitespace(before)
      val afterWs = isWhitespace(after)
      val beforePunct = isPunct(before)
      val afterPunct = isPunct(after)
      val leftFlank = !afterWs && (!afterPunct || beforeWs || beforePunct)
      val rightFlank = !beforeWs && (!beforePunct || afterWs || afterPunct)
      val (canOpen, canClose) =
        if ch == '_' then
          (leftFlank && (!rightFlank || beforePunct), rightFlank && (!leftFlank || afterPunct))
        else (leftFlank, rightFlank)
      val d = new DelimNode(ch, len, canOpen, canClose)
      append(d)
      delims += d
      i += len

    private def scanCloseBracket(): Unit =
      atLineStart = false
      i += 1 // consume ']'
      if brackets.isEmpty then { appendText("]"); return }
      val opener = brackets.remove(brackets.length - 1)
      if !opener.active then { appendText("]"); return }
      parseLinkTail(i) match
        case Some((dest, title, end)) =>
          if opener.image then
            val alt = InlineParser.plain(flatten(opener.next, null))
            spliceWrap(opener, new DoneNode(Inline.Image(alt, dest, title)))
          else
            // Resolve emphasis inside the link text before wrapping it.
            processEmphasisFrom(opener)
            val content = flatten(opener.next, null)
            spliceWrap(opener, new DoneNode(Inline.Link(content, dest, title)))
            // No links inside links: disable earlier link openers.
            brackets.foreach(b => if !b.image then b.active = false)
          i = end
        case None =>
          // Not a link (reference links unsupported): the brackets stay literal.
          appendText("]")

    /* ---- link destination + title after `](` ---- */

    private def parseLinkTail(from: Int): Option[(String, Option[String], Int)] =
      if from >= n || s.charAt(from) != '(' then None
      else
        var j = skipWs(from + 1)
        parseDest(j) match
          case None => None
          case Some((dest, afterDest)) =>
            j = skipWs(afterDest)
            val (title, afterTitle) = parseTitle(j)
            j = skipWs(afterTitle)
            if j < n && s.charAt(j) == ')' then Some((dest, title, j + 1)) else None

    private def parseDest(from: Int): Option[(String, Int)] =
      if from >= n then None
      else if s.charAt(from) == '<' then
        val close = s.indexOf('>', from + 1)
        if close < 0 || s.substring(from + 1, close).contains('\n') then None
        else Some((unescape(s.substring(from + 1, close)), close + 1))
      else
        var j = from
        var depth = 0
        var ok = true
        while j < n && ok do
          val c = s.charAt(j)
          if c == '\\' && j + 1 < n then j += 2
          else if isWhitespace(c) then ok = false
          else if c == '(' then { depth += 1; j += 1 }
          else if c == ')' then
            if depth == 0 then ok = false else { depth -= 1; j += 1 }
          else j += 1
        if depth != 0 || j == from then None
        else Some((unescape(s.substring(from, j)), j))

    private def parseTitle(from: Int): (Option[String], Int) =
      if from >= n then (None, from)
      else
        val q = s.charAt(from)
        if q != '"' && q != '\'' && q != '(' then (None, from)
        else
          val close = if q == '(' then ')' else q
          var j = from + 1
          val sb = new StringBuilder
          var done = false
          while j < n && !done do
            val c = s.charAt(j)
            if c == '\\' && j + 1 < n then { sb.append(s.charAt(j + 1)); j += 2 }
            else if c == close then { done = true; j += 1 }
            else { sb.append(c); j += 1 }
          if done then (Some(sb.toString), j) else (None, from)

    private def skipWs(from: Int): Int =
      var j = from
      while j < n && isWhitespace(s.charAt(j)) do j += 1
      j

    /* ---- linked-list helpers ---- */

    /** Replace the chain from `from` to the tail with `repl` (dropping `from`
      * itself, the opener bracket). Delimiters in the dropped range leave the
      * stack. */
    private def spliceWrap(from: Node, repl: Node): Unit =
      val before = from.prev.nn
      before.next = repl
      repl.prev = before
      repl.next = null
      tail = repl
      var cur: Node | Null = from
      while cur != null do
        val c = cur
        c match
          case d: DelimNode => d.inStack = false
          case _            => ()
        cur = c.next

    /** Flatten the chain [from, until) into inline nodes, merging text. */
    private def flatten(from: Node | Null, until: Node | Null): Vector[Inline] =
      val out = Vector.newBuilder[Inline]
      val text = new StringBuilder
      def flush(): Unit =
        if text.nonEmpty then { out += Inline.Text(text.toString); text.setLength(0) }
      var cur: Node | Null = from
      while cur != null && (until == null || !cur.eq(until)) do
        val c = cur
        c match
          case tn: TextNode   => text.append(tn.text)
          case d: DelimNode   => text.append(d.ch.toString * d.length)
          case dn: DoneNode   => flush(); out += dn.inline
          case b: BracketNode => text.append(if b.image then "![" else "[")
        cur = c.next
      flush()
      out.result()

    /* ---- phase 2: emphasis resolution ---- */

    /** Resolve emphasis whose opener follows `opener` (used to close out a
      * link's inner content before the rest of the line). */
    private def processEmphasisFrom(opener: Node): Unit =
      val bottom = delims.indexWhere(d => follows(opener, d))
      resolveEmphasis(if bottom < 0 then delims.length else bottom)

    private def follows(a: Node, b: Node): Boolean =
      var cur: Node | Null = a.next
      while cur != null do
        val c = cur
        if c.eq(b) then return true
        cur = c.next
      false

    private def resolveEmphasis(bottom: Int): Unit =
      val openersBottom = mutable.HashMap.empty[(Char, Int, Boolean), Int]
      var ci = bottom
      while ci < delims.length do
        val closer = delims(ci)
        if closer.inStack && closer.canClose then
          val key = (closer.ch, closer.length % 3, closer.canOpen)
          val floor = openersBottom.getOrElse(key, bottom)
          var oi = ci - 1
          var openerIdx = -1
          while oi >= floor && oi >= bottom && openerIdx < 0 do
            val opener = delims(oi)
            if opener.inStack && opener.canOpen && opener.ch == closer.ch && matches(opener, closer) then
              openerIdx = oi
            else oi -= 1
          if openerIdx >= 0 then
            val opener = delims(openerIdx)
            val use =
              if opener.ch == '~' then math.min(opener.length, closer.length).min(2)
              else if opener.length >= 2 && closer.length >= 2 then 2
              else 1
            wrap(opener, closer, use)
            var k = openerIdx + 1
            while k < ci do { delims(k).inStack = false; k += 1 }
            if opener.length == 0 then opener.inStack = false
            if closer.length == 0 then { closer.inStack = false; ci += 1 }
          else
            openersBottom(key) = ci
            if !closer.canOpen then closer.inStack = false
            ci += 1
        else ci += 1

    private def matches(opener: DelimNode, closer: DelimNode): Boolean =
      if opener.ch == '~' then true
      else
        val oddMatch = (opener.canClose || closer.canOpen) &&
          (opener.length + closer.length) % 3 == 0 &&
          !(opener.length % 3 == 0 && closer.length % 3 == 0)
        !oddMatch

    /** Wrap the content strictly between opener and closer in a styled node,
      * consuming `use` characters from each delimiter. */
    private def wrap(opener: DelimNode, closer: DelimNode, use: Int): Unit =
      val content = flatten(opener.next, closer)
      val inner =
        if opener.ch == '~' then Inline.Strikethrough(content)
        else if use == 2 then Inline.Strong(content)
        else Inline.Emph(content)
      var cur: Node | Null = opener.next
      while cur != null && !cur.eq(closer) do
        val c = cur
        c match
          case d: DelimNode => d.inStack = false
          case _            => ()
        cur = c.next
      val wrapper = new DoneNode(inner)
      opener.length -= use
      closer.length -= use
      wrapper.prev = opener
      wrapper.next = closer
      opener.next = wrapper
      closer.prev = wrapper

    /* ---- character classes ---- */

    private def isAsciiPunct(c: Char): Boolean =
      "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".indexOf(c) >= 0

    private def isPunct(c: Char): Boolean =
      isAsciiPunct(c) || (!c.isLetterOrDigit && !isWhitespace(c) && c >= '¡')

    private def isWhitespace(c: Char): Boolean =
      c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\u000B' || c == '\u000C'

    private def unescape(str: String): String =
      if str.indexOf('\\') < 0 then str
      else
        val sb = new StringBuilder
        var k = 0
        while k < str.length do
          val c = str.charAt(k)
          if c == '\\' && k + 1 < str.length && isAsciiPunct(str.charAt(k + 1)) then
            sb.append(str.charAt(k + 1)); k += 2
          else { sb.append(c); k += 1 }
        sb.toString

    private def isUriAutolink(inner: String): Boolean =
      val colon = inner.indexOf(':')
      colon > 0 && {
        val scheme = inner.substring(0, colon)
        scheme.head.isLetter && scheme.forall(c => c.isLetterOrDigit || c == '+' || c == '.' || c == '-') &&
          !inner.exists(c => isWhitespace(c) || c == '<')
      }

    private def isEmailAutolink(inner: String): Boolean =
      val at = inner.indexOf('@')
      at > 0 && at < inner.length - 1 && !inner.exists(isWhitespace) &&
        inner.substring(at + 1).contains('.')
