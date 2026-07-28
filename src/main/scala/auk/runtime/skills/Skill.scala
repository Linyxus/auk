package auk.runtime.skills

/** One stored skill: a durable, tested Scala definition preloaded into the
  * agent's REPL session.
  *
  * `code` is the skill's full source — optional leading `import` lines followed
  * by exactly one top-level `object` named `id` — and `tests` are standalone
  * snippets (statements ending in `assert(...)`) run against a candidate skill
  * set before it may replace the live one. All skills together form ONE
  * compilation unit (see [[SkillBlob]]): they may reference each other freely,
  * in any order, and the compiler resolves the dependencies.
  */
final case class Skill(id: String, description: String, code: String, tests: List[String])

object Skill:
  private val IdPattern = "^[A-Za-z][A-Za-z0-9_]*$".r

  /** A skill id must be a plain Scala identifier: it names the skill's `object`
    * and its directory under `.auk/skills/`. */
  def validId(id: String): Boolean = IdPattern.matches(id)

/** Shape validation and interface extraction for a skill's source.
  *
  * A skill's *interface* is its `object` with every implementation redacted —
  * public member signatures only — rendered into the system prompt so the model
  * knows what it can call without paying for the bodies. Extraction is a
  * pragmatic scanner, not a parser: it relies on the forced shape (`skill_save`
  * rejects code it cannot extract from), tracks strings/comments so braces
  * inside them don't confuse it, and requires explicit result types on public
  * members so a signature always ends at its `=`.
  */
object SkillCode:

  private val Modifiers =
    Set("final", "override", "lazy", "implicit", "inline", "transparent", "infix", "opaque")
  private val MemberKeywords =
    Set("def", "val", "var", "given", "type", "object", "class", "trait", "enum", "case", "extension")
  private val ChunkStarters = Modifiers ++ MemberKeywords + "private" + "protected" + "end"

  /** Validate `code`'s shape and extract the redacted interface, rendered as a
    * colon-style object with public signatures only. */
  def interface(id: String, code: String): Either[String, String] =
    if !Skill.validId(id) then Left(s"'$id' is not a valid skill id (a plain Scala identifier)")
    else
      val lines = splitLines(code)
      findOpener(id, lines).flatMap: (openerIdx, braceMode) =>
        bodyLines(id, lines, openerIdx, braceMode).flatMap: body =>
          val nonBlank = body.filter(_.trim.nonEmpty)
          if nonBlank.isEmpty then Right(render(id, Nil))
          else
            val indent = indentOf(nonBlank.head)
            members(body, indent).map(ms => render(id, ms))

  // -- structure ---------------------------------------------------------------

  private def splitLines(code: String): Vector[String] =
    // `linesIterator` drops a trailing empty line, which is fine here.
    code.linesIterator.toVector

  private def indentOf(line: String): Int =
    line.takeWhile(c => c == ' ' || c == '\t').length

  private def firstWord(line: String): String =
    line.trim.takeWhile(c => c.isLetterOrDigit || c == '_' || c == '@')

  /** The single column-0 `object <id>` line, checking the prefix is only
    * imports/comments/blanks and no other column-0 definition exists. Returns
    * the opener's index and whether it opens a brace body. */
  private def findOpener(id: String, lines: Vector[String]): Either[String, (Int, Boolean)] =
    val openerRe = ("""^object\s+""" + id + """\b.*$""").r
    val anyObjectRe = """^object\s+(\w+)""".r
    lines.zipWithIndex.find((l, _) => openerRe.matches(l)) match
      case None =>
        anyObjectRe.findFirstMatchIn(lines.find(l => anyObjectRe.findFirstIn(l).isDefined).getOrElse("")) match
          case Some(m) =>
            Left(s"the top-level object must be named '$id' to match the skill id (found 'object ${m.group(1)}')")
          case None =>
            Left(s"the code must contain a top-level `object $id` at column 0")
      case Some((opener, idx)) =>
        val before = lines.take(idx)
        val badPrefix = before.zip(commentOnlyLines(before)).find: (l, isComment) =>
          val w = firstWord(l)
          l.trim.nonEmpty && !isComment && w != "import"
        badPrefix match
          case Some((l, _)) =>
            Left(s"only `import` lines and comments may precede `object $id` (found: ${l.trim})")
          case None =>
            val otherDef = lines.zipWithIndex.drop(idx + 1).find: (l, _) =>
              indentOf(l) == 0 && l.trim.nonEmpty &&
                MemberKeywords.contains(firstWord(l)) && !l.trim.startsWith("end ")
            otherDef match
              case Some((l, _)) =>
                Left(s"a skill holds exactly one top-level object; move `${l.trim.take(40)}` inside `object $id`")
              case None =>
                headerMode(opener).map(brace => (idx, brace))

  /** Whether the opener line ends a brace body (`{`) or a colon body (`:`). */
  private def headerMode(opener: String): Either[String, Boolean] =
    val active = withMask(opener)
    val lastActive = active.reverseIterator.find((isActive, c) => isActive && !c.isWhitespace)
    lastActive.map(_._2) match
      case Some('{') => Right(true)
      case Some(':') => Right(false)
      case _ =>
        Left(
          "the object header must end with `{` or `:` on its own line " +
            "(a one-line `object X { ... }` body cannot be redacted)"
        )

  /** The body's lines: after the opener, minus the closing `}` (brace mode) or a
    * trailing `end <id>` marker (colon mode). */
  private def bodyLines(
      id: String,
      lines: Vector[String],
      openerIdx: Int,
      braceMode: Boolean
  ): Either[String, Vector[String]] =
    val rest = lines.drop(openerIdx + 1)
    if braceMode then
      rest.lastIndexWhere(l => indentOf(l) == 0 && l.trim == "}") match
        case -1 => Left(s"could not find the closing `}` of `object $id` at column 0")
        case i  => Right(rest.take(i))
    else
      rest.lastIndexWhere(_.trim == s"end $id") match
        case -1 => Right(rest)
        case i  => Right(rest.take(i))

  // -- members -----------------------------------------------------------------

  /** Split the body into member chunks (a member plus the comment lines directly
    * above it) and redact each. Comment-only lines never start a member, however
    * they are indented: a Scaladoc's ` * ...` continuations sit deeper than the
    * member indent, and attach to the chunk (or pending comments) they fall in. */
  private def members(body: Vector[String], indent: Int): Either[String, List[String]] =
    val chunks = collection.mutable.ListBuffer.empty[Vector[String]]
    var pendingComments = Vector.empty[String]
    var currentChunk = Vector.empty[String]
    var error: Option[String] = None
    def flush(): Unit =
      if currentChunk.nonEmpty then
        chunks += currentChunk
        currentChunk = Vector.empty
    val commentOnly = commentOnlyLines(body)
    var li = 0
    while li < body.length && error.isEmpty do
      val line = body(li)
      val t = line.trim
      val atIndent = indentOf(line) == indent && t.nonEmpty
      if atIndent && commentOnly(li) then
        // A comment at member indent introduces the NEXT member.
        flush()
        pendingComments = pendingComments :+ line
      else if commentOnly(li) then
        // A comment continuation deeper than the member indent attaches to
        // whatever is open: the current member, else the pending comments.
        if currentChunk.nonEmpty then currentChunk = currentChunk :+ line
        else pendingComments = pendingComments :+ line
      else if atIndent && (ChunkStarters.contains(firstWord(line)) || t.startsWith("@")) then
        flush()
        currentChunk = pendingComments :+ line
        pendingComments = Vector.empty
      else if t.isEmpty && currentChunk.isEmpty then
        pendingComments = Vector.empty // a blank line detaches floating comments
      else if currentChunk.nonEmpty then currentChunk = currentChunk :+ line
      else if t.nonEmpty then
        error = Some(s"unrecognised member at the object's top level: ${t.take(60)}")
      li += 1
    flush()

    error match
      case Some(err) => Left(err)
      case None =>
        val rendered = collection.mutable.ListBuffer.empty[String]
        var failure: Option[String] = None
        val cit = chunks.iterator
        while cit.hasNext && failure.isEmpty do
          redactMember(cit.next(), indent) match
            case Left(err)        => failure = Some(err)
            case Right(Some(sig)) => rendered += sig
            case Right(None)      => () // private/protected: skip
        failure.toLeft(rendered.toList)

  /** Redact one member chunk to its signature (None for a non-public member). */
  private def redactMember(chunk: Vector[String], indent: Int): Either[String, Option[String]] =
    firstCodeLine(chunk) match
      case None => Right(None) // only comments/annotations; nothing to keep
      case Some(line) =>
        var words = line.trim.split("\\s+").nn.toList.map(_.nn)
        while words.nonEmpty && Modifiers.contains(words.head) do words = words.tail
        words match
          case kw :: _ if kw.startsWith("private") || kw.startsWith("protected") =>
            Right(None)
          case "type" :: _ | "enum" :: _ =>
            // Aliases and enum cases ARE interface: keep the chunk whole.
            Right(Some(dedent(chunk, indent)))
          case kw :: _ if kw == "object" || kw == "class" || kw == "trait" || kw == "case" =>
            headerOnly(chunk, indent)
          case kw :: _ if kw == "def" || kw == "val" || kw == "var" =>
            signatureOf(chunk, indent, requireResultType = true)
          case kw :: _ if kw == "given" || kw == "extension" =>
            signatureOf(chunk, indent, requireResultType = false)
          case _ =>
            Left(s"unsupported member shape: ${line.trim.take(60)}")

  /** Cut a def/val-style member at the `=` that starts its body. */
  private def signatureOf(
      chunk: Vector[String],
      indent: Int,
      requireResultType: Boolean
  ): Either[String, Option[String]] =
    val text = chunk.mkString("\n")
    val cut = cutAtBodyEq(text)
    if requireResultType && !hasTopLevelColon(cut) then
      Left(
        s"public member `${firstDefLine(chunk).take(60)}` needs an explicit result type " +
          "(so its signature can be redacted into the interface)"
      )
    else Right(Some(dedent(splitLines(cut.stripTrailing.nn), indent)))

  /** The chunk's first line of actual code: past the leading comment and
    * annotation lines. Comment detection is mask-based, so the continuation
    * lines of a multi-line comment count as comments wherever they sit. */
  private def firstCodeLine(chunk: Vector[String]): Option[String] =
    chunk
      .zip(commentOnlyLines(chunk))
      .find: (l, isComment) =>
        val t = l.trim
        t.nonEmpty && !isComment && !t.startsWith("@")
      .map((l, _) => l)

  private def firstDefLine(chunk: Vector[String]): String =
    firstCodeLine(chunk).fold("")(_.trim)

  /** For a nested object/class/trait: keep the header, elide the body. */
  private def headerOnly(chunk: Vector[String], indent: Int): Either[String, Option[String]] =
    val text = chunk.mkString("\n")
    val active = withMask(text)
    var depth = 0
    var cutAt = -1
    var i = 0
    while i < active.length && cutAt < 0 do
      val (isActive, c) = active(i)
      if isActive then
        c match
          case '(' | '[' => depth += 1
          case ')' | ']' => depth -= 1
          case '{' if depth == 0 => cutAt = i
          case _ => ()
      i += 1
    val header =
      if cutAt >= 0 then text.take(cutAt).stripTrailing.nn + " { … }"
      else
        // Colon-style nested body: keep up to the `:` line, elide the rest.
        val t = text.stripTrailing.nn
        if t.endsWith(":") then t + " …" else t
    // A colon-body's indented lines are still in `header` when there was no
    // brace; keep only the lines up to and including the header line.
    val headerLines = splitLines(header)
    val keep = headerLines.indexWhere(l => l.stripTrailing.nn.endsWith("{ … }") || l.stripTrailing.nn.endsWith(": …")) match
      case -1 => headerLines
      case i  => headerLines.take(i + 1)
    Right(Some(dedent(keep, indent)))

  // -- the `=` cut -------------------------------------------------------------

  /** Cut `text` just before the first active `=` at bracket depth 0 that is a
    * plain assignment (not `=>`, `==`, `<=`, an operator, …). Returns the whole
    * text when no such `=` exists (an abstract member). */
  private[skills] def cutAtBodyEq(text: String): String =
    val active = withMask(text)
    var depth = 0
    var i = 0
    while i < active.length do
      val (isActive, c) = active(i)
      if isActive then
        c match
          case '(' | '[' | '{' => depth += 1
          case ')' | ']' | '}' => depth -= 1
          case '=' if depth == 0 =>
            val next = if i + 1 < text.length then text.charAt(i + 1) else ' '
            val prev = text.take(i).reverseIterator.find(!_.isWhitespace).getOrElse(' ')
            val operatorish = "=<>!+-*/%&|^:~@?#\\"
            if next != '>' && next != '=' && !operatorish.contains(prev) then
              return text.take(i).stripTrailing.nn
          case _ => ()
      i += 1
    text

  /** Whether the (already cut) signature carries a `:` at bracket depth 0 —
    * i.e. an explicit result type. */
  private[skills] def hasTopLevelColon(sig: String): Boolean =
    val active = withMask(sig)
    var depth = 0
    var i = 0
    while i < active.length do
      val (isActive, c) = active(i)
      if isActive then
        c match
          case '(' | '[' | '{' => depth += 1
          case ')' | ']' | '}' => depth -= 1
          case ':' if depth == 0 => return true
          case _ => ()
      i += 1
    false

  // -- rendering ---------------------------------------------------------------

  private def dedent(chunk: Vector[String], indent: Int): String =
    chunk
      .map: l =>
        if l.trim.isEmpty then ""
        else if indentOf(l) >= indent then l.drop(indent)
        else l.trim
      .mkString("\n")

  private def render(id: String, members: List[String]): String =
    val body =
      if members.isEmpty then List("  // no public members")
      else members.map(m => m.linesIterator.map(l => if l.isEmpty then l else "  " + l).mkString("\n"))
    (s"object $id:" :: body).mkString("\n")

  // -- string/comment awareness ------------------------------------------------

  /** What role each source char plays: active code, comment text, or string /
    * char literal content. */
  private[skills] enum CharClass:
    case Active, Comment, Literal

  /** Each char of `code` paired with its [[CharClass]]. Handles line comments,
    * nested block comments, simple and triple-quoted strings, and char
    * literals; an interpolator's splices count as string (safe for depth
    * counting). */
  private[skills] def classify(code: String): Array[(CharClass, Char)] =
    val n = code.length
    val out = new Array[(CharClass, Char)](n)
    var i = 0
    def at(j: Int): Char = if j >= 0 && j < n then code.charAt(j) else ' '
    def mark(from: Int, until: Int, cc: CharClass): Unit =
      var k = from
      while k < until && k < n do
        out(k) = (cc, code.charAt(k))
        k += 1
    while i < n do
      val c = code.charAt(i)
      if c == '/' && at(i + 1) == '/' then
        var j = i
        while j < n && code.charAt(j) != '\n' do j += 1
        mark(i, j, CharClass.Comment)
        i = j
      else if c == '/' && at(i + 1) == '*' then
        var depth = 0
        var j = i
        var end = -1
        while j < n && end < 0 do
          if code.charAt(j) == '/' && at(j + 1) == '*' then { depth += 1; j += 2 }
          else if code.charAt(j) == '*' && at(j + 1) == '/' then
            depth -= 1; j += 2
            if depth == 0 then end = j
          else j += 1
        val stop = if end < 0 then n else end
        mark(i, stop, CharClass.Comment)
        i = stop
      else if c == '"' && at(i + 1) == '"' && at(i + 2) == '"' then
        var j = i + 3
        var end = -1
        while j < n && end < 0 do
          if code.charAt(j) == '"' && at(j + 1) == '"' && at(j + 2) == '"' then
            var k = j + 3
            while k < n && code.charAt(k) == '"' do k += 1
            end = k
          else j += 1
        val stop = if end < 0 then n else end
        mark(i, stop, CharClass.Literal)
        i = stop
      else if c == '"' then
        var j = i + 1
        var end = -1
        while j < n && end < 0 do
          val cj = code.charAt(j)
          if cj == '\\' then j += 2
          else if cj == '"' then end = j + 1
          else if cj == '\n' then end = j // unterminated: stop at the newline
          else j += 1
        val stop = if end < 0 then n else end
        mark(i, stop, CharClass.Literal)
        i = stop
      else if c == '\'' && (at(i + 1) == '\\' || (at(i + 2) == '\'' && at(i + 1) != '\'')) then
        if at(i + 1) == '\\' then
          var j = i + 2
          while j < n && code.charAt(j) != '\'' do j += 1
          val stop = math.min(j + 1, n)
          mark(i, stop, CharClass.Literal)
          i = stop
        else
          mark(i, i + 3, CharClass.Literal)
          i += 3
      else
        out(i) = (CharClass.Active, c)
        i += 1
    out

  /** Each char of `code` paired with whether it is *active* — outside string
    * literals and comments (see [[classify]]). */
  private[skills] def withMask(code: String): Array[(Boolean, Char)] =
    classify(code).map((cc, c) => (cc == CharClass.Active, c))

  /** Per line: does it carry comment text but no active code? (A `//` line, or
    * a line wholly inside a block comment — e.g. the ` * ...` continuations of
    * a Scaladoc.) Lines inside string/char literals are NOT comment-only:
    * they continue the member they belong to, however they are indented. */
  private def commentOnlyLines(lines: Vector[String]): Vector[Boolean] =
    val classified = classify(lines.mkString("\n"))
    val out = Vector.newBuilder[Boolean]
    var pos = 0
    for line <- lines do
      var code = false
      var comment = false
      var j = 0
      while j < line.length do
        val (cc, c) = classified(pos + j)
        if !c.isWhitespace then
          if cc == CharClass.Active then code = true
          else if cc == CharClass.Comment then comment = true
        j += 1
      out += (!code && comment)
      pos += line.length + 1 // the '\n' joining the lines
    out.result()
