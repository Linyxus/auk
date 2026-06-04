package auk.config

/** The syntactic layer: turns config text into a [[RawConfig]] (a path → raw
  * value map), independent of any schema or value types.
  *
  * Grammar (one forward pass, 1-based line numbers):
  *   - blank lines and full-line `#` comments are ignored (a `#` inside a value
  *     is NOT a comment);
  *   - `[scope]` headers set an absolute dotted prefix for the keys that follow
  *     (headers do not nest by indentation);
  *   - `key = value` records an inline value; indentation of `key`/header lines
  *     is cosmetic and ignored;
  *   - `key =` with an empty RHS opens a *block*: the following more-indented
  *     bare lines (no `=`) each become one list element. An empty block (no
  *     continuation) is recorded as an empty inline value, so a scalar reads it
  *     as `""` and a list as the empty list.
  *
  * Errors (duplicate key, malformed header, orphan indented line, junk line,
  * missing key) are accumulated with line numbers; all are reported together.
  */
object ConfigParser:

  def parse(text: String): Either[List[ConfigError], RawConfig] =
    val lines = text.split("\n", -1)
    val entries = Vector.newBuilder[(Path, RawValue)]
    val errors = List.newBuilder[ConfigError]
    var seen = Set.empty[Path]
    var scope: Path = Nil

    // A `key =` awaiting block-continuation lines.
    var pendingPath: Path | Null = null
    var pendingLine = 0
    var pendingIndent = 0
    val block = List.newBuilder[String]

    def emit(p: Path, v: RawValue, line: Int): Unit =
      if seen.contains(p) then
        errors += ConfigError(s"duplicate key '${p.dotted}'", line = Some(line))
      else
        entries += (p -> v)
        seen += p

    def flush(): Unit =
      val pp = pendingPath
      if pp != null then
        val items = block.result()
        block.clear()
        val value =
          if items.isEmpty then RawValue.Inline("", pendingLine)
          else RawValue.Block(items, pendingLine)
        emit(pp, value, pendingLine)
        pendingPath = null

    var idx = 0
    while idx < lines.length do
      val rawLine = lines(idx)
      val lineNo = idx + 1
      var wsLen = 0
      while wsLen < rawLine.length &&
        (rawLine.charAt(wsLen) == ' ' || rawLine.charAt(wsLen) == '\t')
      do wsLen += 1
      val indent = wsLen
      val trimmed = rawLine.substring(wsLen).trim

      if trimmed.isEmpty then () // blank line: transparent (does not end a block)
      else if trimmed.startsWith("#") then () // full-line comment
      else if trimmed.startsWith("[") then
        flush()
        if !trimmed.endsWith("]") then
          errors += ConfigError("malformed scope header", line = Some(lineNo))
        else
          val inner = trimmed.substring(1, trimmed.length - 1).trim
          if !validKey(inner) then
            errors += ConfigError("malformed scope header", line = Some(lineNo))
          else scope = splitKey(inner)
      else if trimmed.indexOf('=') >= 0 then
        flush()
        val eq = trimmed.indexOf('=')
        val key = trimmed.substring(0, eq).trim
        val rhs = trimmed.substring(eq + 1).trim
        if key.isEmpty then
          errors += ConfigError("missing key before '='", line = Some(lineNo))
        else if !validKey(key) then
          errors += ConfigError(s"invalid key '$key'", line = Some(lineNo))
        else
          val full = scope ++ splitKey(key)
          if rhs.nonEmpty then emit(full, RawValue.Inline(rhs, lineNo), lineNo)
          else
            pendingPath = full
            pendingLine = lineNo
            pendingIndent = indent
            block.clear()
      else
        // bare line (no '='): a block element if it is more indented than its key
        if pendingPath != null && indent > pendingIndent then block += trimmed
        else
          flush()
          if indent > 0 then
            errors += ConfigError("indented line does not belong to any key", line = Some(lineNo))
          else
            errors += ConfigError("expected 'key = value' or '[scope]'", line = Some(lineNo))
      idx += 1

    flush()

    val errs = errors.result()
    if errs.nonEmpty then Left(errs) else Right(RawConfig(entries.result()))

  /** Split a dotted key into its segments, keeping empty segments so malformed
    * keys (`a..b`, `a.`) fail [[validKey]].
    */
  def splitKey(s: String): List[String] =
    val out = List.newBuilder[String]
    val sb = new StringBuilder
    s.foreach { c =>
      if c == '.' then
        out += sb.toString
        sb.clear()
      else sb.append(c)
    }
    out += sb.toString
    out.result()

  /** A dotted key is valid iff every segment is a non-empty run of letters,
    * digits, `_` or `-`.
    */
  def validKey(s: String): Boolean =
    val segs = splitKey(s)
    segs.nonEmpty && segs.forall { seg =>
      seg.nonEmpty && seg.forall(c => c.isLetterOrDigit || c == '_' || c == '-')
    }
