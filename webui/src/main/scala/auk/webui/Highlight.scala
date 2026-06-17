package auk.webui

/** Syntax-highlight token kinds, mirroring the reference page's `.hl-*` classes
  * (keyword / soft-keyword / type / string / number / definition / comment /
  * operator / punctuation). `Plain` is unstyled. */
enum HlKind:
  case Plain, Keyword, Soft, Type, Str, Num, Def, Comment, Op, Punct

/** A run of source text tagged with how to colour it. The concatenation of all
  * tokens' `text` always equals the original input (no characters dropped). */
final case class HlToken(kind: HlKind, text: String)

/** A small, dependency-free Scala highlighter producing a flat token stream that
  * the Laminar binding maps to `<span class="hl-…">`. Pure (hence unit-tested) and
  * pragmatic — a lexer, not a parser — which is plenty for short eval_scala
  * snippets. */
object Highlight:
  private val keywords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false",
    "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new", "null",
    "object", "override", "package", "private", "protected", "return", "sealed", "super", "then",
    "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield"
  )
  private val soft = Set("inline", "opaque", "open", "transparent", "using", "derives", "end", "extension", "infix")
  private val defKeywords = Set("def", "val", "var", "class", "object", "trait", "enum", "type", "given")

  private val opChars = "+-*/%=!<>&|^~:?@#".toSet
  private val punctChars = "()[]{},;.".toSet

  def scala(code: String): Vector[HlToken] =
    val out = Vector.newBuilder[HlToken]
    val n = code.length
    var i = 0
    var prevWord = "" // last identifier/keyword (for def-name detection); cleared by non-word, non-space tokens
    while i < n do
      val c = code.charAt(i)
      if c == '/' && i + 1 < n && code.charAt(i + 1) == '/' then
        var j = i + 2
        while j < n && code.charAt(j) != '\n' do j += 1
        out += HlToken(HlKind.Comment, code.substring(i, j)); i = j; prevWord = ""
      else if c == '/' && i + 1 < n && code.charAt(i + 1) == '*' then
        var j = i + 2
        while j + 1 < n && !(code.charAt(j) == '*' && code.charAt(j + 1) == '/') do j += 1
        val end = if j + 1 < n then j + 2 else n
        out += HlToken(HlKind.Comment, code.substring(i, end)); i = end; prevWord = ""
      else if c == '"' then
        val end =
          if i + 2 < n && code.charAt(i + 1) == '"' && code.charAt(i + 2) == '"' then
            var j = i + 3
            while j + 2 < n && !(code.charAt(j) == '"' && code.charAt(j + 1) == '"' && code.charAt(j + 2) == '"') do j += 1
            if j + 2 < n then j + 3 else n
          else
            var j = i + 1
            while j < n && code.charAt(j) != '"' do (if code.charAt(j) == '\\' then j += 2 else j += 1)
            math.min(n, j + 1)
        out += HlToken(HlKind.Str, code.substring(i, end)); i = end; prevWord = ""
      else if c == '\'' then
        var j = i + 1
        while j < n && code.charAt(j) != '\'' do (if code.charAt(j) == '\\' then j += 2 else j += 1)
        val end = math.min(n, j + 1)
        out += HlToken(HlKind.Str, code.substring(i, end)); i = end; prevWord = ""
      else if c.isDigit then
        var j = i + 1
        while j < n && (code.charAt(j).isLetterOrDigit || code.charAt(j) == '.' || code.charAt(j) == '_') do j += 1
        out += HlToken(HlKind.Num, code.substring(i, j)); i = j; prevWord = ""
      else if c.isLetter || c == '_' then
        var j = i + 1
        while j < n && (code.charAt(j).isLetterOrDigit || code.charAt(j) == '_') do j += 1
        val word = code.substring(i, j)
        val kind =
          if keywords.contains(word) then HlKind.Keyword
          else if soft.contains(word) then HlKind.Soft
          else if defKeywords.contains(prevWord) then HlKind.Def
          else if word.charAt(0).isUpper then HlKind.Type
          else HlKind.Plain
        out += HlToken(kind, word); i = j; prevWord = word
      else if punctChars.contains(c) then
        out += HlToken(HlKind.Punct, c.toString); i += 1; prevWord = ""
      else if opChars.contains(c) then
        var j = i + 1
        while j < n && opChars.contains(code.charAt(j)) do j += 1
        out += HlToken(HlKind.Op, code.substring(i, j)); i = j; prevWord = ""
      else if c.isWhitespace then
        var j = i + 1
        while j < n && code.charAt(j).isWhitespace do j += 1
        out += HlToken(HlKind.Plain, code.substring(i, j)); i = j // whitespace keeps prevWord (def<space>name)
      else
        out += HlToken(HlKind.Plain, c.toString); i += 1; prevWord = ""
    out.result()
