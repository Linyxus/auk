package auk.utils

/** Unicode hygiene for strings that cross the LLM-API boundary.
  *
  * A lone UTF-16 surrogate — a high surrogate (U+D800–U+DBFF) not followed by a
  * low one, or a stray low surrogate (U+DC00–U+DFFF) — is not a valid Unicode
  * scalar value: it has no UTF-8 encoding, and JSON encoders emit it as a
  * `\uXXXX` escape that strict API parsers reject ("illegal char sequence of
  * surrogate pair"). Because the rejected string lives in the message history,
  * the failure is sticky: every later request re-sends it and 400s, wedging the
  * whole conversation.
  *
  * Such characters reach us from in-house sources whose strings are raw UTF-16 —
  * most notably `eval_scala` output (the REPL runs in a UTF-16 JS string) and
  * pasted user input. Sanitizing those at their entry points keeps the
  * conversation sendable. Model-authored content arrives already-valid from the
  * API, so it needs no scrubbing.
  */
object Unicode:
  /** The character a lone surrogate is replaced with (U+FFFD REPLACEMENT
    * CHARACTER) — the same substitution every UTF-8 encoder makes. */
  val Replacement: Char = '\uFFFD'

  private inline def isHigh(c: Char): Boolean = c >= '\uD800' && c <= '\uDBFF'
  private inline def isLow(c: Char): Boolean = c >= '\uDC00' && c <= '\uDFFF'

  /** Replace every unpaired surrogate in `s` with [[Replacement]], leaving valid
    * surrogate pairs (astral characters like emoji) and all other text untouched.
    * Pure and total; returns `s` itself when there is nothing to fix (the common
    * case), so the no-surrogate path allocates nothing. */
  def replaceLoneSurrogates(s: String): String =
    if !containsSurrogate(s) then s
    else
      val n = s.length
      val sb = new StringBuilder(n)
      var i = 0
      while i < n do
        val c = s.charAt(i)
        if isHigh(c) then
          if i + 1 < n && isLow(s.charAt(i + 1)) then
            sb.append(c).append(s.charAt(i + 1)) // a well-formed pair: keep both
            i += 2
          else
            sb.append(Replacement) // high surrogate with no following low
            i += 1
        else if isLow(c) then
          sb.append(Replacement) // low surrogate with no preceding high
          i += 1
        else
          sb.append(c)
          i += 1
      sb.toString

  private def containsSurrogate(s: String): Boolean =
    var i = 0
    val n = s.length
    while i < n do
      val c = s.charAt(i)
      if isHigh(c) || isLow(c) then return true
      i += 1
    false
