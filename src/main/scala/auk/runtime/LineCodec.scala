package auk.runtime

/** The shared line model used by [[Read]] and [[Edit]].
  *
  * A file is treated as a sequence of 0-based lines. [[Read]] renders each line
  * as `<n>@ <content>`, where `n` is the line number; [[Edit]] addresses lines
  * by that same number, taking an inclusive `[startLine, endLine]` range to
  * replace. The two tools therefore agree on what a "line" is and how lines are
  * numbered, without the model ever having to quote content back.
  *
  * Splitting is trailing-newline aware so that line numbers match what an editor
  * would show: `"a\nb\n"` is two lines (`a`, `b`), not three. Whether the file
  * ended in a newline is tracked separately and restored on write.
  */
private[runtime] object LineCodec:

  /** Split file text into lines, ignoring a single trailing newline so the
    * count matches an editor's view. The empty string is zero lines.
    */
  def split(content: String): Vector[String] =
    if content.isEmpty then Vector.empty
    else
      val body = if content.endsWith("\n") then content.dropRight(1) else content
      body.split("\n", -1).toVector

  /** Whether `content` ends in a newline (so a rewrite can preserve it). */
  def endsWithNewline(content: String): Boolean = content.endsWith("\n")

  /** Reassemble lines into file text, re-adding a trailing newline when the
    * original had one and there is at least one line to terminate.
    */
  def join(lines: Vector[String], trailingNewline: Boolean): String =
    val base = lines.mkString("\n")
    if trailingNewline && lines.nonEmpty then base + "\n" else base

  /** Render one source line in the `<n>@ <content>` display format. */
  def render(lineNumber: Int, content: String): String =
    s"$lineNumber@ $content"
