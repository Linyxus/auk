package auk.runtime

/** The shared line-addressing format used by [[Read]] and [[Edit]].
  *
  * A file is treated as a sequence of 0-based lines. [[Read]] renders each line
  * as `@<n>> <content>`; [[Edit]] requires its `old` argument to use the very
  * same prefixes, so the two tools speak one language: the model reads numbered
  * lines and edits by quoting them back.
  *
  * Splitting is trailing-newline aware so that line numbers match what an editor
  * would show: `"a\nb\n"` is two lines (`a`, `b`), not three. Whether the file
  * ended in a newline is tracked separately and restored on write.
  */
private[runtime] object LineCodec:

  /** The literal that, in an `old` line, matches whatever content is there. */
  val Wildcard = "..."

  /** A parsed `old` line: which file line it addresses and, unless it is a
    * [[Wildcard]], the exact content it claims that line holds.
    */
  final case class Spec(lineNumber: Int, content: Option[String])

  private val Prefixed = """@(\d+)>(.*)""".r

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

  /** Render one source line in the `@<n>> <content>` display format. */
  def render(lineNumber: Int, content: String): String =
    s"@$lineNumber> $content"

  /** Parse one `old` line back into a [[Spec]], or describe why it cannot.
    *
    * The single space after `>` is the delimiter and is dropped; any further
    * leading spaces belong to the content (so indentation survives). A bare
    * `@<n>>` with no content addresses an empty line.
    */
  def parseSpec(line: String): Either[String, Spec] =
    line match
      case Prefixed(numRaw, restRaw) =>
        val num = numRaw.nn
        val rest = restRaw.nn
        num.toIntOption match
          case None => Left(s"line number '$num' is not a valid integer: $line")
          case Some(n) =>
            val content = if rest.startsWith(" ") then rest.substring(1).nn else rest
            Right(Spec(n, if content == Wildcard then None else Some(content)))
      case _ =>
        Left(s"line is missing the required '@<n>>' prefix: $line")
