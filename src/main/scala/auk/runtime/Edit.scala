package auk.runtime

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}

/** Arguments for the [[Edit]] tool. */
case class EditParams(
    @desc("Path to the file to edit, absolute or relative to the working directory")
    path: String,
    @desc(
      "The lines to replace, each carrying the `@<n>> ` prefix exactly as the " +
        "`read` tool prints them. Line numbers must be consecutive. A line's " +
        "content must match the file, or be `...` to match whatever is there. " +
        "Example: `@11> ...` replaces line 11 whatever it holds."
    )
    old: String,
    @desc(
      "The replacement text, with NO `@<n>> ` prefixes — just the raw lines to " +
        "put in place of `old`. Empty deletes the matched lines."
    )
    `new`: String
) derives ToolInput

/** Replace a consecutive run of lines, addressed by line number.
  *
  * Unlike a string-match edit, `old` quotes the lines to replace using the same
  * `@<n>> <content>` prefixes the [[Read]] tool prints. The line numbers pin the
  * edit to one place (no ambiguity, no `replaceAll`); each line's content is
  * verified against the file unless it is the `...` wildcard. `new` is the raw
  * replacement text without prefixes.
  *
  * Side-effecting, so it consults [[RuntimeContext.approvals]] before writing —
  * but only after the edit is known to apply cleanly.
  */
object Edit extends Tool:
  type Params = EditParams

  val name = "edit"

  val description =
    "Edit a file by replacing a consecutive run of lines. `old` quotes the " +
      "lines to replace using the `@<n>> ` prefixes from the `read` tool " +
      "(content may be `...` to match any line); `new` is the raw replacement " +
      "text without prefixes."

  val input: ToolInput[EditParams] = ToolInput[EditParams]

  def execute(params: EditParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    parseSpecs(params.old) match
      case Left(err) => ToolResult.error(err)
      case Right(specs) =>
        val path = ctx.resolve(params.path)
        if !Files.exists(path) then ToolResult.error(s"file not found: ${params.path}")
        else if Files.isDirectory(path) then
          ToolResult.error(s"path is a directory, not a file: ${params.path}")
        else
          val content =
            try Files.readString(path, UTF_8).nn
            catch case e: IOException => return ToolResult.error(s"failed to read file: ${e.getMessage}")
          val lines = LineCodec.split(content)
          locate(specs, lines) match
            case Left(err) => ToolResult.error(err)
            case Right((start, count)) =>
              applyEdit(params, path, content, lines, start, count)

  /** Split `old` into [[LineCodec.Spec]]s, tolerating one trailing newline. */
  private def parseSpecs(old: String): Either[String, Vector[LineCodec.Spec]] =
    val raw = old.split("\n", -1).toVector
    val trimmed = if raw.nonEmpty && raw.last.isEmpty then raw.dropRight(1) else raw
    if trimmed.isEmpty then Left("old is empty: provide at least one `@<n>> ` line")
    else
      val specs = Vector.newBuilder[LineCodec.Spec]
      var error: Option[String] = None
      val it = trimmed.iterator
      while it.hasNext && error.isEmpty do
        LineCodec.parseSpec(it.next()) match
          case Right(s) => specs += s
          case Left(e)  => error = Some(e)
      error.toLeft(specs.result())

  /** Validate the specs against the file and return the `(start, count)` window
    * they address, or an error explaining the mismatch.
    */
  private def locate(
      specs: Vector[LineCodec.Spec],
      lines: Vector[String]
  ): Either[String, (Int, Int)] =
    val start = specs.head.lineNumber
    val nonConsecutive = specs.zipWithIndex.collectFirst {
      case (s, i) if s.lineNumber != start + i =>
        s"old line numbers must be consecutive: expected @${start + i} but found @${s.lineNumber}"
    }
    nonConsecutive match
      case Some(e) => Left(e)
      case None =>
        val count = specs.length
        val end = start + count - 1
        if end >= lines.length then
          Left(
            s"@$end is out of range: the file has ${lines.length} line(s) " +
              s"(@0..@${lines.length - 1})"
          )
        else
          val mismatch = specs.collectFirst {
            case s if s.content.exists(_ != lines(s.lineNumber)) =>
              val expected = s.content.get
              s"content mismatch at @${s.lineNumber}: old says " +
                s"${quote(expected)} but the file has ${quote(lines(s.lineNumber))}"
          }
          mismatch.toLeft((start, count))

  private def applyEdit(
      params: EditParams,
      path: Path,
      content: String,
      lines: Vector[String],
      start: Int,
      count: Int
  )(using ctx: RuntimeContext, async: Async): ToolResult =
    if !ctx.approvals.request(ApprovalRequest(name, s"edit ${params.path}")) then
      ToolResult.error(s"edit not approved: ${params.path}")
    else
      val replacement =
        if params.`new`.isEmpty then Vector.empty
        else params.`new`.split("\n", -1).toVector
      val updated = lines.take(start) ++ replacement ++ lines.drop(start + count)
      val rewritten = LineCodec.join(updated, LineCodec.endsWithNewline(content))
      try Files.writeString(path, rewritten, UTF_8)
      catch case e: IOException => return ToolResult.error(s"failed to write file: ${e.getMessage}")

      ToolResult.ok(
        s"Edited ${params.path}: replaced @$start..@${start + count - 1} " +
          s"($count line(s)) with ${replacement.length} line(s).",
        metadata = Map(
          "startLine" -> start.toString,
          "oldLineCount" -> count.toString,
          "newLineCount" -> replacement.length.toString
        )
      )

  private def quote(s: String): String = "\"" + s + "\""
