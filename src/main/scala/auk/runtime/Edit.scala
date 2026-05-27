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
      "0-based line number of the first line to replace, as shown by the `read` tool."
    )
    startLine: Int,
    @desc(
      "0-based line number of the last line to replace, inclusive. Set equal to " +
        "`startLine` to replace a single line."
    )
    endLine: Int,
    @desc(
      "The replacement text for the line range, as raw lines (it may span several " +
        "lines). Empty deletes the range."
    )
    content: String
) derives ToolInput

/** Replace an inclusive range of lines, addressed by line number.
  *
  * The `[startLine, endLine]` window uses the same 0-based line numbers the
  * [[Read]] tool prints, so editing is "read the lines, then name the range":
  * no need to quote the original content back. `content` is the raw replacement
  * text (empty deletes the range).
  *
  * It only edits an existing, non-empty file; an empty or missing file is an
  * error that points at the [[Write]] tool, which creates files.
  *
  * Side-effecting, so it consults [[RuntimeContext.approvals]] before writing —
  * but only after the edit is known to apply cleanly.
  */
object Edit extends Tool:
  type Params = EditParams

  val name = "edit"

  val description =
    "Edit a file by replacing an inclusive range of lines `[startLine, endLine]`, " +
      "addressed by the 0-based line numbers the `read` tool prints. `content` is " +
      "the raw replacement text (empty deletes the range). To create a new file, " +
      "or to fill an empty one, use the `write` tool instead."

  val input: ToolInput[EditParams] = ToolInput[EditParams]

  def execute(params: EditParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val path = ctx.resolve(params.path)
    if params.startLine < 0 then
      ToolResult.error(s"startLine must be >= 0, got ${params.startLine}")
    else if params.endLine < params.startLine then
      ToolResult.error(
        s"endLine (${params.endLine}) must be >= startLine (${params.startLine})"
      )
    else if !Files.exists(path) then
      ToolResult.error(
        s"file does not exist: ${params.path} — use the `write` tool to create it"
      )
    else if Files.isDirectory(path) then
      ToolResult.error(s"path is a directory, not a file: ${params.path}")
    else
      val content =
        try Files.readString(path, UTF_8).nn
        catch case e: IOException => return ToolResult.error(s"failed to read file: ${e.getMessage}")
      val lines = LineCodec.split(content)
      if lines.isEmpty then
        ToolResult.error(
          s"file is empty: ${params.path} — use the `write` tool to add content"
        )
      else if params.endLine >= lines.length then
        ToolResult.error(
          s"line ${params.endLine} is out of range: the file has ${lines.length} " +
            s"line(s) (lines 0..${lines.length - 1})"
        )
      else applyEdit(params, path, content, lines)

  private def applyEdit(
      params: EditParams,
      path: Path,
      content: String,
      lines: Vector[String]
  )(using ctx: RuntimeContext, async: Async): ToolResult =
    if !ctx.approvals.request(ApprovalRequest(name, s"edit ${params.path}")) then
      ToolResult.error(s"edit not approved: ${params.path}")
    else
      val start = params.startLine
      val end = params.endLine
      val count = end - start + 1
      val replacement =
        if params.content.isEmpty then Vector.empty
        else params.content.split("\n", -1).toVector
      val updated = lines.take(start) ++ replacement ++ lines.drop(end + 1)
      val rewritten = LineCodec.join(updated, LineCodec.endsWithNewline(content))
      try Files.writeString(path, rewritten, UTF_8)
      catch case e: IOException => return ToolResult.error(s"failed to write file: ${e.getMessage}")

      ToolResult.ok(
        s"Edited ${params.path}: replaced lines $start..$end " +
          s"($count line(s)) with ${replacement.length} line(s).",
        metadata = Map(
          "startLine" -> start.toString,
          "endLine" -> end.toString,
          "oldLineCount" -> count.toString,
          "newLineCount" -> replacement.length.toString
        )
      )
