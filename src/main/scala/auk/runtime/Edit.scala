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
      "The exact existing text to replace, copied verbatim from the file — NOT " +
        "including the `<n>@ ` line-number prefixes that `read` prints. It must " +
        "occur exactly once; include enough surrounding lines to make it unique."
    )
    oldText: String,
    @desc("The replacement text. Empty deletes the matched text.")
    newText: String
) derives ToolInput

/** Replace an exact snippet of a file's text.
  *
  * `oldText` is matched verbatim against the file's contents and must occur
  * exactly once; it is replaced by `newText` (empty deletes it). Because edits
  * are anchored to content rather than position, they survive earlier edits that
  * shift line numbers — so a `read` stays valid across several consecutive edits.
  * The `<n>@ ` prefixes `read` prints are display only and must NOT be part of
  * `oldText`.
  *
  * It only edits an existing, non-empty file; an empty or missing file is an
  * error that points at the [[Write]] tool, which creates files.
  *
  * Side-effecting, so it consults [[RuntimeContext.approvals]] before writing —
  * but only after the edit is known to apply (a unique match was found).
  */
object Edit extends Tool:
  type Params = EditParams

  val name = "edit"

  val description =
    "Edit a file by replacing an exact snippet of its text. `oldText` is matched " +
      "verbatim and must occur exactly once (add surrounding lines to disambiguate); " +
      "it is replaced by `newText` (empty deletes it). `oldText` is raw file text, " +
      "WITHOUT the `<n>@ ` prefixes `read` shows. To create a new file or fill an " +
      "empty one, use the `write` tool instead."

  val input: ToolInput[EditParams] = ToolInput[EditParams]

  def execute(params: EditParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val path = ctx.resolve(params.path)
    if params.oldText.isEmpty then
      ToolResult.error(
        "oldText must not be empty — use the `write` tool to create or replace whole-file content"
      )
    else if params.oldText == params.newText then
      ToolResult.error("oldText and newText are identical; nothing to change")
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
      if content.isEmpty then
        ToolResult.error(
          s"file is empty: ${params.path} — use the `write` tool to add content"
        )
      else
        val matches = matchStartIndices(content, params.oldText)
        if matches.isEmpty then
          ToolResult.error(
            s"oldText not found in ${params.path}; copy it verbatim from a `read` " +
              "(without the `<n>@ ` prefix)"
          )
        else if matches.length > 1 then notUnique(params.path, content, matches)
        else applyEdit(params, path, content, matches.head)

  private def applyEdit(
      params: EditParams,
      path: Path,
      content: String,
      at: Int
  )(using ctx: RuntimeContext, async: Async): ToolResult =
    if !ctx.approvals.request(ApprovalRequest(name, s"edit ${params.path}")) then
      ToolResult.error(s"edit not approved: ${params.path}")
    else
      val updated =
        content.substring(0, at) + params.newText +
          content.substring(at + params.oldText.length)
      try Files.writeString(path, updated, UTF_8)
      catch case e: IOException => return ToolResult.error(s"failed to write file: ${e.getMessage}")

      val oldLines = lineCount(params.oldText)
      val newLines = lineCount(params.newText)
      ToolResult.ok(
        s"Edited ${params.path}: replaced $oldLines line(s) with $newLines line(s).",
        metadata = Map(
          "oldLineCount" -> oldLines.toString,
          "newLineCount" -> newLines.toString
        )
      )

  /** Start indices of every non-overlapping occurrence of `needle` (non-empty). */
  private def matchStartIndices(haystack: String, needle: String): List[Int] =
    val buf = List.newBuilder[Int]
    var i = haystack.indexOf(needle)
    while i >= 0 do
      buf += i
      i = haystack.indexOf(needle, i + needle.length)
    buf.result()

  /** The 0-based line number that character offset `at` falls on. */
  private def lineAt(content: String, at: Int): Int =
    var line = 0
    var i = 0
    while i < at do
      if content.charAt(i) == '\n' then line += 1
      i += 1
    line

  /** Up to this many distinct matching lines are listed in the ambiguity error. */
  private val MaxShownMatches = 10

  /** The "not unique" error, listing each line a match falls on with its
    * `read`-style `<n>@` number, so the model can add surrounding context. */
  private def notUnique(path: String, content: String, matches: List[Int]): ToolResult =
    val lines = LineCodec.split(content)
    val matchLines = matches.map(lineAt(content, _)).distinct
    val shown = matchLines.take(MaxShownMatches)
    val rendered = shown.map { ln =>
      val idx = ln.min(lines.length - 1)
      "  " + LineCodec.render(idx, lines(idx))
    }
    val more =
      if matchLines.length > shown.length then
        s"\n  … and ${matchLines.length - shown.length} more line(s)"
      else ""
    ToolResult.error(
      s"oldText is not unique in $path (found ${matches.length} matches on " +
        s"${matchLines.length} line(s)); add surrounding lines so it matches exactly " +
        s"once. Matches are on these lines:\n" + rendered.mkString("\n") + more
    )

  /** Lines spanned by `s`: 0 for empty, else one more than its newline count. */
  private def lineCount(s: String): Int =
    if s.isEmpty then 0 else s.count(_ == '\n') + 1
