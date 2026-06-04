package auk.runtime

import java.io.IOException

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}

/** Arguments for the [[Read]] tool. */
case class ReadParams(
    @desc("Path to the file to read, absolute or relative to the working directory")
    path: String,
    @desc("0-based line number to start reading from. Defaults to 0 (the first line).")
    offset: Option[Int] = None,
    @desc("Maximum number of lines to read. Defaults to reading to the end of the file.")
    limit: Option[Int] = None
) derives ToolInput

/** Read a file as numbered lines.
  *
  * Every line is rendered as `<n>@ <content>`, where `n` is the 0-based line
  * number, shown for orientation. The [[Edit]] tool anchors on content, not line
  * numbers, so to change the file you copy the relevant text — without the
  * `<n>@ ` prefix — into edit's `oldText`. `offset` and `limit` select a window;
  * the prefixes keep the file's true line numbers regardless of where the window
  * starts.
  *
  * An empty or nonexistent file is reported as a plain (non-error) message that
  * points at the [[Write]] tool, since there is nothing to read yet.
  *
  * Read-only, so it does not consult the approval policy.
  */
object Read extends Tool:
  type Params = ReadParams

  val name = "read"

  val description =
    "Read a file from the filesystem as numbered lines. Each line is returned " +
      "as `<n>@ <content>` with `n` the 0-based line number, shown for " +
      "orientation. Use `offset` and `limit` to read a window. To change the " +
      "file, copy the relevant text (WITHOUT the `<n>@ ` prefix) into the `edit` " +
      "tool's `oldText`. If the file is empty or does not exist, use the `write` " +
      "tool to create it."

  val input: ToolInput[ReadParams] = ToolInput[ReadParams]

  /** Files larger than this are refused outright to bound memory use. */
  val MaxFileBytes = 5_000_000L

  /** The rendered output is truncated past this many bytes. */
  val MaxOutputBytes = 100_000

  def execute(params: ReadParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val path = ctx.resolve(params.path)
    val offset = params.offset.getOrElse(0)
    if offset < 0 then ToolResult.error(s"offset must be >= 0, got $offset")
    else if params.limit.exists(_ < 0) then
      ToolResult.error(s"limit must be >= 0, got ${params.limit.get}")
    else if !ctx.fs.exists(path) then
      // Not an error: tell the model how to create it.
      ToolResult.ok(
        s"This file does not exist: ${params.path}\n" +
          "Use the `write` tool to create it with content.",
        metadata = Map("exists" -> "false", "totalLines" -> "0")
      )
    else if ctx.fs.isDirectory(path) then
      ToolResult.error(s"path is a directory, not a file: ${params.path}")
    else
      val size = ctx.fs.size(path)
      if size > MaxFileBytes then
        ToolResult.error(
          s"file is too large to read ($size bytes > $MaxFileBytes); " +
            "narrow the read with offset/limit on a smaller file"
        )
      else
        try render(ctx.fs.readString(path), offset, params.limit)
        catch case e: IOException => ToolResult.error(s"failed to read file: ${e.getMessage}")

  private def render(content: String, offset: Int, limit: Option[Int]): ToolResult =
    val lines = LineCodec.split(content)
    val total = lines.length

    if total == 0 then
      // An existing-but-empty file: again, point at write rather than read.
      return ToolResult.ok(
        "This file is empty.\nUse the `write` tool to add content to it.",
        metadata = Map("exists" -> "true", "totalLines" -> "0")
      )

    val from = offset.min(total)
    val until = limit.fold(total)(l => (from + l).min(total))

    val sb = new StringBuilder
    var i = from
    var truncated = false
    while i < until && !truncated do
      val rendered = LineCodec.render(i, lines(i))
      if sb.length + rendered.length + 1 > MaxOutputBytes then truncated = true
      else
        if sb.nonEmpty then sb.append('\n')
        sb.append(rendered)
        i += 1

    if truncated then
      sb.append(s"\n[output truncated at $MaxOutputBytes bytes; read more with offset=$i]")

    val output =
      if from >= total then s"(offset $offset is past the end; file has $total line(s))"
      else sb.toString

    ToolResult.ok(
      output,
      metadata = Map(
        "totalLines" -> total.toString,
        "from" -> from.toString,
        "to" -> (until - 1).max(from).toString,
        "truncated" -> truncated.toString
      )
    )
