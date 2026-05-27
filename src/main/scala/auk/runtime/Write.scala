package auk.runtime

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}

/** Arguments for the [[Write]] tool. */
case class WriteParams(
    @desc("Path to the file to write, absolute or relative to the working directory")
    path: String,
    @desc(
      "The full contents to write. Creates the file (and any missing parent " +
        "directories) if it does not exist, and overwrites it entirely if it does."
    )
    content: String
) derives ToolInput

/** Create a new file or overwrite an existing one with the given content.
  *
  * The counterpart to [[Edit]]: where `edit` changes lines of an existing file,
  * `write` lays down a whole file at once. Use it to create a file that does not
  * exist yet (or is empty); missing parent directories are created along the
  * way.
  *
  * Side-effecting, so it consults [[RuntimeContext.approvals]] before writing.
  */
object Write extends Tool:
  type Params = WriteParams

  val name = "write"

  val description =
    "Create a new file or overwrite an existing one with `content`. Use this " +
      "when a file does not exist yet or is empty; use `edit` to change parts of " +
      "an existing file. Missing parent directories are created."

  val input: ToolInput[WriteParams] = ToolInput[WriteParams]

  def execute(params: WriteParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val path = ctx.resolve(params.path)
    if Files.isDirectory(path) then
      ToolResult.error(s"path is a directory, not a file: ${params.path}")
    else if !ctx.approvals.request(ApprovalRequest(name, s"write ${params.path}")) then
      ToolResult.error(s"write not approved: ${params.path}")
    else
      val existed = Files.exists(path)
      try
        val parent = path.getParent
        if parent != null then Files.createDirectories(parent)
        Files.writeString(path, params.content, UTF_8)
      catch case e: IOException => return ToolResult.error(s"failed to write file: ${e.getMessage}")

      val lineCount = LineCodec.split(params.content).length
      ToolResult.ok(
        s"${if existed then "Overwrote" else "Created"} ${params.path} ($lineCount line(s)).",
        metadata = Map(
          "created" -> (!existed).toString,
          "lineCount" -> lineCount.toString,
          "bytes" -> params.content.getBytes(UTF_8).nn.length.toString
        )
      )
