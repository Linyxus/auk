package auk.runtime

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}

/** Arguments for the [[Read]] tool. */
case class ReadParams(
    @desc("Path to the file to read, absolute or relative to the working directory")
    path: String,
    @desc("0-based line number to start reading from (optional)")
    offset: Option[Int] = None,
    @desc("Maximum number of lines to read (optional)")
    limit: Option[Int] = None
) derives ToolInput

/** Read the contents of a file.
  *
  * STUB: the schema and contract are final; the body is not yet implemented.
  * To finish it, resolve `params.path` against `ctx.workingDirectory`, read the
  * (optional) line window, and return the text via [[ToolResult.ok]] — guarding
  * missing/oversized files with [[ToolResult.error]].
  */
object Read extends Tool:
  type Params = ReadParams

  val name = "read"

  val description =
    "Read a file from the filesystem. Returns the file's text, optionally " +
      "limited to a window of lines via `offset` and `limit`."

  val input: ToolInput[ReadParams] = ToolInput[ReadParams]

  def execute(params: ReadParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    // TODO: read ctx.resolve(params.path), honouring offset/limit.
    ToolResult.error("read is not yet implemented")
