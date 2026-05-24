package auk.runtime

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}

/** Arguments for the [[Edit]] tool. */
case class EditParams(
    @desc("Path to the file to edit, absolute or relative to the working directory")
    path: String,
    @desc("The exact text to replace; must occur in the file")
    oldString: String,
    @desc("The text to replace it with")
    newString: String,
    @desc("Replace every occurrence instead of requiring a unique match")
    replaceAll: Option[Boolean] = None
) derives ToolInput

/** Replace a span of text in a file.
  *
  * STUB: the schema and contract are final; the body is not yet implemented.
  * When implemented it should request approval (it mutates the world), then
  * resolve the path, verify `oldString` occurs exactly once (unless
  * `replaceAll`), apply the edit, and report what changed.
  */
object Edit extends Tool:
  type Params = EditParams

  val name = "edit"

  val description =
    "Edit a file by replacing an exact string with new text. By default the " +
      "old string must match exactly once; set replaceAll to replace every " +
      "occurrence."

  val input: ToolInput[EditParams] = ToolInput[EditParams]

  def execute(params: EditParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    // Sketch of the approval gate a mutating tool will use:
    //   if !ctx.approvals.request(ApprovalRequest(name, s"edit ${params.path}")) then
    //     return ToolResult.error("edit denied by approval policy")
    // TODO: apply the replacement to ctx.resolve(params.path).
    ToolResult.error("edit is not yet implemented")
