package auk.runtime

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}

/** Arguments for the [[Bash]] tool. */
case class BashParams(
    @desc("The shell command to run")
    command: String,
    @desc("Timeout in milliseconds before the command is cancelled (optional)")
    timeoutMs: Option[Int] = None
) derives ToolInput

/** Run a shell command and return its combined output.
  *
  * STUB: the schema and contract are final; the body is not yet implemented.
  * When implemented it should request approval, spawn the process in
  * `ctx.workingDirectory`, enforce `timeoutMs` using the ambient `Async` scope
  * (so a cancelled turn kills the child), and return stdout/stderr with the
  * exit code in [[ToolResult.metadata]].
  */
object Bash extends Tool:
  type Params = BashParams

  val name = "bash"

  val description =
    "Run a shell command in the working directory and return its combined " +
      "stdout and stderr. Use timeoutMs to bound long-running commands."

  val input: ToolInput[BashParams] = ToolInput[BashParams]

  def execute(params: BashParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    // TODO: spawn `params.command` under ctx.workingDirectory with a deadline,
    // returning ToolResult.ok(output, metadata = Map("exitCode" -> code)).
    ToolResult.error("bash is not yet implemented")
