package auk.runtime

import gears.async.Async

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}
import auk.platform.Platform

/** Arguments for the [[Bash]] tool. */
case class BashParams(
    @desc("The shell command to run")
    command: String,
    @desc(
      "Timeout in milliseconds before the command is killed. Defaults to " +
        "120000 (2 minutes) and is capped at 600000 (10 minutes)."
    )
    timeoutMs: Option[Int] = None
) derives ToolInput

/** Run a shell command and return its combined stdout and stderr.
  *
  * The command runs through `bash -c` in [[RuntimeContext.workingDirectory]],
  * with stdin closed (so commands that read stdin see EOF rather than hanging).
  * stdout and stderr are merged into one stream and captured up to
  * [[Bash.MaxOutputBytes]]; beyond that the output is truncated and flagged.
  *
  * Completion is bounded by `timeoutMs`: when it elapses the process tree is
  * killed and the call returns what was captured so far, marked as an error.
  * The wait runs as a JSPI-suspended await, so cancelling the surrounding turn
  * kills the child too (see `auk.platform.js.NodeProcess`).
  *
  * Result conventions:
  *   - exit code `0`           → success; `output` is the captured text.
  *   - non-zero exit code      → `isError`; output gains a `[command exited
  *     with code N]` footer.
  *   - timed out               → `isError`; output gains a timeout footer.
  *   - [[ToolResult.metadata]] always carries `exitCode`, `timedOut`, and
  *     `truncated`.
  *
  * Side-effecting, so it consults [[RuntimeContext.approvals]] before running.
  */
object Bash extends Tool:
  type Params = BashParams

  val name = "bash"

  val description =
    "Run a shell command in the working directory and return its combined " +
      "stdout and stderr. Use timeoutMs to bound long-running commands " +
      "(default 2 minutes)."

  val input: ToolInput[BashParams] = ToolInput[BashParams]

  /** Shell used to interpret the command. */
  private val Shell = "bash"

  /** Timeout applied when the caller does not specify one. */
  val DefaultTimeoutMs = 120_000

  /** Upper bound on any requested timeout, to keep a turn from hanging. */
  val MaxTimeoutMs = 600_000

  /** Captured output is truncated past this many bytes. */
  val MaxOutputBytes = 100_000

  def execute(params: BashParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val command = params.command
    if command.trim.isEmpty then ToolResult.error("empty command")
    else if !ctx.approvals.request(ApprovalRequest(name, command)) then
      ToolResult.error(s"command not approved: $command")
    else
      val timeout =
        params.timeoutMs.getOrElse(DefaultTimeoutMs).max(1).min(MaxTimeoutMs)
      runShell(command, ctx, timeout)

  private def runShell(command: String, ctx: RuntimeContext, timeoutMs: Int)(using
      Async
  ): ToolResult =
    val r = Platform.process.runCaptured(
      List(Shell, "-c", command),
      ctx.workingDirectory,
      timeoutMs,
      MaxOutputBytes
    )
    render(r.output, r.exitCode, r.timedOut, r.truncated, timeoutMs)

  /** Assemble the output text and metadata from a finished run. */
  private def render(
      text: String,
      exitCode: Int,
      timedOut: Boolean,
      truncated: Boolean,
      timeoutMs: Int
  ): ToolResult =
    val sb = new StringBuilder
    if text.nonEmpty then sb.append(text)
    if truncated then
      sb.append(s"\n[output truncated to $MaxOutputBytes bytes]")
    if timedOut then
      sb.append(s"\n[command timed out after ${timeoutMs}ms and was killed]")
    else if exitCode != 0 then sb.append(s"\n[command exited with code $exitCode]")

    val output =
      if sb.isEmpty then "(no output)"
      else sb.toString.stripPrefix("\n")

    ToolResult(
      output = output,
      isError = timedOut || exitCode != 0,
      metadata = Map(
        "exitCode" -> exitCode.toString,
        "timedOut" -> timedOut.toString,
        "truncated" -> truncated.toString
      )
    )
