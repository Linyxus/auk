package auk.runtime

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import gears.async.{Async, JvmAsyncOperations}

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}

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
  * The blocking wait runs inside `jvmInterruptible`, so cancelling the
  * surrounding turn interrupts the wait and kills the child too.
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
    val pb = new ProcessBuilder(Shell, "-c", command)
    pb.directory(ctx.workingDirectory.toFile)
    pb.redirectErrorStream(true)

    val proc =
      try pb.start().nn
      catch
        case e: IOException =>
          return ToolResult.error(s"failed to start command: ${e.getMessage}")

    // No input is provided; closing stdin gives readers EOF instead of a hang.
    try proc.getOutputStream.nn.close()
    catch case _: IOException => ()

    // Drain the merged output on a side thread so a chatty process can't fill
    // the pipe buffer and deadlock the wait below. We always read to EOF (even
    // past the cap, discarding the overflow) so the process can finish.
    val capture = new ByteArrayOutputStream()
    val truncated = new AtomicBoolean(false)
    val reader = startReader(proc, capture, truncated)

    var timedOut = false
    try
      JvmAsyncOperations.jvmInterruptible:
        if !proc.waitFor(timeoutMs.toLong, TimeUnit.MILLISECONDS) then
          timedOut = true
          proc.destroyForcibly()
          proc.waitFor() // reap the killed process
    catch
      case e: InterruptedException =>
        // The turn was cancelled: kill the child and let cancellation propagate
        // (the dispatcher does not catch InterruptedException).
        proc.destroyForcibly()
        reader.join(GraceMs)
        throw e

    // Give the reader a moment to flush whatever is left after exit/kill.
    reader.join(GraceMs)

    val exitCode =
      try proc.exitValue()
      catch case _: IllegalThreadStateException => -1
    val text = new String(capture.toByteArray.nn, UTF_8.nn)
    render(text, exitCode, timedOut, truncated.get(), timeoutMs)

  /** How long to wait for the reader thread to flush after the process ends. */
  private val GraceMs = 500L

  private def startReader(
      proc: Process,
      capture: ByteArrayOutputStream,
      truncated: AtomicBoolean
  ): Thread =
    val stream = proc.getInputStream.nn
    val t = new Thread(
      () =>
        val buf = new Array[Byte](8192)
        try
          var n = stream.read(buf)
          while n != -1 do
            if n > 0 then
              val room = MaxOutputBytes - capture.size()
              if room <= 0 then truncated.set(true)
              else
                capture.write(buf, 0, math.min(n, room))
                if n > room then truncated.set(true)
            n = stream.read(buf)
        catch case _: IOException => () // stream closed when the process is killed
      ,
      "bash-output-reader"
    )
    t.setDaemon(true)
    t.start()
    t

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
