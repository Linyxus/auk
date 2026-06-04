package auk.platform

import gears.async.Async

/** Outcome of a captured subprocess run. */
final case class ProcessResult(
    output: String,
    exitCode: Int,
    timedOut: Boolean,
    truncated: Boolean
)

/** Subprocess seam, replacing `java.lang.ProcessBuilder`.
  *
  * Runs asynchronously (via the runtime's child-process API + JSPI suspension)
  * so a long-running command does not block the event loop / UI. Cancelling the
  * surrounding gears scope kills the child.
  */
trait Process:
  /** Run `argv` in `cwd` with stdin closed and stdout+stderr merged, capturing up
    * to `maxOutputBytes` and killing the child after `timeoutMs`. */
  def runCaptured(
      argv: List[String],
      cwd: String,
      timeoutMs: Int,
      maxOutputBytes: Int
  )(using Async): ProcessResult
