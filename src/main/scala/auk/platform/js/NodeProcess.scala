package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.timers
import scala.util.Success
import gears.async.{Async, Future}

import auk.platform.{Process, ProcessResult}

/** [[Process]] backed by Node/Bun `child_process.spawn`.
  *
  * Output is drained via `data` events (so a chatty child can't deadlock), the
  * run is bounded by a `setTimeout` kill, and cancelling the surrounding gears
  * scope (the `finally`) kills the child — the JSPI equivalent of the old
  * `jvmInterruptible` wait.
  */
object NodeProcess extends Process:
  def runCaptured(
      argv: List[String],
      cwd: String,
      timeoutMs: Int,
      maxOutputBytes: Int
  )(using Async): ProcessResult =
    val opts = js.Dynamic.literal(
      cwd = cwd,
      stdio = js.Array[String]("ignore", "pipe", "pipe")
    )
    val child = NodeChildProcess.spawn(argv.head, js.Array(argv.tail*), opts.asInstanceOf[js.Object])

    val sb = new StringBuilder
    var truncated = false
    def append(chunk: js.Any): Unit =
      if !truncated then
        val s = chunk.asInstanceOf[String]
        val room = maxOutputBytes - sb.length
        if room <= 0 then truncated = true
        else if s.length > room then { sb.append(s.substring(0, room)); truncated = true }
        else sb.append(s)

    child.stdout.setEncoding("utf8")
    child.stderr.setEncoding("utf8")
    child.stdout.on("data", append)
    child.stderr.on("data", append)

    val pr = Future.Promise[ProcessResult]()
    var settled = false
    var timedOut = false

    val timer = timers.setTimeout(timeoutMs.toDouble) {
      timedOut = true
      child.kill("SIGKILL")
    }
    def settle(r: ProcessResult): Unit =
      if !settled then
        settled = true
        timers.clearTimeout(timer)
        pr.complete(Success(r))

    child.on("error", (e: js.Any, _: js.Any) => settle(ProcessResult(s"failed to start command: ${e.toString}", -1, false, false)))
    child.on("close", (code: js.Any, signal: js.Any) =>
      // Node reports a signal-killed child as code=null, signal="SIGKILL"; mirror
      // the shell's 128+signal convention so e.g. SIGKILL → 137.
      val exit =
        if code != null then code.asInstanceOf[Int]
        else if signal != null then 128 + signalNumber(signal.asInstanceOf[String])
        else -1
      settle(ProcessResult(sb.toString, if timedOut then -1 else exit, timedOut, truncated))
    )

    try pr.asFuture.await
    finally
      timers.clearTimeout(timer)
      if !settled then child.kill("SIGKILL") // scope cancelled mid-run

  private def signalNumber(name: String): Int = name match
    case "SIGHUP"  => 1
    case "SIGINT"  => 2
    case "SIGQUIT" => 3
    case "SIGILL"  => 4
    case "SIGABRT" => 6
    case "SIGFPE"  => 8
    case "SIGKILL" => 9
    case "SIGBUS"  => 10
    case "SIGSEGV" => 11
    case "SIGPIPE" => 13
    case "SIGALRM" => 14
    case "SIGTERM" => 15
    case _         => 0
