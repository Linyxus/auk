package auk.runtime.repl

import scala.scalajs.js.timers
import scala.util.Success
import gears.async.{Async, Future}

import auk.platform.js.{LineProcess, ReplArtifacts}

/** A persistent Scala REPL session hosted in a child worker process.
  *
  * The worker (see [[auk.platform.js.ReplArtifacts]]) is spawned lazily on the
  * first [[eval]] and reused: REPL state — vals, defs, classes, givens,
  * imports — accumulates across calls. If it dies (crash, kill, or an eval
  * timeout), the next eval spawns a fresh worker and reports
  * `restartedSession = true` so the caller can tell the model its definitions
  * are gone.
  *
  * Every fresh worker first evaluates the [[preamble]] (by default
  * [[ReplPreamble.Source]]: import the runtime library, bind `lib`), so the
  * session scope promised by the system prompt holds from the first user eval
  * and is restored on every respawn. A preamble failure discards the worker
  * and surfaces as a distinct error — it is an auk bug or a stale
  * library.bin, never the model's code.
  *
  * Calls are FIFO-serialised: the worker handles one request at a time and
  * answers in order, so concurrent evals queue here rather than interleaving.
  * A timed-out or cancelled eval kills the worker — its eventual response
  * could no longer be paired with a request, and a half-consumed session would
  * silently desync.
  */
final class ScalaRepl(
    spawnSpec: () => Either[String, ReplArtifacts.Spawn] = () => ReplArtifacts.resolve(),
    preamble: String = ReplPreamble.Source
):
  import ScalaRepl.*

  private var worker: Option[LineProcess.Handle] = None
  private var inFlight: Option[Future.Promise[Status]] = None
  private var stderrTail = ""
  private var everDied = false
  private var queueTail: Future[Unit] | Null = null
  // Each spawn gets a fresh generation; stream/exit events carry the one they
  // were registered under, and stale ones are dropped. A SIGKILLed worker's
  // `close` event arrives on a later tick — without this it could be
  // misattributed to a request already running on the replacement worker.
  private var generation = 0

  /** Evaluate `code` as one REPL entry, waiting at most `timeoutMs`. */
  def eval(code: String, timeoutMs: Int)(using Async): EvalResult = serialized:
    ensureWorker() match
      case Left(err) => EvalResult(Status.Failed(err), restartedSession = false)
      case Right((handle, restarted)) =>
        EvalResult(request(handle, ReplProtocol.evalRequest(code), timeoutMs), restarted)

  /** Shut the worker down — gracefully, then by force. Idempotent. */
  def close()(using Async): Unit = serialized:
    worker.foreach: handle =>
      if handle.alive then
        val _ = request(handle, ReplProtocol.shutdownRequest, ShutdownGraceMs)
      handle.kill()
    worker = None

  // -- worker lifecycle --------------------------------------------------------

  private def ensureWorker()(using Async): Either[String, (LineProcess.Handle, Boolean)] =
    worker.filter(_.alive) match
      case Some(h) => Right((h, false))
      case None =>
        spawnSpec().flatMap: spec =>
          val restarted = everDied
          stderrTail = ""
          generation += 1
          val gen = generation
          val handle = LineProcess.spawn(
            spec.argv,
            spec.env,
            onLine = line => handleLine(gen, line),
            onStderr = chunk =>
              if gen == generation then
                stderrTail = (stderrTail + chunk).takeRight(StderrTailBytes),
            onExit = code => handleExit(gen, code)
          )
          worker = Some(handle)
          loadPreamble(handle).map(_ => (handle, restarted))

  /** Evaluate the preamble as a fresh worker's first entry. On failure the
    * worker is discarded — a session without its promised scope must not serve
    * evals — and the error names the preamble so it cannot be mistaken for a
    * problem with the model's code. */
  private def loadPreamble(handle: LineProcess.Handle)(using Async): Either[String, Unit] =
    if preamble.isEmpty then Right(())
    else
      request(handle, ReplProtocol.evalRequest(preamble), PreambleTimeoutMs) match
        case Status.Completed(r) if r.ok => Right(())
        case Status.Completed(r) =>
          everDied = true
          worker = None
          handle.kill()
          val detail = if r.output.nonEmpty then r.output else r.error.getOrElse("unknown error")
          Left(
            "the REPL preamble failed to load (an auk bug or a stale library.bin, " +
              s"not a problem with the evaluated code):\n${ReplProtocol.stripAnsi(detail)}"
          )
        case Status.TimedOut(ms) =>
          // The request timeout already killed the worker.
          Left(s"the REPL preamble timed out after ${ms}ms")
        case Status.Failed(reason) =>
          // The exit handler already cleaned up.
          Left(s"the REPL preamble failed to load: $reason")

  private def handleLine(gen: Int, line: String): Unit =
    if gen == generation then
      settle:
        ReplProtocol.parse(line) match
          case Right(r) if r.op == "protocol" =>
            Status.Failed(s"REPL protocol error: ${r.error.getOrElse("unknown")}")
          case Right(r) => Status.Completed(r)
          case Left(err) => Status.Failed(err)

  private def handleExit(gen: Int, code: Int): Unit =
    if gen == generation then
      everDied = true
      worker = None
      settle:
        val detail = if stderrTail.isEmpty then "" else s"; recent stderr:\n$stderrTail"
        Status.Failed(s"REPL worker exited with code $code$detail")

  /** Complete the in-flight request, if any, exactly once. */
  private def settle(status: => Status): Unit =
    inFlight match
      case Some(p) =>
        inFlight = None
        p.complete(Success(status))
      case None => ()

  // -- request/response --------------------------------------------------------

  private def request(handle: LineProcess.Handle, json: String, timeoutMs: Int)(using
      Async
  ): Status =
    val p = Future.Promise[Status]()
    inFlight = Some(p)
    if !handle.writeLine(json) then
      inFlight = None
      Status.Failed("REPL worker is not accepting input")
    else
      val timer = timers.setTimeout(timeoutMs.toDouble):
        settle(Status.TimedOut(timeoutMs))
        // Mid-eval, only a kill stops the worker; the session state is lost.
        everDied = true
        worker = None
        handle.kill()
      try p.asFuture.await
      finally
        timers.clearTimeout(timer)
        if inFlight.contains(p) then
          // Cancelled mid-await: the eventual response can no longer be paired
          // with a request, so the session is unusable — kill it.
          inFlight = None
          everDied = true
          worker = None
          handle.kill()

  /** FIFO-serialise `body` against other calls. Single-threaded JS makes the
    * tail swap race-free; predecessors are awaited, failures included. */
  private def serialized[T](body: Async ?=> T)(using Async): T =
    val gate = Future.Promise[Unit]()
    val prev = queueTail
    queueTail = gate.asFuture
    if prev != null then { val _ = prev.awaitResult }
    try body
    finally gate.complete(Success(()))

object ScalaRepl:
  enum Status:
    case Completed(response: ReplProtocol.Response)
    case TimedOut(timeoutMs: Int)
    case Failed(reason: String)

  /** `restartedSession`: this call ran on a fresh worker after a previous
    * session died, so definitions accumulated before it are gone. */
  final case class EvalResult(status: Status, restartedSession: Boolean)

  private val ShutdownGraceMs = 1_000
  private val StderrTailBytes = 4_000

  /** The preamble compiles right after worker boot, which is when the JS-hosted
    * compiler is at its slowest — give it generous room. */
  private val PreambleTimeoutMs = 60_000
