package auk.runtime.repl

import scala.scalajs.js
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
  *
  * A protocol-2 worker also announces itself at boot and reports progress
  * while it works (see [[ReplProtocol.Line]]), which is what [[liveness]] and
  * an [[EvalResult]]'s diagnosis are derived from. Those signals are
  * observations, never control: a worker that stops ticking is described, not
  * killed, because only the timeout and the caller's cancellation may end a
  * request. A worker that sends none of them (any released worker so far) is
  * legacy — [[liveness]] stays `None`, diagnoses stay `None`, and every other
  * decision this class makes is bit-for-bit what it was before liveness
  * existed.
  *
  * @param makeLog
  *   builds the [[WorkerLog]] for one spawned worker, from its generation and
  *   the pid the spawn returned. Called once per spawn, so each worker's
  *   record is its own file; a [[spawnSpec]] that resolves to `Left` never
  *   reaches a process and therefore gets no log at all.
  */
final class ScalaRepl(
    spawnSpec: () => Either[String, ReplArtifacts.Spawn] = () => ReplArtifacts.resolve(),
    preamble: String = ReplPreamble.Source,
    extraEnv: Map[String, String] = Map.empty,
    makeLog: (Int, Option[Int]) => WorkerLog = WorkerLogs.NoopFactory
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

  /** The newest worker's log. Kept past `worker = None` so that the events a
    * death produces — the settle, the kill — still reach the file of the
    * worker they are about. Per-worker events (a line, an exit) instead use
    * the log their callback closed over, which is the only way a stale event
    * can land in the file of the worker that actually produced it. */
  private var currentLog: WorkerLog = WorkerLog.Noop

  /** The current worker's boot announcement; `None` for a legacy worker, and
    * the switch that keeps every liveness feature dark for one. */
  private var helloInfo: Option[ReplProtocol.Line.Hello] = None

  // What the worker has told us about the request in flight. Set when a
  // request is written, cleared when it settles; `requestSentAt` is therefore
  // also the answer to "is a request outstanding".
  //
  // `lastSignalAt` is the newest line of *any* kind that only a turning event
  // loop could have produced — a tick or a progress line. Both are signals in
  // exactly the same sense, so a worker that reports phases without ticking is
  // as demonstrably alive as one that only ticks.
  private var requestSentAt: Option[Double] = None
  private var ackAt: Option[Double] = None
  private var lastSignalAt: Option[Double] = None
  private var lastPhase: Option[String] = None
  private var lastLivenessKind: Option[String] = None

  /** When the worker entered the stage [[lastPhase]] names — moved only when
    * the stage itself changes, so it times the stage rather than the chatter
    * about it. Set and cleared with `lastPhase`, so the two can never disagree. */
  private var phaseChangedAt: Option[Double] = None

  /** How liveness looked when the in-flight request died — taken at the kill
    * or exit itself, before the per-request state is cleared, and read out by
    * [[eval]] on the way back. */
  private var lastDiagnosis: Option[String] = None

  /** Evaluate `code` as one REPL entry. A `timeoutMs` arms a kill timer; `None`
    * (the eval_scala default) runs until the worker answers or the caller
    * cancels — a long workflow is bounded by the user's interrupt, not a clock. */
  def eval(code: String, timeoutMs: Option[Int])(using Async): EvalResult = serialized:
    ensureWorker() match
      case Left(err) => EvalResult(Status.Failed(err), restartedSession = false)
      case Right((handle, restarted)) =>
        val status = request(handle, ReplProtocol.evalRequest(code), timeoutMs)
        EvalResult(status, restarted, lastDiagnosis)

  /** Shut the worker down — gracefully, then by force. Idempotent. */
  def close()(using Async): Unit = serialized:
    worker.foreach: handle =>
      if handle.alive then
        val _ = request(handle, ReplProtocol.shutdownRequest, Some(ShutdownGraceMs))
      currentLog.killed("shutdown")
      handle.kill()
    worker = None

  // -- liveness ----------------------------------------------------------------

  /** What the worker is observably doing about the request in flight, or
    * `None` when nothing is in flight or the worker never said hello.
    *
    * Derived at the moment of the call — this class starts no timer of its own
    * for it, so a caller that never asks pays nothing and a caller that asks
    * decides its own resolution. Asking also logs a liveness transition when
    * the answer's kind differs from the last one logged for this request: the
    * log records what an observer was actually told, which is the only thing
    * this class knows.
    *
    * That makes the log's liveness stream the pull-based transitions observed
    * while a request was in flight, plus exactly one forced snapshot at its
    * death (see [[logLivenessSnapshot]]) — so a post-mortem never depends on
    * anyone having been watching. */
  def liveness: Option[Liveness] =
    snapshot().map: (state, silentMs) =>
      val kind = livenessKind(state)
      // The kind alone decides whether this is a transition worth logging. The
      // phase is carried on the events that do fire and is never a reason for
      // one: a worker moving from 'compiling' to 'running' has not changed what
      // it looks like from here, and a log that said otherwise would be a record
      // of the worker's chatter rather than of its liveness.
      if !lastLivenessKind.contains(kind) then
        lastLivenessKind = Some(kind)
        currentLog.liveness(kind, silentMs, lastPhase)
      state

  /** The live worker's OS process id; `None` when no worker is up (including
    * the moment right after one died) or the spawn reported none. */
  def workerPid: Option[Int] = worker.flatMap(_.pid)

  /** The stage of its pipeline the worker last named for the request in flight
    * — `compiling`, `running`, `rendering`, or anything else a worker chooses to
    * call itself. `None` when it has named none: no request out, a worker not
    * there yet, or a worker that does not report phases at all.
    *
    * The worker's own words, kept only to be shown ([[displayPhase]] bounds
    * them); nothing here or above ever decides anything by their content. */
  def phase: Option[String] = lastPhase

  /** How long the worker has been in the stage [[phase]] names, or `None` when
    * it names none. Derived at the moment of the call like [[liveness]], and
    * measured from the stage's arrival rather than from the request's, so a
    * compile that hands over to a run is two clocks and not one — which is the
    * whole point of naming stages at all. */
  def phaseAgeMs: Option[Long] = phaseChangedAt.map(at => (js.Date.now() - at).toLong)

  /** The liveness state plus the silence it was derived from, without touching
    * the log — the shared source of [[liveness]] and any diagnosis. Silence is
    * measured from the last signal of any kind, so a worker that acked and then
    * ticked or reported progress is judged on whichever of those came last. */
  private def snapshot(): Option[(Liveness, Long)] =
    for
      hello <- helloInfo
      sentAt <- requestSentAt
    yield
      val now = js.Date.now()
      val elapsed = (now - sentAt).toLong
      ackAt match
        // No ack at all: nothing has been heard about this request, so how long
        // the worker has been quiet *is* how long the request has been out.
        case None => (Liveness.AwaitingAck(elapsed), elapsed)
        case Some(ack) =>
          val silent = (now - math.max(ack, lastSignalAt.getOrElse(ack))).toLong
          val state =
            if silent > BlockedAfterMissedTicks * hello.tickMs then Liveness.Blocked(elapsed, silent)
            else Liveness.Working(elapsed)
          (state, silent)

  private def livenessKind(state: Liveness): String = state match
    case _: Liveness.AwaitingAck => "awaiting-ack"
    case _: Liveness.Working     => "working"
    case _: Liveness.Blocked     => "blocked"

  /** Record where liveness stood, transition or not — called at every death of
    * an in-flight request (timeout, exit, cancellation), immediately before the
    * state is cleared. The log is then a post-mortem in its own right: a
    * structured state and silence at the death, not just the prose a settle
    * carries and not just for whoever happened to be watching. The stage the
    * worker had last named goes in the same event: at a death it is the single
    * most explanatory thing known about it, and a reader should not have to
    * reconstruct it from the `recv` lines around it. A no-op when nothing is in
    * flight or the worker never said hello, so callers need not check. */
  private def logLivenessSnapshot(): Unit =
    snapshot().foreach: (state, silentMs) =>
      lastLivenessKind = Some(livenessKind(state))
      currentLog.liveness(livenessKind(state), silentMs, lastPhase)

  /** Turn the liveness picture into the sentence a reader needs at a death:
    * which of "it was idle and waiting", "its loop was occupied or stuck" and
    * "it never even heard us" this was. Only a worker that said hello can be
    * described this way; for a legacy one there is nothing to say and nothing
    * is said. */
  private def captureDiagnosis(): Unit =
    lastDiagnosis =
      for
        hello <- helloInfo
        (state, _) <- snapshot()
      yield
        val who = workerPid.orElse(hello.pid).fold("the worker")(p => s"worker pid $p")
        state match
          case _: Liveness.AwaitingAck =>
            s"$who never acknowledged the request — it may never have received it"
          case _: Liveness.Working =>
            s"$who was heartbeating throughout — the eval was awaiting something that never completed"
          case Liveness.Blocked(_, silentForMs) =>
            // The stage it went quiet in is most of the answer: a worker silent
            // during 'compiling' was most likely occupied BY the compile, and
            // saying so beats offering the reader the same three guesses.
            val during = lastPhase.fold("")(p => s" during '$p'")
            s"$who's event loop had been silent for ${seconds(silentForMs)}$during — a long computation, " +
              "a stuck synchronous call, or an infinite loop"

  private def seconds(ms: Long): String =
    val s = ms / 1000.0
    if s >= 10 then s"${s.round}s" else f"$s%.1fs"

  /** A worker's phase token as text fit to show: its first line, trimmed, and
    * cut to [[MaxPhaseChars]]. The tokens are the worker's free choice, so the
    * one guarantee this parent must make about them is that no phase can wreck
    * a line of UI or the middle of a diagnosis sentence. A token left empty by
    * that is no phase at all. */
  private def displayPhase(raw: String): Option[String] =
    raw.linesIterator.nextOption().map(_.trim.nn).filter(_.nonEmpty).map(_.take(MaxPhaseChars))

  // -- worker lifecycle --------------------------------------------------------

  private def ensureWorker()(using Async): Either[String, (LineProcess.Handle, Boolean)] =
    worker.filter(_.alive) match
      case Some(h) => Right((h, false))
      case None =>
        spawnSpec().flatMap: spec =>
          val restarted = everDied
          stderrTail = ""
          helloInfo = None
          generation += 1
          val gen = generation
          val env = spec.env ++ extraEnv
          // The callbacks must log into this worker's own file, which cannot be
          // opened before the spawn hands back the pid it is named after — so
          // they read a cell filled in immediately after. Nothing can observe it
          // empty: node delivers stream and exit events on a later tick.
          var log: WorkerLog = WorkerLog.Noop
          val handle = LineProcess.spawn(
            spec.argv,
            env,
            onLine = line => handleLine(gen, log, line),
            onStderr = chunk =>
              log.stderrChunk(chunk)
              if gen == generation then
                stderrTail = (stderrTail + chunk).takeRight(StderrTailBytes),
            onExit = code => handleExit(gen, log, code)
          )
          log = makeLog(gen, handle.pid)
          currentLog = log
          // Key names only: an env value may be a credential (see WorkerLog).
          log.spawned(spec.argv, gen, env.keys.toList.sorted, handle.pid)
          worker = Some(handle)
          loadPreamble(handle).map(_ => (handle, restarted))

  /** Evaluate the preamble as a fresh worker's first entry. On failure the
    * worker is discarded — a session without its promised scope must not serve
    * evals — and the error names the preamble so it cannot be mistaken for a
    * problem with the model's code. */
  private def loadPreamble(handle: LineProcess.Handle)(using Async): Either[String, Unit] =
    if preamble.isEmpty then Right(())
    else
      request(handle, ReplProtocol.evalRequest(preamble), Some(PreambleTimeoutMs)) match
        case Status.Completed(r) if r.ok => Right(())
        case Status.Completed(r) =>
          everDied = true
          worker = None
          currentLog.killed("preamble-failure")
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

  /** Every line the worker sends is recorded with the classification it was
    * given, before the generation guard, so a stale worker's parting words are
    * in its own file rather than lost. Only the guard's survivors change any
    * state, and only a [[ReplProtocol.Line.Reply]] among them settles the
    * request: the liveness ops are observations, and an unrecognised op is
    * dropped so that a newer worker cannot break an older parent. A line that
    * is not a protocol message at all still fails the request — mid-eval
    * corruption of the stream must stay loud. */
  private def handleLine(gen: Int, log: WorkerLog, line: String): Unit =
    val classified = ReplProtocol.classify(line)
    log.recv(line, kindOf(classified))
    if gen == generation then
      classified match
        case Right(ReplProtocol.Line.Reply(r)) if r.op == "protocol" =>
          settle(Status.Failed(s"REPL protocol error: ${r.error.getOrElse("unknown")}"))
        case Right(ReplProtocol.Line.Reply(r))     => settle(Status.Completed(r))
        case Right(hello: ReplProtocol.Line.Hello) => helloInfo = Some(hello)
        case Right(ReplProtocol.Line.Received)     => ackAt = Some(js.Date.now())
        case Right(_: ReplProtocol.Line.Tick)      => lastSignalAt = Some(js.Date.now())
        case Right(ReplProtocol.Line.Progress(p))  => progressed(p)
        case Right(_: ReplProtocol.Line.Unknown)   => ()
        case Left(err)                             => settle(Status.Failed(err))

  /** A progress line counts twice over: for the stage it names, and for the
    * bare fact that it arrived — the same proof of a turning event loop a tick
    * is. Folding it into the freshness ticks feed is what keeps a worker that
    * reports phases without ticking from ever being called blocked. */
  private def progressed(phase: String): Unit =
    val named = displayPhase(phase)
    // Only a change of stage moves the stage clock. A worker that repeats the
    // stage it is already in has not started anything, and a clock restarted on
    // every mention would measure how talkative the worker is.
    if named != lastPhase then
      lastPhase = named
      phaseChangedAt = named.map(_ => js.Date.now())
    lastSignalAt = Some(js.Date.now())

  private def kindOf(classified: Either[String, ReplProtocol.Line]): String = classified match
    case Right(_: ReplProtocol.Line.Reply)    => "reply"
    case Right(_: ReplProtocol.Line.Hello)    => "hello"
    case Right(ReplProtocol.Line.Received)    => "received"
    case Right(_: ReplProtocol.Line.Tick)     => "tick"
    case Right(_: ReplProtocol.Line.Progress) => "progress"
    case Right(_: ReplProtocol.Line.Unknown)  => "unknown"
    case Left(_)                              => "garbage"

  private def handleExit(gen: Int, log: WorkerLog, code: Int): Unit =
    log.exit(code, stale = gen != generation)
    if gen == generation then
      // Both while the handle is still known, so the diagnosis can name the pid,
      // and before the settle clears the picture they are taken from.
      captureDiagnosis()
      logLivenessSnapshot()
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
        val settled = status
        currentLog.settled(statusKind(settled), statusDetail(settled))
        clearRequest()
        p.complete(Success(settled))
      case None => ()

  private def statusKind(status: Status): String = status match
    case _: Status.Completed => "completed"
    case _: Status.TimedOut  => "timed-out"
    case _: Status.Failed    => "failed"

  /** A one-line gloss for the log. The response itself is already there
    * verbatim as the `recv` line, so this never restates it. */
  private def statusDetail(status: Status): String = status match
    case Status.Completed(r) => s"${r.op} ${if r.ok then "ok" else "error"}"
    case Status.TimedOut(ms) => s"no response within ${ms}ms"
    case Status.Failed(reason) => reason

  private def startRequest(): Unit =
    requestSentAt = Some(js.Date.now())
    ackAt = None
    lastSignalAt = None
    lastPhase = None
    phaseChangedAt = None
    lastLivenessKind = None
    lastDiagnosis = None

  private def clearRequest(): Unit =
    requestSentAt = None
    ackAt = None
    lastSignalAt = None
    lastPhase = None
    phaseChangedAt = None
    lastLivenessKind = None

  // -- request/response --------------------------------------------------------

  private def request(handle: LineProcess.Handle, json: String, timeoutMs: Option[Int])(using
      Async
  ): Status =
    val p = Future.Promise[Status]()
    inFlight = Some(p)
    startRequest()
    if !handle.writeLine(json) then
      inFlight = None
      clearRequest()
      val failed = Status.Failed("REPL worker is not accepting input")
      currentLog.settled(statusKind(failed), statusDetail(failed))
      failed
    else
      currentLog.sent(json)
      // Only an explicit timeout arms a kill timer; without one the eval runs
      // until the worker answers or the await is cancelled (a user interrupt).
      val timer =
        timeoutMs.map: ms =>
          timers.setTimeout(ms.toDouble):
            captureDiagnosis()
            logLivenessSnapshot()
            settle(Status.TimedOut(ms))
            // Mid-eval, only a kill stops the worker; the session state is lost.
            everDied = true
            worker = None
            currentLog.killed("timeout")
            handle.kill()
      try p.asFuture.await
      finally
        timer.foreach(timers.clearTimeout)
        if inFlight.contains(p) then
          // Cancelled mid-await: the eventual response can no longer be paired
          // with a request, so the session is unusable — kill it. No caller is
          // left to hand a diagnosis to, so here the log is the only record.
          logLivenessSnapshot()
          inFlight = None
          clearRequest()
          everDied = true
          worker = None
          currentLog.killed("cancel")
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
    * session died, so definitions accumulated before it are gone.
    *
    * `diagnosis`: a sentence about what the worker was doing when the request
    * died, for a call that ended by timeout or with the worker's exit. Present
    * only when the worker spoke the liveness protocol — there is nothing
    * honest to say about a legacy worker — and never for a call that
    * completed. */
  final case class EvalResult(
      status: Status,
      restartedSession: Boolean,
      diagnosis: Option[String] = None
  )

  /** What a worker is observably doing about the request it was given. Every
    * case is a statement about signals received, not about progress made — and
    * the two signals mean nearly opposite things. Ticks fire only while the
    * worker's event loop is IDLE, so a steady heartbeat means it is waiting on
    * something, not computing; healthy synchronous work (a compile, an
    * interpreter run) never yields the loop and is therefore silent between
    * progress lines. Neither state is a verdict on the worker's health. */
  enum Liveness:
    /** The request went out and the worker has not said it took it. */
    case AwaitingAck(elapsedMs: Long)

    /** The worker took the request and its signals are current. */
    case Working(elapsedMs: Long)

    /** The worker took the request and has since gone quiet: its event loop is
      * not turning — occupied or stuck, which from out here look identical. A
      * healthy long computation reaches this state and stays in it, so nothing
      * above may treat it as proof that the worker is dead. */
    case Blocked(elapsedMs: Long, silentForMs: Long)

  /** Silence worth this many heartbeats is called blocked. More than one missed
    * tick, so ordinary scheduling jitter alone never gets a worker called
    * blocked; few enough that a loop stuck for good is named while the user is
    * still watching. */
  val BlockedAfterMissedTicks = 3

  /** How much of a worker's phase token is kept. Long enough for any stage name
    * worth reading, short enough that a worker cannot push anything else off a
    * one-line label — the tokens are the worker's free choice, so their length
    * is not something this parent can otherwise rely on. */
  val MaxPhaseChars = 40

  /** Metadata keys under which a tool result carries the liveness picture. */
  val MetaState = "replState"
  val MetaSilentMs = "replSilentMs"
  val MetaElapsedMs = "replElapsedMs"
  val MetaWorkerPid = "workerPid"
  val MetaLiveness = "liveness"

  /** The pipeline stage the worker last named, when it named one. */
  val MetaPhase = "replPhase"

  /** How long that stage has been the current one — a clock of its own, started
    * when the stage was entered rather than when the request was sent. */
  val MetaPhaseMs = "replPhaseMs"

  private val ShutdownGraceMs = 1_000
  private val StderrTailBytes = 4_000

  /** The preamble compiles right after worker boot, which is when the JS-hosted
    * compiler is at its slowest — give it generous room. */
  private val PreambleTimeoutMs = 60_000
