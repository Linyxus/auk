package auk.runtime

import scala.collection.mutable
import scala.scalajs.js

import gears.async.{Async, Future, UnboundedChannel}

import auk.llm.tools.{Json, RuntimeContext}
import auk.loop.{Budgets, LoopEvent, LoopStatus, LoopStore, ParkReason}
import auk.platform.js.{Interop, SocketServer}
import auk.runtime.repl.{ReplProtocol, ScalaRepl}
import auk.session.SessionRef
import auk.snapshot.{GitError, Snapshot}

/** The host side of the refinement-loop bridge: a Unix-domain-socket server the
  * lead's REPL worker connects to (`auk.library.LoopClient`), plus the machinery that
  * turns a `lib.loop.start` call into a durable loop.
  *
  * Creating a loop is two-part, because half of a loop definition is a Scala closure
  * that cannot cross a socket. The eval that calls `loop.start` sends its whole
  * configuration here (`hello`) and prints an in-band marker; when that eval COMPLETES,
  * `eval_scala` hands over its entire source ([[announceDef]]). This bridge then
  * re-evaluates that source in a private "gate" worker spawned in attach mode
  * (`AUK_LOOP_ATTACH`), where `loop.start` binds the checker instead of creating a
  * second loop and acknowledges it with `bound`. Only then does anything reach disk.
  *
  * That round trip is the validation: a definition that leans on values defined in
  * other evals compiles in the session that wrote it and fails here, which is exactly
  * the mistake worth catching before a loop starts spending generations. A failure
  * discards the pending loop, closes the gate worker, and tells the lead what broke —
  * nothing is persisted, so there is no half-loop to clean up.
  *
  * Only ONE loop may be active at a time, per session and per project: a loop drives
  * generations against the live working tree, and two of them would be editing and
  * snapshotting the same files. A loop recorded as running by a session that is gone is
  * refused rather than adopted; picking one back up is the resume path (P5).
  */
final class LoopBridge(
    val socketPath: String,
    makeRepl: Map[String, String] => ScalaRepl,
    context: RuntimeContext,
    notifyLead: String => Unit,
    onNotice: String => Unit = _ => (),
    sessionRef: Option[SessionRef] = None
):
  import LoopBridge.*

  private val incoming = UnboundedChannel[(SocketServer.Conn, Json)]()
  private var server: SocketServer.Handle | Null = null
  // Every live connection: status snapshots are broadcast, since a loop's phase is
  // worth knowing to any worker that asks about it, and only the lead ever connects.
  private val conns = mutable.Set.empty[SocketServer.Conn]
  // Loops this bridge knows about, in creation order.
  private val loops = mutable.LinkedHashMap.empty[String, LoopEntry]
  // Ids whose `hello` was refused. Kept so [[announceDef]] can tell "refused" from
  // "the hello has not been dispatched yet" without waiting out its whole deadline.
  private val refused = mutable.Set.empty[String]

  /** Per-loop host state. Mutated only on the single JS event loop, so no locking. */
  private final class LoopEntry(val id: String, val config: LoopConfig):
    var phase: String = Validating
    /** The gate worker holding this loop's checker; retained for as long as the loop
      * lives, since every later check runs in it. */
    var repl: ScalaRepl | Null = null
    /** Set when the attach-mode session acknowledges that it bound the checker. */
    var bound: Boolean = false

  private def store: LoopStore = LoopStore.in(context)

  private def sessionId: String = sessionRef.map(_.id).getOrElse("")

  /** Bind the socket and start servicing clients. Spawns the dispatch fiber in the
    * caller's scope; returns immediately. */
  def start(onReady: () => Unit = () => ())(using Async.Spawn): Unit =
    server = SocketServer.listen(socketPath, onReady): conn =>
      conns += conn
      conn.onClose(() => { conns -= conn; () })
      conn.onLine: line =>
        Json.parse(line) match
          case Right(msg) => incoming.sendImmediately((conn, msg))
          case Left(_)    => ()
    Future:
      var running = true
      while running do
        incoming.read() match
          case Right((conn, msg)) => dispatch(conn, msg)
          case Left(_)            => running = false

  private def dispatch(conn: SocketServer.Conn, msg: Json): Unit =
    field(msg, "t") match
      case Some("hello")  => handleHello(conn, msg)
      case Some("bound")  => handleBound(str(msg, "loopId"))
      case Some("park")   => handlePark(conn, str(msg, "loopId"))
      case Some("resume") => handleResume(conn, str(msg, "loopId"))
      case _              => ()

  // -- creating a loop ----------------------------------------------------------

  /** A new loop's configuration. Accepted only as PENDING: nothing is written until
    * the definition that goes with it has been captured and validated. */
  private def handleHello(conn: SocketServer.Conn, msg: Json): Unit =
    val loopId = str(msg, "loopId")
    refusal(loopId, msg) match
      case Some(reason) =>
        refused += loopId
        reject(conn, loopId, reason)
        notifyLead(rejectionNotice(loopId, reason))
      case None =>
        // An id refused earlier (while another loop held the slot) is fair game again.
        refused -= loopId
        loops(loopId) = new LoopEntry(loopId, configOf(msg))
        broadcastStatus()
        onNotice(validatingNotice(loopId))

  /** Why this loop may not be started, or `None`. Ordered so the most specific answer
    * wins: what is wrong with the request, then what is already running here, then what
    * the project's own `.auk/loops` says. */
  private def refusal(loopId: String, msg: Json): Option[String] =
    LoopStore
      .validateId(loopId)
      .left
      .toOption
      .orElse:
        if bool(msg, "checkerRegistered") then None
        else Some("the loop definition registered no checker")
      .orElse:
        loops.valuesIterator
          .find(e => e.phase == Validating || e.phase == Running)
          .map(active => s"loop '${active.id}' is already active; park it before starting another")
      .orElse:
        runningOnDisk().map(id => s"loop '$id' is recorded as running; resume or park it first")
      .orElse:
        if store.list().contains(loopId) then
          Some(s"a loop with id '$loopId' already exists in this project; choose a different id")
        else None

  /** The first loop on disk whose ledger folds to a running state. A ledger that
    * cannot be folded is not evidence of anything, so it is skipped rather than
    * treated as a blocker. */
  private def runningOnDisk(): Option[String] =
    store.list().find(id => store.state(id).exists(_.status == LoopStatus.Running))

  private def handleBound(loopId: String): Unit =
    loops.get(loopId).foreach(_.bound = true)

  /** Validate and persist the definition that started `loopId`, given the ENTIRE
    * source of the eval that called `loop.start` (see the class doc for why the source
    * arrives separately, and after the fact).
    *
    * Blocking by design: it spawns a worker and compiles the definition in it, and the
    * `eval_scala` call that started the loop waits for the verdict, so a definition
    * that does not validate is reported while the model is still looking at it. A loop
    * id this bridge is not expecting — one whose `hello` was refused — is ignored. */
  def announceDef(loopId: String, source: String)(using Async): Unit =
    awaitPending(loopId) match
      case Some(entry) =>
        validate(entry, source) match
          case Right(())   => persist(entry, source)
          case Left(error) => discard(entry, error)
      case None => ()

  /** The pending entry for `loopId`, waiting for it if it has not been dispatched yet.
    *
    * The worker writes its `hello` to the socket before the eval it belongs to
    * finishes, but the two travel over different channels — a socket and the worker's
    * stdout pipe — so the eval's reply can reach the host first, with the `hello` still
    * sitting in the accept queue. Polling gives the event loop the turn it needs to
    * deliver it. Only an id this bridge has heard NOTHING about is worth waiting for: a
    * refusal, or a loop that is already past validating, is answered at once. */
  private def awaitPending(loopId: String)(using Async): Option[LoopEntry] =
    var waited = 0
    def unheard: Boolean = !loops.contains(loopId) && !refused.contains(loopId)
    while unheard && waited < HelloTimeoutMs do
      Interop.sleep(HelloPollMs.toDouble)
      waited += HelloPollMs
    loops.get(loopId).filter(e => e.phase == Validating && e.repl == null)

  /** Re-evaluate the captured source in a fresh attach-mode worker. `Right` means it
    * compiled, ran, and bound its checker there. */
  private def validate(entry: LoopEntry, source: String)(using Async): Either[String, Unit] =
    val repl = makeRepl(Map("AUK_LOOP_SOCK" -> socketPath, "AUK_LOOP_ATTACH" -> entry.id))
    entry.repl = repl
    repl.eval(source, Some(GateTimeoutMs)).status match
      case ScalaRepl.Status.Completed(r) if r.ok =>
        if awaitBound(entry) then Right(())
        else Left(s"the definition ran but never bound a checker for loop '${entry.id}'")
      case ScalaRepl.Status.Completed(r) =>
        val detail = if r.output.nonEmpty then r.output else r.error.getOrElse("the definition failed to evaluate")
        Left(ReplProtocol.stripAnsi(detail).trim)
      case ScalaRepl.Status.TimedOut(ms) =>
        Left(s"the definition did not finish evaluating within ${ms}ms")
      case ScalaRepl.Status.Failed(reason) =>
        Left(reason)

  /** Wait for the attach-mode session's `bound`. It is written to the socket before
    * the eval's own reply comes back, but over a different channel, so the two can
    * land out of order; polling the event loop lets the dispatch fiber deliver it. */
  private def awaitBound(entry: LoopEntry)(using Async): Boolean =
    var waited = 0
    while !entry.bound && waited < BoundTimeoutMs do
      Interop.sleep(BoundPollMs.toDouble)
      waited += BoundPollMs
    entry.bound

  /** Record the baseline and write the loop's first two events. The baseline snapshot
    * is taken here, after validation, so a definition that never becomes a loop leaves
    * no ref behind. Its `parent` is HEAD as it stood — empty for an unborn HEAD — which
    * is exactly `headAtCreation`, and saves asking git the same question twice. */
  private def persist(entry: LoopEntry, source: String)(using Async): Unit =
    val at = now()
    val created =
      for
        snap <- Snapshot
          .create(context.workingDirectory, baselineId(entry.id))
          .left
          .map(e => s"could not record the loop's baseline: ${describe(e)}")
        _ <- store.append(
          entry.id,
          LoopEvent.LoopCreated(entry.id, snap.commit, snap.parent.getOrElse(""), sessionId, at)
        )
        _ <- store.append(
          entry.id,
          LoopEvent.DefAttached(
            version = 1,
            source = source,
            goal = entry.config.goal,
            rubric = entry.config.rubric,
            budgets = entry.config.budgets,
            artifactSchema = entry.config.artifactSchema,
            at = at
          )
        )
      yield snap.commit
    created match
      case Right(commit) =>
        entry.phase = Running
        broadcastStatus()
        notifyLead(startedNotice(entry.id, commit))
        onNotice(runningNotice(entry.id, commit))
      case Left(error) => discard(entry, error)

  /** Drop a loop that never became one: the pending entry goes, its gate worker is shut
    * down, and both the client and the lead are told why. Nothing was persisted, so
    * there is nothing to undo. */
  private def discard(entry: LoopEntry, error: String)(using Async): Unit =
    loops.remove(entry.id)
    val repl = entry.repl
    entry.repl = null
    broadcastStatus()
    conns.foreach(_.write(errorMsg(entry.id, error).render))
    notifyLead(validationFailedNotice(entry.id, error))
    onNotice(failedNotice(entry.id))
    if repl != null then repl.close()

  // -- park / resume -------------------------------------------------------------

  private def handlePark(conn: SocketServer.Conn, loopId: String): Unit =
    loops.get(loopId) match
      case Some(entry) if entry.phase == Running =>
        // A park mid-generation is legal: the generation in flight is allowed to
        // finish, and only NEW work needs a resume first.
        store.append(loopId, LoopEvent.Parked(ParkReason.UserRequested, now())) match
          case Right(_) =>
            entry.phase = phaseFor(ParkReason.UserRequested)
            broadcastStatus()
            notifyLead(parkedNotice(loopId))
          case Left(error) => reject(conn, loopId, error)
      case Some(entry) => reject(conn, loopId, s"loop '$loopId' is not running (it is ${entry.phase})")
      case None        => reject(conn, loopId, unknownLoop(loopId))

  private def handleResume(conn: SocketServer.Conn, loopId: String): Unit =
    loops.get(loopId) match
      case Some(entry) if entry.phase.startsWith(ParkedPrefix) =>
        store.append(loopId, LoopEvent.Resumed(sessionId, now())) match
          case Right(_) =>
            entry.phase = Running
            broadcastStatus()
            notifyLead(resumedNotice(loopId))
          case Left(error) => reject(conn, loopId, error)
      case Some(entry) => reject(conn, loopId, s"loop '$loopId' is not parked (it is ${entry.phase})")
      case None        => reject(conn, loopId, unknownLoop(loopId))

  // -- wire out --------------------------------------------------------------------

  private def reject(conn: SocketServer.Conn, loopId: String, reason: String): Unit =
    conn.write(errorMsg(loopId, reason).render)

  /** Push a full snapshot of every loop's phase to every connected worker, after any
    * change. Cheap (a handful of loops) and idempotent, so nothing has to track which
    * client last saw what. */
  private def broadcastStatus(): Unit =
    val line = statusMsg(loops.valuesIterator.map(e => (e.id, e.phase)).toList).render
    conns.foreach(_.write(line))

  /** The phase this bridge currently reports for `loopId`. */
  def statusOf(loopId: String): Option[String] = loops.get(loopId).map(_.phase)

  def close()(using Async): Unit =
    loops.valuesIterator.foreach: entry =>
      val repl = entry.repl
      entry.repl = null
      if repl != null then repl.close()
    if server != null then server.nn.close()

  // -- message parsing ---------------------------------------------------------------

  private def configOf(msg: Json): LoopConfig =
    LoopConfig(
      goal = str(msg, "goal"),
      rubric = str(msg, "rubric"),
      budgets = budgetsOf(msg),
      // The schema travels as JSON text; an unparseable one is recorded as JSON null
      // rather than failing the loop, since it is descriptive, not load-bearing.
      artifactSchema = Json.parse(str(msg, "artifactSchema")).getOrElse(Json.Null)
    )

  private def budgetsOf(msg: Json): Budgets =
    val defaults = Budgets()
    obj(msg, "budgets") match
      case None => defaults
      case Some(b) =>
        Budgets(
          maxGenerations = int(b, "maxGenerations").getOrElse(defaults.maxGenerations),
          patience = int(b, "patience").getOrElse(defaults.patience),
          maxAttemptsPerGeneration = int(b, "maxAttemptsPerGeneration").getOrElse(defaults.maxAttemptsPerGeneration)
        )

object LoopBridge:
  /** One loop's configuration as it arrived from the worker, before it is a loop. */
  private[runtime] final case class LoopConfig(goal: String, rubric: String, budgets: Budgets, artifactSchema: Json)

  /** The definition has been submitted and is being checked; no loop exists yet. */
  val Validating: String = "validating"

  /** The loop exists and is live. */
  val Running: String = "running"

  /** Every parked phase starts with this. */
  val ParkedPrefix: String = "parked: "

  /** How long the gate worker gets to compile and run a definition. Generous: a cold
    * worker pays REPL startup plus a full compilation of the eval. */
  private val GateTimeoutMs = 120_000

  /** How long to keep giving the event loop a chance to deliver the `bound`
    * acknowledgement after the gate eval has answered. */
  private val BoundTimeoutMs = 5_000
  private val BoundPollMs = 10

  /** The same, for the `hello` that precedes a captured definition. */
  private val HelloTimeoutMs = 5_000
  private val HelloPollMs = 5

  /** A per-process temp socket path for the bridge (the server unlinks any stale file
    * on bind). */
  def defaultSocketPath(): String =
    val os = js.Dynamic.global.require("node:os")
    val pid = js.Dynamic.global.process.pid
    s"${os.tmpdir().asInstanceOf[String]}/auk-loop-$pid.sock"

  /** The snapshot id holding the tree a loop measures itself against. Namespaced under
    * the loop so everything it ever records is reachable from one prefix. */
  def baselineId(loopId: String): String = s"loop/$loopId/baseline"

  /** How a park reason reads as a phase string. */
  def phaseFor(reason: ParkReason): String = reason match
    case ParkReason.GoalReached       => s"${ParkedPrefix}goal reached"
    case ParkReason.BudgetExhausted   => s"${ParkedPrefix}budget exhausted"
    case ParkReason.PatienceExhausted => s"${ParkedPrefix}patience exhausted"
    case ParkReason.UserRequested     => s"${ParkedPrefix}user requested"
    case ParkReason.ApiFailure(d)     => s"${ParkedPrefix}api failure: $d"
    case ParkReason.Anomaly(d)        => s"${ParkedPrefix}anomaly: $d"

  // -- notices ------------------------------------------------------------------------

  /** The system notice the lead receives once a loop's definition has validated and the
    * loop is live. */
  def startedNotice(loopId: String, baselineCommit: String): String =
    s"Loop '$loopId' validated and is now running; its baseline is commit ${shortCommit(baselineCommit)}."

  /** The system notice the lead receives when a definition does NOT validate. It names
    * the constraint that is almost always the cause, because the eval that failed here
    * is one that succeeded where the model wrote it. */
  def validationFailedNotice(loopId: String, error: String): String =
    s"Loop '$loopId' was NOT created: its definition did not validate when re-evaluated on its own.\n$error\n" +
      "The eval that calls lib.loop.start is captured whole and re-run in a fresh session, so it must be " +
      "self-contained: it can use `lib` and whatever it defines itself, but nothing from other evals. " +
      "Fix it and start the loop again."

  /** The system notice the lead receives when the host refuses to start a loop. */
  def rejectionNotice(loopId: String, reason: String): String =
    s"[loop] rejected starting loop '$loopId': $reason"

  def parkedNotice(loopId: String): String =
    s"Loop '$loopId' is parked. It keeps its whole history; resume it with lib.loop.get(\"$loopId\").resume()."

  def resumedNotice(loopId: String): String =
    s"Loop '$loopId' is running again."

  // User-facing chatter: the model is already waiting on its eval, but the user is
  // watching a spinner and deserves to know what it is spinning on.
  private[runtime] def validatingNotice(loopId: String): String =
    s"[loop] validating the definition of '$loopId' in a fresh session…"

  private[runtime] def runningNotice(loopId: String, baselineCommit: String): String =
    s"[loop] '$loopId' is running (baseline ${shortCommit(baselineCommit)})"

  private[runtime] def failedNotice(loopId: String): String =
    s"[loop] '$loopId' did not validate; no loop was created"

  private def unknownLoop(loopId: String): String =
    s"unknown loop '$loopId'"

  private def shortCommit(commit: String): String =
    if commit.length > 12 then commit.take(12) else commit

  /** ISO-8601, stamped here: the ledger layer is pure and never reads a clock. */
  private def now(): String = new js.Date().toISOString()

  /** A [[GitError]] as one line of prose. The snapshot module is deliberately free of
    * presentation, so its consumers render it. */
  private def describe(e: GitError): String = e match
    case GitError.NotARepository(dir)  => s"'$dir' is not a git repository"
    case GitError.InvalidId(id, why)   => s"'$id' is not a usable snapshot id: $why"
    case GitError.SnapshotExists(id)   => s"a snapshot '$id' already exists"
    case GitError.SnapshotNotFound(id) => s"there is no snapshot '$id'"
    case GitError.CommandFailed(args, code, stderr) =>
      s"git ${args.mkString(" ")} failed ($code): ${stderr.trim}"

  // -- message accessors ---------------------------------------------------------------

  private def field(j: Json, k: String): Option[String] = j match
    case o: Json.Obj => o.get(k).collect { case Json.Str(s) => s }
    case _           => None
  private def str(j: Json, k: String): String = field(j, k).getOrElse("")
  private def bool(j: Json, k: String): Boolean = j match
    case o: Json.Obj => o.get(k).collect { case Json.Bool(b) => b }.getOrElse(false)
    case _           => false
  private def obj(j: Json, k: String): Option[Json.Obj] = j match
    case o: Json.Obj => o.get(k).collect { case nested: Json.Obj => nested }
    case _           => None
  private def int(j: Json, k: String): Option[Int] = j match
    case o: Json.Obj => o.get(k).collect { case Json.Num(n) => n.toInt }
    case _           => None

  private def errorMsg(loopId: String, msg: String): Json =
    Json.Obj(List("t" -> Json.Str("error"), "loopId" -> Json.Str(loopId), "msg" -> Json.Str(msg)))

  private def statusMsg(phases: List[(String, String)]): Json =
    val entries = phases.map((id, phase) => Json.Obj(List("id" -> Json.Str(id), "phase" -> Json.Str(phase))))
    Json.Obj(List("t" -> Json.Str("status"), "loops" -> Json.Arr(entries)))
