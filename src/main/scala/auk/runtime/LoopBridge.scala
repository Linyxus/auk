package auk.runtime

import scala.collection.mutable
import scala.scalajs.js

import gears.async.{Async, CancellationException, Future, UnboundedChannel}

import auk.agent.{LoopGenerationState, LoopGenerationView, LoopView}
import auk.llm.endpoint.{Endpoint, Message}
import auk.llm.provider.ModelSession
import auk.llm.tools.{Json, RuntimeContext, Tool, ToolInput, ToolResult}
import auk.loop.{Budgets, GenerationRecord, LoopEvent, LoopState, LoopStatus, LoopStore, ParkReason}
import auk.platform.js.{Interop, SocketServer}
import auk.runtime.repl.{ReplProtocol, ScalaRepl}
import auk.session.{JsonlLog, SessionProvider, SessionRef}
import auk.snapshot.{GitError, ResetError, Snapshot, Worktree}
import auk.workflow.{TranscriptEvent, WireCodec, WireMessage}

/** The host side of the refinement-loop bridge: a Unix-domain-socket server the
  * lead's REPL worker connects to (`auk.library.LoopClient`), the machinery that turns
  * a `lib.loop.start` call into a durable loop, and the driver that then spends the
  * loop's budget on it generation by generation.
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
  * Once a loop is live the host drives it, one generation at a time, with no further
  * involvement from the lead ([[drive]]): a fresh worker agent improves the working
  * tree and submits a typed artifact, the definition's checker runs in the gate worker
  * as the mechanical gate, an evaluator agent judges what passes, and the accepted
  * attempt's snapshot becomes the tree the next generation starts from. Every step is
  * appended to the loop's ledger before it is acted on, so a loop that is interrupted
  * — by a park, an API outage, or the session ending — is picked up from the ledger
  * rather than from anything held in memory here.
  *
  * A generation's worker is the one loop agent that may delegate: it is spawned with
  * the host's workflow and team sockets ([[workerEnvFor]]), so a generation can fan a
  * question out to sub-agents or hire a member to hold a thread of reasoning. Both are
  * bounded by the generation. A workflow run dies with the worker's REPL, and every
  * team member the worker created is retired when the generation settles, however it
  * settles — a member reasoning about a tree that has just been rolled back is worse
  * than no member at all. What a worker may NOT do is start a loop: it has no loop
  * socket, and the library says so in those words.
  *
  * Only ONE loop may be active at a time, per session and per project: a loop drives
  * generations against the live working tree, and two of them would be editing and
  * snapshotting the same files.
  *
  * A loop outlives the session that started it, so this bridge also picks up loops it
  * did not create ([[adoptResume]]): a `resume` or `park` naming an id it has never
  * heard of is answered from `.auk/loops` if the ledger there folds. Resuming one is
  * the same validation a creation gets — the ledger's own definition source is
  * re-evaluated in a fresh gate worker — because the code around a stored definition
  * may have moved since it was written, and a loop whose artifact type no longer
  * matches its lineage must be refused rather than run.
  */
final class LoopBridge(
    val socketPath: String,
    models: ModelSession,
    makeRepl: Map[String, String] => ScalaRepl,
    /** The tools every loop agent gets, on top of its own submit tool. Given the
      * agent's own REPL, exactly as the workflow and team bridges take them. */
    baseTools: ScalaRepl => List[Tool],
    /** The base system prompt for a loop's worker and evaluator agents; the loop's own
      * section ([[LoopBridge.workerSection]] / [[LoopBridge.evaluatorSection]]) is
      * appended to it. */
    workerSystemPrompt: String,
    context: RuntimeContext,
    notifyLead: String => Unit,
    /** The user's sticky notice area: a line pinned above the input box for the rest of
      * the session. Reserved for the rare warning that has no other surface — where a
      * loop stands is the panel's job, and what it does is the lead's. */
    onNotice: String => Unit = _ => (),
    /** The TUI's loop panel: a full snapshot of every loop this session can act on —
      * the ones it drives and the ones its `.auk/loops` holds — pushed on every phase
      * change and on every stage of the drive cycle. Snapshots rather than deltas,
      * exactly as the team bridge pushes its roster. */
    onLoop: Vector[LoopView] => Unit = _ => (),
    /** Every loop agent's transcript, as it streams: the same events this bridge
      * already tees to the session's JSONL, keyed by loop id and the agent's label
      * (`gen-3-worker`, `gen-3-eval`) so the panel's transcript overlay can follow a
      * generation live. */
    onActivity: (String, TranscriptEvent) => Unit = (_, _) => (),
    /** The environment a GENERATION WORKER is spawned with, on top of the tags this
      * bridge adds per generation ([[workerEnvFor]]): the host's workflow and team
      * sockets, so a generation can delegate the way the lead can. Deliberately not
      * the loop socket — see [[workerEnvFor]] — and deliberately not given to the
      * gate or the evaluator, neither of which delegates anything. */
    workerEnv: Map[String, String] = Map.empty,
    /** Retire every team member created for an owner tag, answering with the ids
      * that were retired. Wired to the team bridge by the host; the default is the
      * honest answer for a session with no team bridge at all. Named as a function
      * rather than taken as a bridge so the two stay strangers. */
    retireTeamOwned: Async ?=> String => List[String] = _ => Nil,
    sessionRef: Option[SessionRef] = None,
    maxResultRetries: Int = WorkflowBridge.MaxResultRetries,
    /** Each loop agent's API retry schedule (tests shrink it). */
    retryDelaysMs: List[Long] = Endpoint.RetryDelaysMs
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
  // The gate evaluation currently in flight for each loop, if any: a creation being
  // validated, an amendment, or a stored definition being re-checked on adoption. It
  // is where the attach-mode session's `bound` lands, and only one can be open per
  // loop, since all three start by taking the loop's single active slot.
  private val binds = mutable.Map.empty[String, Bind]
  // The scope a drive fiber is spawned into: the one `start` was called in, which is
  // the host's own lifetime. A generation outlives the socket message that begat it
  // (a resume) and the eval that begat it (a creation), so neither can own it.
  private var scope: Async.Spawn | Null = null
  // Set by `close`, so a drive fiber stops between steps instead of being torn down
  // mid-generation by scope cancellation.
  private var closed = false

  /** Per-loop host state. Mutated only on the single JS event loop, so no locking. */
  private final class LoopEntry(val id: String, val config: LoopConfig):
    var phase: String = Validating
    /** The gate worker holding this loop's checker; retained for as long as the loop
      * lives, since every later check runs in it. */
    var repl: ScalaRepl | Null = null
    /** An amendment's gate worker, validated and waiting to take over. The swap is
      * deferred to a generation boundary because [[runCheck]] holds the live gate for
      * the length of a check: replacing it under a check in flight would judge the
      * candidate by a checker the generation never ran under, and closing the old one
      * would kill the check outright. */
    var pendingGate: ScalaRepl | Null = null
    /** Whether a drive fiber is live for this loop. The one guard that keeps a resume
      * (or a redundant one) from putting two drivers on the same working tree. */
    var driving: Boolean = false
    /** Where the drive cycle stands inside the generation in flight, for the panel.
      * The ledger says which generation is open but not which of worker, checker and
      * evaluator is running right now, and that is exactly what a reader watching a
      * loop wants to know. `None` between generations. */
    var stage: Option[Stage] = None

  /** One gate evaluation in flight. The attach-mode session answers with `bound`, which
    * carries the configuration the definition declares — for an amendment that is the
    * only place the host learns it, and on adoption it is the schema as the definition
    * compiles TODAY, which is what makes drift detectable. */
  private final class Bind:
    var bound: Boolean = false
    var reported: Option[LoopConfig] = None

  private def store: LoopStore = LoopStore.in(context)

  private def sessionId: String = sessionRef.map(_.id).getOrElse("")

  /** Bind the socket and start servicing clients. Spawns the dispatch fiber in the
    * caller's scope; returns immediately. */
  def start(onReady: () => Unit = () => ())(using spawn: Async.Spawn): Unit =
    scope = spawn
    server = SocketServer.listen(socketPath, onReady): conn =>
      conns += conn
      // A client is told the state of the world as soon as it arrives, rather than at
      // the next thing that changes. It matters for exactly one case, and that case is
      // the point of a durable ledger: a session that starts with loops already on disk
      // changes nothing, so without this its lead would never hear that they exist.
      conn.write(statusMsg(phases()).render)
      conn.onClose(() => { conns -= conn; () })
      conn.onLine: line =>
        Json.parse(line) match
          case Right(msg) => incoming.sendImmediately((conn, msg))
          case Left(_)    => ()
    // And so is the UI, for the same reason and at the same moment: a session opening
    // on a project with loops already on disk has nothing to change, so without this
    // its panel would stay empty until something happened to one of them.
    emitLoops()
    Future:
      var running = true
      while running do
        incoming.read() match
          case Right((conn, msg)) => dispatch(conn, msg)
          case Left(_)            => running = false

  private def dispatch(conn: SocketServer.Conn, msg: Json)(using Async): Unit =
    field(msg, "t") match
      case Some("hello")       => handleHello(conn, msg)
      case Some("bound")       => handleBound(msg)
      case Some("reconfigure") => handleReconfigure(conn, msg)
      case Some("park")        => handlePark(conn, str(msg, "loopId"))
      case Some("resume")      => handleResume(conn, str(msg, "loopId"))
      case _                   => ()

  // -- creating a loop ----------------------------------------------------------

  /** A new loop's configuration. Accepted only as PENDING: nothing is written until
    * the definition that goes with it has been captured and validated. */
  private def handleHello(conn: SocketServer.Conn, msg: Json)(using Async): Unit =
    val loopId = str(msg, "loopId")
    reclaimPending(loopId)
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

  /** Take the single active slot back from a pending loop whose defining eval never
    * came back.
    *
    * A `hello` reserves the slot, and the definition that would fill it arrives only
    * when the eval COMPLETES — so an eval that is killed (a timeout, a REPL restart,
    * an interrupt) leaves a pending entry nothing will ever settle, and every later
    * loop is refused by a loop that does not exist. The lead retrying `start` is the
    * signal to let go of it: a second `hello` for a still-pending id discards the first
    * outright. There is no timer, because there is no honest timeout — a gate
    * compilation can take a minute and the eval that will settle it might be on its
    * way. A `hello` for a loop that reached [[persist]] is a different matter and stays
    * refused; that one really does exist. */
  private def reclaimPending(loopId: String)(using Async): Unit =
    loops.get(loopId).filter(_.phase == Validating).foreach: stale =>
      loops.remove(loopId)
      val repl = stale.repl
      stale.repl = null
      // Closing the gate makes an in-flight `validate` fail, which lands in [[discard]]
      // — where the entry is no longer current and so nothing is written, said, or
      // removed. The client is told nothing here either: an `error` line naming this id
      // would set it to `failed:` in the mirror, which is where the REPLACING loop is
      // about to appear.
      if repl != null then closeQuietly(repl)

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
        // Nothing is excluded here, unlike an adoption's [[occupied]]: a `hello` naming
        // a loop that is already live IS a collision with itself, and the loop it
        // collides with is the one worth naming.
        activeHere(except = "").map(id => s"loop '$id' is already active; park it before starting another")
      .orElse:
        runningOnDisk(except = "").map(id => s"loop '$id' is recorded as running; resume or park it first")
      .orElse:
        if store.list().contains(loopId) then
          Some(s"a loop with id '$loopId' already exists in this project; choose a different id")
        else None

  /** The loop holding this session's single active slot, if it is not `except`. Every
    * phase that is not parked counts: a loop being validated or adopted is on its way
    * to driving the working tree, which is the thing there is only one of. */
  private def activeHere(except: String): Option[String] =
    loops.valuesIterator
      .find(e => e.id != except && (e.phase == Validating || e.phase == Adopting || e.phase.startsWith(Running)))
      .map(_.id)

  /** The first loop on disk whose ledger folds to a running state. A ledger that
    * cannot be folded is not evidence of anything, so it is skipped rather than
    * treated as a blocker. */
  private def runningOnDisk(except: String): Option[String] =
    store.list().find(id => id != except && store.state(id).exists(_.status == LoopStatus.Running))

  private def handleBound(msg: Json): Unit =
    binds.get(str(msg, "loopId")).foreach: bind =>
      bind.bound = true
      bind.reported = Some(configOf(msg))

  /** Validate and persist the definition that started `loopId`, given the ENTIRE
    * source of the eval that called `loop.start` (see the class doc for why the source
    * arrives separately, and after the fact).
    *
    * Blocking by design: it spawns a worker and compiles the definition in it, and the
    * `eval_scala` call that started the loop waits for the verdict, so a definition
    * that does not validate is reported while the model is still looking at it. The
    * loop's first generation, by contrast, is NOT waited for — it starts on its own
    * fiber, so the eval returns as soon as the loop exists. A loop id this bridge is
    * not expecting — one whose `hello` was refused — is ignored. */
  def announceDef(loopId: String, source: String)(using Async): Unit =
    awaitPending(loopId) match
      case Some(entry) =>
        val repl = makeRepl(gateEnv(entry.id))
        entry.repl = repl
        validate(entry.id, repl, source) match
          case Right(_) =>
            // The pending entry may have been reclaimed while its gate compiled — by a
            // lead that gave up on this eval and started the loop again. The newer
            // attempt owns the id now, so this one just goes quietly.
            if isCurrent(entry) then persist(entry, source)
            else
              entry.repl = null
              closeQuietly(repl)
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

  /** The environment a gate worker is spawned with: this bridge's socket, and the loop
    * whose definition it exists to hold. */
  private def gateEnv(loopId: String): Map[String, String] =
    Map("AUK_LOOP_SOCK" -> socketPath, "AUK_LOOP_ATTACH" -> loopId)

  /** Evaluate a definition's source in `repl`, a fresh attach-mode worker. `Right` means
    * it compiled, ran, and bound its checker there, and carries the configuration that
    * source declares — which for an amendment is how the host learns it at all, and on
    * adoption is the definition's schema as it compiles TODAY.
    *
    * The caller owns `repl` either way: on success it becomes (or replaces) the loop's
    * gate, and on failure it is closed by whoever asked for the validation. */
  private def validate(loopId: String, repl: ScalaRepl, source: String)(using Async): Either[String, LoopConfig] =
    val bind = new Bind
    binds(loopId) = bind
    try
      repl.eval(source, Some(GateTimeoutMs)).status match
        case ScalaRepl.Status.Completed(r) if r.ok =>
          if awaitBound(bind) then Right(bind.reported.getOrElse(LoopConfig("", "", Budgets(), Json.Null)))
          else Left(s"the definition ran but never bound a checker for loop '$loopId'")
        case ScalaRepl.Status.Completed(r) =>
          val detail = if r.output.nonEmpty then r.output else r.error.getOrElse("the definition failed to evaluate")
          Left(ReplProtocol.stripAnsi(detail).trim)
        case ScalaRepl.Status.TimedOut(ms) =>
          Left(s"the definition did not finish evaluating within ${ms}ms")
        case ScalaRepl.Status.Failed(reason) =>
          Left(reason)
    // Only ever its own: a validation that was reclaimed mid-compile finishes after the
    // one that replaced it has already opened a bind of its own, and taking that one
    // down would leave the live gate's acknowledgement with nowhere to land.
    finally if binds.get(loopId).exists(_ eq bind) then binds.remove(loopId)

  /** Wait for the attach-mode session's `bound`. It is written to the socket before
    * the eval's own reply comes back, but over a different channel, so the two can
    * land out of order; polling the event loop lets the dispatch fiber deliver it. */
  private def awaitBound(bind: Bind)(using Async): Boolean =
    var waited = 0
    while !bind.bound && waited < BoundTimeoutMs do
      Interop.sleep(BoundPollMs.toDouble)
      waited += BoundPollMs
    bind.bound

  /** Whether `entry` is still the bridge's entry for its id — false once it has been
    * reclaimed by a later attempt at the same loop. */
  private def isCurrent(entry: LoopEntry): Boolean = loops.get(entry.id).exists(_ eq entry)

  /** Record the baseline, write the loop's first two events, and set it going. The
    * baseline snapshot is taken here, after validation, so a definition that never
    * becomes a loop leaves no ref behind. Its `parent` is HEAD as it stood — empty for
    * an unborn HEAD — which is exactly `headAtCreation`, and saves asking git the same
    * question twice. */
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
        launchDrive(entry)
      case Left(error) => discard(entry, error)

  /** Drop a loop that never became one: the pending entry goes, its gate worker is shut
    * down, and both the client and the lead are told why. Nothing was persisted, so
    * there is nothing to undo.
    *
    * An entry that is no longer current was reclaimed by a later `hello` for the same
    * id — which is what closed its gate and made this validation fail in the first
    * place. Nothing is said about it: the id belongs to the replacing attempt now, and
    * a failure notice naming it would be about a loop that is very much still alive. */
  private def discard(entry: LoopEntry, error: String)(using Async): Unit =
    val repl = entry.repl
    entry.repl = null
    if isCurrent(entry) then
      loops.remove(entry.id)
      broadcastStatus()
      conns.foreach(_.write(errorMsg(entry.id, error).render))
      notifyLead(validationFailedNotice(entry.id, error))
    if repl != null then closeQuietly(repl)

  // -- steering ---------------------------------------------------------------------

  /** Attach a NEW definition to a loop that already exists, given the whole source of
    * the eval that called `lib.loop.amend` (which travels exactly as a creation's does,
    * and for the same reason — see [[announceDef]]).
    *
    * The amendment is validated in its own fresh gate worker, so a definition that does
    * not compile costs the loop nothing: it keeps running, on the definition it has.
    * The one thing that cannot change is the ARTIFACT SCHEMA. A loop's generations are
    * judged against each other, and a lineage whose entries do not have the same shape
    * is not a lineage — so a schema that no longer matches the ledger's is refused,
    * loudly and without touching the loop.
    *
    * On acceptance the new version is appended at once (that is the durable record that
    * the amendment was accepted, and a park must not be able to lose it) while the gate
    * SWAP waits for a generation boundary — see [[LoopEntry.pendingGate]]. So the
    * generation in flight finishes under the checker it started with, and the new
    * configuration reaches its next prompt. */
  def announceAmend(loopId: String, source: String)(using Async): Unit =
    amendTarget(loopId) match
      case Left(reason) => refuseAmend(loopId, reason)
      case Right(entry) =>
        val repl = makeRepl(gateEnv(loopId))
        val outcome =
          for
            config <- validate(loopId, repl, source)
            state  <- store.state(loopId).left.map(e => s"the loop's ledger could not be read: $e")
            _ <-
              if LoopStore.schemasMatch(config.artifactSchema, state.artifactSchema) then Right(())
              else Left(driftDetail(state.artifactSchema, config.artifactSchema))
            _ <- store.append(
              loopId,
              LoopEvent.DefAttached(
                version = state.defVersion + 1,
                source = source,
                goal = config.goal,
                rubric = config.rubric,
                budgets = config.budgets,
                artifactSchema = config.artifactSchema,
                at = now()
              )
            )
          yield state.defVersion + 1
        outcome match
          case Right(version) =>
            stageGate(entry, repl)
            broadcastStatus()
            notifyLead(amendedNotice(loopId, version))
          case Left(reason) =>
            closeQuietly(repl)
            refuseAmend(loopId, reason)

  /** The loop an amendment may be attached to. A loop still being validated or adopted
    * has no definition to replace yet, and one that this session has not picked up is
    * not something to redefine from a distance — resuming or parking it is what brings
    * it here, and either is a decision worth making explicitly. */
  private def amendTarget(loopId: String): Either[String, LoopEntry] =
    loops.get(loopId) match
      case Some(entry) if entry.phase == Validating || entry.phase == Adopting =>
        Left(s"loop '$loopId' is still ${entry.phase}; wait for its definition to settle before amending it")
      case Some(entry) => Right(entry)
      case None if store.list().contains(loopId) =>
        Left(s"loop '$loopId' belongs to an earlier session; resume or park it here before amending it")
      case None => Left(unknownLoop(loopId))

  /** Put an amendment's validated gate worker in line to take over. A loop with no
    * driver takes it immediately; one that is driving picks it up at the next generation
    * boundary, or as its driver exits. */
  private def stageGate(entry: LoopEntry, repl: ScalaRepl)(using Async): Unit =
    entry.pendingGate = repl
    if !entry.driving then applyStagedGate(entry)

  /** Swap in a staged gate worker and close the one it replaces. */
  private def applyStagedGate(entry: LoopEntry)(using Async): Unit =
    val staged = entry.pendingGate
    if staged != null then
      entry.pendingGate = null
      val old = entry.repl
      entry.repl = staged
      if old != null then closeQuietly(old)

  private def refuseAmend(loopId: String, reason: String): Unit =
    conns.foreach(_.write(errorMsg(loopId, reason).render))
    notifyLead(amendRefusedNotice(loopId, reason))

  /** A data-only amendment: retune the goal, the rubric or the budgets of a loop that
    * already exists, without touching its checker. Only the fields the message names are
    * overlaid, and the fold is what makes that work — the loop's effective configuration
    * is read from the ledger every time a prompt is composed, so the next one is
    * composed from this. */
  private def handleReconfigure(conn: SocketServer.Conn, msg: Json): Unit =
    val loopId = str(msg, "loopId")
    val goal = field(msg, "goal")
    val rubric = field(msg, "rubric")
    val budgets = obj(msg, "budgets").map(_ => budgetsOf(msg))
    if goal.isEmpty && rubric.isEmpty && budgets.isEmpty then
      reject(conn, loopId, s"the reconfigure of loop '$loopId' named nothing to change")
    else
      reconfigureTarget(loopId) match
        case Left(reason) => reject(conn, loopId, reason)
        case Right(()) =>
          store.append(loopId, LoopEvent.ConfigAmended(goal, rubric, budgets, now())) match
            case Left(error) => reject(conn, loopId, error)
            case Right(_) =>
              broadcastStatus()
              notifyLead(reconfiguredNotice(loopId, goal, rubric, budgets))

  /** Whether `loopId` is a loop this project has and this session may retune. Unlike an
    * amendment this needs no gate worker — nothing is compiled and no checker changes —
    * so a loop from an earlier session is retuned where it lies. */
  private def reconfigureTarget(loopId: String): Either[String, Unit] =
    loops.get(loopId) match
      case Some(entry) if entry.phase == Validating || entry.phase == Adopting =>
        Left(s"loop '$loopId' is still ${entry.phase}; wait for its definition to settle before reconfiguring it")
      case Some(_) => Right(())
      case None =>
        if !store.list().contains(loopId) then Left(unknownLoop(loopId))
        else store.state(loopId).map(_ => ()).left.map(e => s"loop '$loopId' could not be read: $e")

  // -- park / resume -------------------------------------------------------------

  private def handlePark(conn: SocketServer.Conn, loopId: String): Unit =
    loops.get(loopId) match
      case Some(entry) if entry.phase.startsWith(Running) =>
        // A park mid-generation is legal: the generation in flight is allowed to
        // finish, and only NEW work needs a resume first. The driver notices between
        // generations, which is why nothing is cancelled here.
        store.append(loopId, LoopEvent.Parked(ParkReason.UserRequested, now())) match
          case Right(_) =>
            entry.phase = phaseFor(ParkReason.UserRequested)
            broadcastStatus()
            notifyLead(parkedNotice(loopId))
          case Left(error) => reject(conn, loopId, error)
      case Some(entry) => reject(conn, loopId, s"loop '$loopId' is not running (it is ${entry.phase})")
      case None        => parkOnDisk(conn, loopId)

  /** Park a loop this session never picked up. Nothing has to be compiled to stop a
    * loop — the checker only matters to a loop that is going to run — so this is just
    * the event, appended where the loop lies. It is the honest way to close out a loop
    * left running by a session that is gone: the ledger ends up saying it stopped
    * because someone said so, which is exactly what happened. */
  private def parkOnDisk(conn: SocketServer.Conn, loopId: String): Unit =
    if !store.list().contains(loopId) then reject(conn, loopId, unknownLoop(loopId))
    else
      store.state(loopId) match
        case Left(error) => reject(conn, loopId, s"loop '$loopId' could not be read: $error")
        case Right(state) =>
          state.parkedReason match
            case Some(reason) => reject(conn, loopId, s"loop '$loopId' is not running (it is ${phaseFor(reason)})")
            case None =>
              store.append(loopId, LoopEvent.Parked(ParkReason.UserRequested, now())) match
                case Left(error) => reject(conn, loopId, error)
                case Right(_) =>
                  broadcastStatus()
                  notifyLead(parkedNotice(loopId))

  private def handleResume(conn: SocketServer.Conn, loopId: String): Unit =
    loops.get(loopId) match
      case Some(entry) if entry.phase.startsWith(ParkedPrefix) =>
        store.append(loopId, LoopEvent.Resumed(sessionId, now())) match
          case Right(_) =>
            entry.phase = Running
            broadcastStatus()
            notifyLead(resumedNotice(loopId))
            launchDrive(entry)
          case Left(error) => reject(conn, loopId, error)
      case Some(entry) => reject(conn, loopId, s"loop '$loopId' is not parked (it is ${entry.phase})")
      case None        => adoptResume(conn, loopId)

  // -- adoption ----------------------------------------------------------------------

  /** Pick up a loop this bridge did not create: one left parked by an earlier session,
    * or one that session was still driving when it ended.
    *
    * The ledger is the whole handover — [[drive]] already takes every decision from it,
    * so a loop resumed here is in exactly the position one created here is. What has to
    * be re-established is the half that is NOT in the ledger: the checker, which is a
    * closure that died with the session that compiled it. So the stored definition is
    * re-evaluated in a fresh gate worker, and the schema that comes back is compared
    * with the one the ledger recorded. A definition whose artifact type has changed
    * underneath it — the case class it names has gained a field, say — still compiles
    * and would still check candidates, but against a shape the lineage does not have;
    * that is refused here rather than discovered halfway through a generation.
    *
    * The gate is spawned on the bridge's own fiber, not the dispatch fiber, because the
    * attach-mode session acknowledges over this very socket: waiting for it inline would
    * be waiting for a message only the waiting fiber can deliver. */
  private def adoptResume(conn: SocketServer.Conn, loopId: String): Unit =
    val spawn = scope
    if !store.list().contains(loopId) then reject(conn, loopId, unknownLoop(loopId))
    else if spawn == null || closed then reject(conn, loopId, s"loop '$loopId' cannot be resumed: the host is shutting down")
    else
      store.state(loopId) match
        case Left(error) => reject(conn, loopId, s"loop '$loopId' could not be read: $error")
        case Right(state) =>
          occupied(loopId) match
            case Some(reason) => reject(conn, loopId, reason)
            case None =>
              val entry = new LoopEntry(loopId, LoopConfig(state.goal, state.rubric, state.budgets, state.artifactSchema))
              entry.phase = Adopting
              loops(loopId) = entry
              broadcastStatus()
              // On a fiber of its own, and it must stay that way: the gate worker
              // acknowledges over THIS socket, and this is the fiber that delivers
              // socket messages — awaiting `bound` inline would be awaiting a message
              // only the awaiting fiber can hand over. Same single-dispatch-fiber shape
              // as the hello race [[awaitPending]] polls around.
              given Async.Spawn = spawn
              Future(finishAdoption(conn, entry, state))
              ()

  /** Who is holding the single active slot against an adoption, if anyone. */
  private def occupied(loopId: String): Option[String] =
    activeHere(except = loopId)
      .map(id => s"loop '$id' is already active; park it before resuming another")
      .orElse:
        runningOnDisk(except = loopId).map(id => s"loop '$id' is recorded as running; park it first")

  /** Re-validate an adopted loop's stored definition and, if it holds up, set it going.
    *
    * The gap is recorded before the resume when the ledger still says Running: a session
    * that ended mid-loop left no park behind, and writing one now — as an anomaly saying
    * so — is what keeps the ledger an honest account of a loop that stopped for a while.
    * [[drive]] then finds whatever generation was in flight and rescues it. */
  private def finishAdoption(conn: SocketServer.Conn, entry: LoopEntry, state: LoopState)(using Async): Unit =
    val repl = makeRepl(gateEnv(entry.id))
    val outcome =
      for
        config <- validate(entry.id, repl, state.defSource)
        _ <-
          if LoopStore.schemasMatch(config.artifactSchema, state.artifactSchema) then Right(())
          else Left(driftDetail(state.artifactSchema, config.artifactSchema))
      yield ()
    outcome match
      case Left(reason) =>
        loops.remove(entry.id)
        closeQuietly(repl)
        broadcastStatus()
        notifyLead(adoptionRefusedNotice(entry.id, reason))
        reject(conn, entry.id, reason)
      case Right(()) =>
        entry.repl = repl
        val fromDeadSession = state.parkedReason.isEmpty
        if fromDeadSession then
          store.append(entry.id, LoopEvent.Parked(ParkReason.Anomaly(DeadSessionDetail), now()))
        store.append(entry.id, LoopEvent.Resumed(sessionId, now())) match
          case Left(error) =>
            loops.remove(entry.id)
            entry.repl = null
            closeQuietly(repl)
            broadcastStatus()
            notifyLead(adoptionRefusedNotice(entry.id, error))
            reject(conn, entry.id, error)
          case Right(_) =>
            entry.phase = Running
            broadcastStatus()
            notifyLead(adoptedNotice(entry.id, state, fromDeadSession))
            launchDrive(entry)

  // -- the drive cycle ---------------------------------------------------------------

  /** Put a driver on `entry`, unless one is already there. Spawned into [[scope]] — the
    * host's lifetime — because a generation outlives whatever asked for it. */
  private def launchDrive(entry: LoopEntry): Unit =
    val spawn = scope
    if spawn != null && !entry.driving && !closed then
      entry.driving = true
      given Async.Spawn = spawn
      Future(drive(entry))
      ()

  /** Spend the loop's budget, one generation at a time, until it parks.
    *
    * Every decision is taken from the LEDGER rather than from anything carried across
    * an iteration: the budgets, the lineage, whether a park landed while a generation
    * was in flight, and whether a previous driver left a generation unsettled. That is
    * what makes a resume identical to a start — the loop picks up from what is written
    * down, whoever wrote it and however long ago.
    */
  private def drive(entry: LoopEntry)(using Async): Unit =
    try
      var running = true
      while running && !closed do
        // A generation boundary is the one safe moment to let an amendment's checker
        // take over, so it is taken here rather than where the amendment landed.
        applyStagedGate(entry)
        store.state(entry.id) match
          case Left(error) =>
            park(entry, ParkReason.Anomaly(s"the loop's ledger could not be read: $error"))
            running = false
          case Right(state) =>
            if state.parkedReason.isDefined then running = false
            // A generation still in flight belongs to a driver that is gone (an API
            // outage parked it, or the session ended). It cannot be resumed — the
            // worker's conversation died with it — so it is rescued and abandoned, and
            // the patience budget gets its say on the next pass.
            else if state.inFlight.isDefined then rescueUnsettled(entry, state)
            else if state.budgetExhausted then
              park(entry, ParkReason.BudgetExhausted)
              running = false
            else if state.patienceExhausted then
              park(entry, ParkReason.PatienceExhausted)
              running = false
            else running = runGeneration(entry, state)
    catch case _: CancellationException => ()
    finally
      entry.driving = false
      // An amendment that landed while this driver was on its last generation would
      // otherwise wait for a resume that may never come.
      applyStagedGate(entry)

  /** Settle a generation whose driver never finished it: capture whatever is on disk,
    * roll the tree back to the last state worth keeping, and record the abandonment. */
  private def rescueUnsettled(entry: LoopEntry, state: LoopState)(using Async): Unit =
    val flight = state.inFlight.get
    // Usually nothing to sweep — the members this generation hired died with the
    // session that ran it — but a generation rescued WITHIN a session is settling
    // here for the first time, and its members are still on the roster.
    retireOwned(entry.id, flight.gen)
    abandon(entry, state, flight.gen, flight.attempts)
    ()

  /** One generation, start to settled. Returns whether the driver should carry on. */
  private def runGeneration(entry: LoopEntry, state: LoopState)(using Async): Boolean =
    val gen = state.nextGen
    // Pinned now, workflow-style, so a `/new` or `/resume` mid-generation cannot split
    // one generation's transcripts across two session directories.
    val genSession = sessionId
    store.append(entry.id, LoopEvent.GenerationStarted(gen, state.latestAccepted.map(_.gen), genSession, now())) match
      case Left(error) =>
        park(entry, ParkReason.Anomaly(s"generation $gen could not be started: $error"))
        false
      case Right(_) =>
        setPhase(entry, runningPhase(gen))
        val repl = makeRepl(workerEnvFor(entry.id, gen))
        try runAttempts(entry, state, gen, genSession, repl)
        // However this generation ended — accepted, abandoned, parked under it — the
        // help it hired goes with it, before its own worker is closed, and the panel
        // stops claiming a stage that nothing is standing at.
        finally
          setStage(entry, None)
          retireOwned(entry.id, gen)
          closeQuietly(repl)

  /** The environment ONE generation's worker is spawned with: the host's orchestration
    * sockets ([[workerEnv]]), plus the two tags that say which piece of work this is.
    *
    * `AUK_TEAM_OWNER` is what makes [[retireOwned]] possible — every member this worker
    * creates is stamped with it, so the sweep at the end of the generation can name
    * exactly the members that generation hired and no others. It is per-GENERATION
    * rather than per-loop because that is the unit of work a member was reasoning
    * about; the next generation starts from a different tree and hires its own.
    *
    * There is no `AUK_LOOP_SOCK` here, and that absence is the nested-loop prohibition:
    * a generation calling `lib.loop.start` fails in the library, which reads
    * `AUK_LOOP_WORKER` to say why (see `auk.library.LoopImpl.connect`). A generation
    * that could start a loop would be spending a budget nobody granted it, in a tree
    * this loop is already snapshotting. */
  private def workerEnvFor(loopId: String, gen: Int): Map[String, String] =
    workerEnv ++ Map(
      "AUK_TEAM_OWNER" -> ownerTag(loopId, gen),
      "AUK_LOOP_WORKER" -> workerLabel(loopId, gen)
    )

  /** Retire whatever team members this generation created. Idempotent and cheap: a
    * generation that hired nobody (the usual case) retires nothing. */
  private def retireOwned(loopId: String, gen: Int)(using Async): Unit =
    retireTeamOwned(ownerTag(loopId, gen))
    ()

  /** The retry loop inside one generation: worker → check → evaluator, up to
    * `maxAttemptsPerGeneration` times, all on ONE worker conversation so a retry reads
    * as "that didn't work, here's why" rather than as a fresh worker rediscovering the
    * problem. Returns whether the driver should carry on.
    *
    * The ledger is re-folded before every attempt, so a prompt is always composed from
    * the loop's configuration as it stands NOW. That is what makes steering feel like
    * steering: a lead watching a generation flail can retune the goal, the rubric or the
    * budgets and have the next attempt read the new words, instead of waiting out a
    * generation that is already going the wrong way. A re-read that fails leaves the
    * attempt on the configuration it had — a ledger that cannot be read is [[drive]]'s
    * problem to park over, not a reason to spend this attempt on nothing.
    *
    * Once per attempt, though, and not once per prompt: one attempt is composed from ONE
    * reading of the ledger, which is what [[settleAttempt]] is handed as well, so a
    * candidate is never judged against a rubric its author was not shown. An amendment
    * that lands mid-attempt waits for the next one, where the worker and the evaluator
    * pick it up together.
    */
  private def runAttempts(
      entry: LoopEntry,
      state: LoopState,
      gen: Int,
      genSession: String,
      repl: ScalaRepl
  )(using Async): Boolean =
    val prev = state.latestAccepted
    val tools = baseTools(repl)
    var live = state
    var history: List[Message] = Nil
    var attempt = 0
    var feedback: Option[String] = None
    var settled: Option[Boolean] = None
    while settled.isEmpty do
      attempt += 1
      live = store.state(entry.id).getOrElse(live)
      val budgets = live.budgets
      val submit = new SubmitFields(
        "submit_generation",
        submitGenerationDescription(live.artifactSchema),
        generationSchema(live.artifactSchema),
        maxResultRetries
      )
      val seed =
        if attempt == 1 then List(Message.user(workerTask(entry.id, gen)))
        else List(Message.user(retryMessage(gen, attempt - 1, budgets.maxAttemptsPerGeneration, feedback.getOrElse(""))))
      val system = workerSystemPrompt + "\n\n" + workerSection(
        loopId = entry.id,
        goal = live.goal,
        rubric = live.rubric,
        gen = gen,
        maxGenerations = budgets.maxGenerations,
        attempt = attempt,
        maxAttempts = budgets.maxAttemptsPerGeneration,
        lineage = live.generations.toList,
        knowledge = store.readKnowledge(entry.id).getOrElse(""),
        feedback = feedback
      )
      setStage(entry, Some(Stage(gen, attempt, Step.Working)))
      runAgent(entry.id, genSession, workerTranscriptLabel(gen), history ++ seed, system, tools, submit) match
        case Left(Interruption.Parked) => settled = Some(false)
        case Left(Interruption.Barren(why)) =>
          settled = Some(abandon(entry, live, gen, attempt - 1, why))
        case Right((grown, fields)) =>
          history = grown
          settleAttempt(entry, live, gen, genSession, attempt, prev, fields) match
            case Left(Interruption.Parked)      => settled = Some(false)
            case Left(Interruption.Barren(why)) => settled = Some(abandon(entry, live, gen, attempt, why))
            case Right(Some(rejection)) =>
              if attempt >= budgets.maxAttemptsPerGeneration then
                settled = Some(abandon(entry, live, gen, attempt, rejection))
              else feedback = Some(rejection)
            case Right(None) => settled = Some(true)
    settled.getOrElse(false)

  /** Everything that happens to a submitted attempt: snapshot, check, evaluate, and
    * either accept it or hand back the rejection the next attempt is told about.
    * `Right(None)` means the generation was accepted. */
  private def settleAttempt(
      entry: LoopEntry,
      state: LoopState,
      gen: Int,
      genSession: String,
      attempt: Int,
      prev: Option[GenerationRecord],
      fields: Json.Obj
  )(using Async): Either[Interruption, Option[String]] =
    val artifact = fields.get("artifact").getOrElse(Json.Null)
    val description = fields.get("description").collect { case Json.Str(s) => s }.getOrElse("")
    val knowledge = fields.get("knowledge").collect { case Json.Str(s) => s }.filter(_.nonEmpty)
    val snapshotId = attemptId(entry.id, gen, attempt)
    Snapshot.create(context.workingDirectory, snapshotId) match
      case Left(error) =>
        Left(anomaly(entry, s"generation $gen's attempt $attempt could not be captured: ${describe(error)}"))
      case Right(snap) =>
        store.append(
          entry.id,
          LoopEvent.AttemptSubmitted(gen, attempt, artifact, description, knowledge, List(snap.commit), now())
        ) match
          case Left(error) => Left(anomaly(entry, s"generation $gen's attempt $attempt could not be recorded: $error"))
          case Right(_) =>
            setStage(entry, Some(Stage(gen, attempt, Step.Checking)))
            runCheck(entry, gen, attempt, prev, artifact, description, snap.commit) match
              case Left(detail) => Left(anomaly(entry, detail))
              case Right(report) =>
                store.append(
                  entry.id,
                  LoopEvent.CheckCompleted(gen, attempt, report.passed, report.reasons, report.metrics, now())
                )
                if !report.passed then Right(Some(reasonsText(report.reasons)))
                else
                  evaluate(entry, state, gen, genSession, attempt, report, artifact, description, snap.commit, prev)
                    .flatMap: outcome =>
                      if !outcome.accepted then Right(Some(verdictText(outcome.feedback)))
                      else accept(entry, gen, attempt, snap.commit, snapshotId, description, knowledge, report, outcome)

  /** Hand the candidate to the loop's checker, which lives in the gate worker as a
    * closure and can only be reached by evaluating a call to it there. `Left` is a
    * broken loop rather than a failed candidate — see [[auk.library.LoopRegistry]]. */
  private def runCheck(
      entry: LoopEntry,
      gen: Int,
      attempt: Int,
      prev: Option[GenerationRecord],
      artifact: Json,
      description: String,
      commit: String
  )(using Async): Either[String, CheckReport] =
    val repl = entry.repl
    if repl == null then Left(s"loop '${entry.id}' has no gate session to run its checker in")
    else
      val source = checkSource(entry.id, prev.map(prevPayload), candPayload(artifact, description, List(commit)))
      repl.eval(source, Some(CheckTimeoutMs)).status match
        case ScalaRepl.Status.Completed(r) if r.ok =>
          parseCheckMarker(r.stdout).left.map(detail => s"generation $gen's check (attempt $attempt) failed to report: $detail")
        case ScalaRepl.Status.Completed(r) =>
          val detail = if r.output.nonEmpty then r.output else r.error.getOrElse("the check did not evaluate")
          Left(s"generation $gen's check (attempt $attempt) could not run: ${ReplProtocol.stripAnsi(detail).trim}")
        case ScalaRepl.Status.TimedOut(ms) =>
          Left(s"generation $gen's checker did not finish within ${ms}ms")
        case ScalaRepl.Status.Failed(reason) =>
          Left(s"generation $gen's check (attempt $attempt) could not run: $reason")

  /** The judgement gate after the checker's mechanical one: a fresh agent reads the
    * rubric, the checker's report, and the diff, then submits a verdict.
    *
    * It gets the same tools every loop agent gets, so it can look at the live tree
    * rather than only at the patch — and that is exactly why the tree is reset to the
    * attempt's snapshot afterwards. An evaluator that edits while it reads would
    * otherwise silently become a co-author of the generation it is judging. */
  private def evaluate(
      entry: LoopEntry,
      state: LoopState,
      gen: Int,
      genSession: String,
      attempt: Int,
      report: CheckReport,
      artifact: Json,
      description: String,
      commit: String,
      prev: Option[GenerationRecord]
  )(using Async): Either[Interruption, Verdict] =
    val base = prev.map(_.commit).getOrElse(state.baselineCommit)
    val diff = Snapshot.diffTrees(context.workingDirectory, base, commit).getOrElse("")
    val submit = new SubmitFields("submit_verdict", SubmitVerdictDescription, VerdictSchema, maxResultRetries)
    val system = workerSystemPrompt + "\n\n" + evaluatorSection(entry.id, state.goal, state.rubric)
    val repl = makeRepl(Map.empty)
    val outcome =
      try
        setStage(entry, Some(Stage(gen, attempt, Step.Evaluating)))
        val seed = List(Message.user(evaluatorCase(gen, state.rubric, state.generations.toList, report, artifact, description, diff)))
        runAgent(entry.id, genSession, evalTranscriptLabel(gen), seed, system, baseTools(repl), submit) match
          case Left(interruption) => Left(interruption)
          case Right((_, fields)) =>
            Right(
              Verdict(
                accepted = fields.get("accepted").collect { case Json.Bool(b) => b }.getOrElse(false),
                feedback = fields.get("feedback").collect { case Json.Str(s) => s }.getOrElse(""),
                goalReached = fields.get("goalReached").collect { case Json.Bool(b) => b }.getOrElse(false)
              )
            )
      finally closeQuietly(repl)
    // An evaluator that never submits is a rejection, not an acceptance: nothing was
    // judged, and the conservative reading of "no verdict" is to keep the work out of
    // the lineage and let the worker try again.
    val settled = outcome match
      case Left(Interruption.Barren(why)) => Right(Verdict(accepted = false, feedback = why, goalReached = false))
      case other                          => other
    settled.flatMap: verdict =>
      store.append(entry.id, LoopEvent.VerdictIssued(gen, attempt, verdict.accepted, verdict.feedback, verdict.goalReached, now()))
      Worktree.reset(context.workingDirectory, commit) match
        case Right(reset) =>
          if reset.skippedGitlinks.nonEmpty then onNotice(gitlinkNotice(entry.id, reset.skippedGitlinks))
          Right(verdict)
        case Left(ResetError.WouldClobberIgnored(paths)) =>
          Left(anomaly(entry, clobberDetail(s"generation $gen's evaluation", paths)))
        case Left(ResetError.Git(error)) =>
          Left(anomaly(entry, s"the tree could not be restored after generation $gen was evaluated: ${describe(error)}"))

  /** Take the attempt into the lineage: drop the refs of the attempts that lost, record
    * the acceptance, apply whatever the worker learned, and park if the evaluator says
    * the loop is done. */
  private def accept(
      entry: LoopEntry,
      gen: Int,
      attempt: Int,
      commit: String,
      snapshotId: String,
      description: String,
      knowledge: Option[String],
      report: CheckReport,
      verdict: Verdict
  )(using Async): Either[Interruption, Option[String]] =
    deleteAttemptRefs(entry.id, gen, 1 to attempt, except = Some(attempt))
    store.append(entry.id, LoopEvent.GenerationAccepted(gen, snapshotId, commit, description, report.metrics, now())) match
      case Left(error) => Left(anomaly(entry, s"generation $gen's acceptance could not be recorded: $error"))
      case Right(_) =>
        knowledge.foreach(text => { store.writeKnowledge(entry.id, text); () })
        notifyLead(acceptedNotice(entry.id, gen, report.metrics, verdict.goalReached))
        if verdict.goalReached then park(entry, ParkReason.GoalReached)
        Right(None)

  /** Give up on a generation, without losing what it did: the dirty tree is captured
    * first, THEN rolled back to the last state worth keeping. Returns whether the
    * driver should carry on. */
  private def abandon(entry: LoopEntry, state: LoopState, gen: Int, attempts: Int, why: String = "")(using
      Async
  ): Boolean =
    val rescueId = abandonedId(entry.id, gen)
    // Forced, so a rescue that runs twice (a reset that refused, then a resume) repoints
    // the ref at what is on disk now instead of failing on the ref it left behind.
    val rescue = Snapshot.create(context.workingDirectory, rescueId, force = true).toOption
    val target = state.latestAccepted.map(_.commit).getOrElse(state.baselineCommit)
    Worktree.reset(context.workingDirectory, target) match
      case Left(ResetError.WouldClobberIgnored(paths)) =>
        park(entry, ParkReason.Anomaly(clobberDetail(s"abandoning generation $gen", paths)))
        false
      case Left(ResetError.Git(error)) =>
        park(entry, ParkReason.Anomaly(s"the tree could not be rolled back after generation $gen: ${describe(error)}"))
        false
      case Right(reset) =>
        if reset.skippedGitlinks.nonEmpty then onNotice(gitlinkNotice(entry.id, reset.skippedGitlinks))
        deleteAttemptRefs(entry.id, gen, 1 to attempts.max(0), except = None)
        store.append(entry.id, LoopEvent.GenerationAbandoned(gen, attempts, rescue.map(_ => rescueId), now())) match
          case Left(error) =>
            park(entry, ParkReason.Anomaly(s"generation $gen's abandonment could not be recorded: $error"))
            false
          case Right(_) =>
            notifyLead(abandonedNotice(entry.id, gen, attempts, rescue.map(_ => rescueId), why))
            true

  /** Run one loop agent to completion, teeing its transcript to the session log.
    * `Left` is a run that produced nothing to act on; `Right` carries the grown
    * conversation (so the next attempt continues it) and the submitted fields. */
  private def runAgent(
      loopId: String,
      genSession: String,
      label: String,
      messages: List[Message],
      system: String,
      tools: List[Tool],
      submit: SubmitFields
  )(using Async): Either[Interruption, (List[Message], Json.Obj)] =
    given RuntimeContext = context
    val log = logger(loopId, genSession, label)
    val registry = ToolRegistry.of((tools :+ submit)*)
    var nudges = 0
    val outcome = HeadlessAgent.runConversation(
      messages,
      models,
      registry,
      system,
      onActivity = a => publish(loopId, label, log, a),
      finishGuard = () =>
        if submit.captured.isDefined || submit.rejections >= maxResultRetries || nudges >= maxResultRetries then None
        else { nudges += 1; Some(nudgeMessage(submit.name)) },
      haltAfterTools = () => submit.rejections >= maxResultRetries,
      retryDelaysMs = retryDelaysMs
    )
    outcome.llmError match
      case Some(error) if error.transient =>
        // The API kept failing through the whole retry schedule — a provider outage,
        // not this loop's fault. Park exactly as the workflow bridge auto-pauses, and
        // tell the USER rather than the lead: a lead told about a dead API would just
        // spend its own retry schedule on the same dead API.
        Left(apiFailure(loopId, error.description))
      case Some(error) =>
        // A permanent error (a rejected request, a model that does not exist) repeats
        // on every retry, so burning the loop's budget on it helps nobody.
        Left(anomalyById(loopId, s"the model API refused the $label request: ${error.description}"))
      case None =>
        submit.captured match
          case Some(fields) => Right((outcome.messages, fields))
          case None         => Left(Interruption.Barren(noSubmission(submit.name, submit.lastError, submit.rejections)))

  // -- driver helpers ------------------------------------------------------------------

  /** Record a park, move the phase, and say so. An API failure is the one reason the
    * lead is NOT told about: it is the user's outage to wait out. */
  private def park(entry: LoopEntry, reason: ParkReason): Unit =
    store.append(entry.id, LoopEvent.Parked(reason, now()))
    setPhase(entry, phaseFor(reason))
    reason match
      case ParkReason.ApiFailure(_) => ()
      case _                        => notifyLead(parkedForNotice(entry.id, reason))

  private def anomaly(entry: LoopEntry, detail: String): Interruption =
    park(entry, ParkReason.Anomaly(detail))
    Interruption.Parked

  private def anomalyById(loopId: String, detail: String): Interruption =
    loops.get(loopId).foreach(entry => park(entry, ParkReason.Anomaly(detail)))
    Interruption.Parked

  private def apiFailure(loopId: String, detail: String): Interruption =
    loops.get(loopId).foreach(entry => park(entry, ParkReason.ApiFailure(detail)))
    Interruption.Parked

  private def setPhase(entry: LoopEntry, phase: String): Unit =
    entry.phase = phase
    broadcastStatus()

  private def deleteAttemptRefs(loopId: String, gen: Int, attempts: Range, except: Option[Int]): Unit =
    attempts.filterNot(except.contains).foreach: k =>
      Snapshot.delete(context.workingDirectory, attemptId(loopId, gen, k))
      ()

  /** One thing a loop agent said, to the panel and to the session log at once — the
    * same event to both, so what a reader watches live is exactly what the log keeps. */
  private def publish(loopId: String, label: String, log: WireMessage => Unit, a: HeadlessAgent.Activity): Unit =
    val ev = WorkflowBridge.transcriptOf(a, loopId, label)
    onActivity(loopId, ev)
    log(WireMessage.Activity(ev))

  /** Per-agent log: tee this run's transcript to
    * `.auk/sessions/<session>/loop/<loop>__<label>.jsonl` as the same `WireMessage`
    * JSONL the dashboard consumes. Best-effort — a write failure never disturbs a
    * generation — and `None` (tests) disables it. */
  private def logger(loopId: String, genSession: String, label: String): WireMessage => Unit =
    val path = sessionRef.map(_ =>
      SessionRef.loopLog(context.resolve(SessionProvider.RelativePath), genSession, loopId, label))
    m => path.foreach(p => { JsonlLog.append(p, WireCodec.encode(m)); () })

  private def closeQuietly(repl: ScalaRepl)(using Async): Unit =
    try repl.close()
    catch case _: Throwable => ()

  // -- wire out --------------------------------------------------------------------

  private def reject(conn: SocketServer.Conn, loopId: String, reason: String): Unit =
    conn.write(errorMsg(loopId, reason).render)

  /** Push a full snapshot of every loop's phase to every connected worker, and of every
    * loop entire to the UI, after any change. Cheap (a handful of loops) and idempotent,
    * so nothing has to track which client last saw what. */
  private def broadcastStatus(): Unit =
    val all = views()
    val line = statusMsg(all.map(v => (v.id, v.phase)).toList).render
    conns.foreach(_.write(line))
    onLoop(all)

  /** Push the loops to the UI without troubling the wire: the drive cycle moving from
    * the worker to the checker to the evaluator changes what the panel says while the
    * PHASE — which is all the wire carries — stays `running (gen N)`. */
  private def emitLoops(): Unit = onLoop(views())

  /** Move the in-flight stage and tell the panel. */
  private def setStage(entry: LoopEntry, stage: Option[Stage]): Unit =
    entry.stage = stage
    emitLoops()

  /** Every loop this session could act on, in full: the ones this bridge is driving, in
    * creation order, then whatever else the project's `.auk/loops` holds.
    *
    * The disk half is what makes a loop from an earlier session reachable at all — a
    * lead that cannot see a loop has no way to name it, and `resume` is the whole point
    * of the ledger outliving its session. A ledger that will not fold is left out: it
    * describes nothing this bridge could pick up. Nor has a loop still being validated
    * written one yet, so that one is reported from the configuration it arrived with.
    */
  private[runtime] def views(): Vector[LoopView] =
    val live = loops.valuesIterator.map(viewOf).toVector
    val named = live.map(_.id).toSet
    live ++ store
      .list()
      .filterNot(named)
      .flatMap(id => store.state(id).toOption.map(state => loopView(id, diskPhase(state), None, state)))

  /** Every loop this session could act on, as `(id, phase)` — the wire's half of
    * [[views]], in the same order. */
  private[runtime] def phases(): List[(String, String)] = views().map(v => (v.id, v.phase)).toList

  private def viewOf(entry: LoopEntry): LoopView =
    store.state(entry.id).toOption match
      case Some(state) => loopView(entry.id, entry.phase, entry.stage, state)
      case None        => pendingView(entry.id, entry.phase, entry.config.goal)

  /** How a loop nobody here is driving reads. A ledger that says "running" with no
    * driver behind it is not running — it is what a session that ended mid-generation
    * left behind — and saying so is what tells a lead there is something to pick up. */
  private def diskPhase(state: LoopState): String =
    state.parkedReason.map(phaseFor).getOrElse(Orphaned)

  /** The phase this bridge currently reports for `loopId`, from what it is driving. */
  def statusOf(loopId: String): Option[String] = loops.get(loopId).map(_.phase)

  def close()(using Async): Unit =
    closed = true
    loops.valuesIterator.foreach: entry =>
      val repl = entry.repl
      val staged = entry.pendingGate
      entry.repl = null
      entry.pendingGate = null
      if repl != null then repl.close()
      if staged != null then staged.close()
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

  /** What a loop's checker said about one candidate, as parsed back out of the gate
    * session's marker line. */
  private[runtime] final case class CheckReport(passed: Boolean, reasons: List[String], metrics: Map[String, Double])

  /** What the evaluator agent said about one candidate. */
  private[runtime] final case class Verdict(accepted: Boolean, feedback: String, goalReached: Boolean)

  /** Why a step of the drive cycle produced nothing to act on. [[Parked]] means the
    * loop has already been parked and the driver should stop; [[Barren]] means the
    * agent ran but submitted nothing, which costs the generation rather than the loop. */
  private[runtime] enum Interruption:
    case Parked
    case Barren(why: String)

  /** The definition has been submitted and is being checked; no loop exists yet. */
  val Validating: String = "validating"

  /** A loop from an earlier session is being picked up: its stored definition is being
    * re-evaluated in a fresh gate worker before it is allowed to run again. */
  val Adopting: String = "adopting"

  /** How a loop the project records as running, with nobody driving it, is reported.
    * Not a park — nothing decided to stop it — but not live either. */
  val Orphaned: String = "orphaned (dead session)"

  /** What an adoption writes into the ledger for the stretch nobody was driving. */
  private[runtime] val DeadSessionDetail: String = "adopted from a dead session"

  /** The loop exists and is live. Every running phase starts with this — a loop
    * working on a generation reports `running (gen N)` — so the prefix, not equality,
    * is what tells "live" from "validating" or "parked". */
  val Running: String = "running"

  /** Every parked phase starts with this. */
  val ParkedPrefix: String = "parked: "

  /** The phase of a loop that is working on generation `gen`. */
  def runningPhase(gen: Int): String = s"$Running (gen $gen)"

  // -- what the panel is told ------------------------------------------------------

  /** Which of a generation's three agents is running right now. The ledger records
    * what has HAPPENED, which leaves "the worker is thinking" and "the evaluator is
    * reading the diff" indistinguishable — so the driver says which, and only the
    * driver can. */
  private[runtime] enum Step:
    case Working, Checking, Evaluating

  /** Where the drive cycle stands inside the generation in flight. `attempt` is the
    * one being worked, counted from 1 — the ledger's own count trails it, since an
    * attempt is only recorded once it has been submitted. */
  private[runtime] final case class Stage(gen: Int, attempt: Int, step: Step)

  /** Where a generation's worker files its transcript, under the loop's id. The panel
    * reads the same key, so the two must never be written out separately. */
  def workerTranscriptLabel(gen: Int): String = s"gen-$gen-worker"

  /** The same, for the generation's evaluator. */
  def evalTranscriptLabel(gen: Int): String = s"gen-$gen-eval"

  /** One loop as the UI draws it, from the fold of its ledger and the phase and stage
    * only the driver knows. Pure: given the same three it always says the same thing,
    * which is what lets the panel be tested without a bridge, a socket or a worker.
    *
    * `phase` is the authority on whether the loop is live, not the ledger: a loop
    * being ADOPTED has a ledger that still says parked — the resume is written once
    * its definition has been re-checked — and reporting it as parked would hide the
    * one thing happening to it.
    */
  private[runtime] def loopView(id: String, phase: String, stage: Option[Stage], state: LoopState): LoopView =
    val accepted = state.generations.map(g => g.gen -> g).toMap
    val inFlight = state.inFlight.map(_.gen)
    // Every generation NUMBER the loop has spent, not just the ones that worked: an
    // abandoned generation leaves no record in the fold, but it did happen, and a
    // lineage drawn without it reads as an unbroken run of successes.
    val generations = (1 to state.generationsStarted).toVector.map: gen =>
      accepted.get(gen) match
        case Some(record) =>
          LoopGenerationView(
            gen,
            LoopGenerationState.Accepted,
            record.metrics.toVector.sortBy(_._1),
            headLine(record.description)
          )
        case None if inFlight.contains(gen) =>
          LoopGenerationView(gen, LoopGenerationState.Running, Vector.empty, "")
        case None =>
          LoopGenerationView(gen, LoopGenerationState.Abandoned, Vector.empty, "")
    LoopView(
      id = id,
      phase = phase,
      goal = headLine(state.goal),
      generations = generations,
      activity = stage.map(activityLine),
      liveLabel = stage.map(transcriptLabel),
      parked = Option.when(phase.startsWith(ParkedPrefix))(phase.drop(ParkedPrefix.length)),
      orphaned = phase == Orphaned
    )

  /** A loop that has been accepted but not yet written down: everything the panel can
    * say about it comes from the configuration its `hello` carried. */
  private[runtime] def pendingView(id: String, phase: String, goal: String): LoopView =
    LoopView(id, phase, headLine(goal), Vector.empty, None, None, None, orphaned = false)

  private def activityLine(stage: Stage): String =
    val step = stage.step match
      case Step.Working    => "working"
      case Step.Checking   => "checking"
      case Step.Evaluating => "evaluating"
    s"gen ${stage.gen}, attempt ${stage.attempt} — $step"

  /** Which transcript a stage is streaming into. A check runs as a closure in the gate
    * worker and streams nothing, so it leaves the worker's transcript on screen — which
    * is the right thing to be reading while its work is being measured. */
  private def transcriptLabel(stage: Stage): String = stage.step match
    case Step.Evaluating              => evalTranscriptLabel(stage.gen)
    case Step.Working | Step.Checking => workerTranscriptLabel(stage.gen)

  /** The first non-blank line of a written field, bounded — a goal or a description is
    * prose someone typed, and the panel has one line for it. */
  private def headLine(text: String): String =
    val first = text.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
    if first.length > 160 then first.take(159) + "…" else first

  /** How long the gate worker gets to compile and run a definition. Generous: a cold
    * worker pays REPL startup plus a full compilation of the eval. */
  private val GateTimeoutMs = 120_000

  /** How long a single check may take. A checker runs builds and benchmarks, so this is
    * deliberately long — but it is bounded, because a checker that hangs would hang the
    * loop forever with nothing to show for it. */
  private val CheckTimeoutMs = 1_800_000

  /** How long to keep giving the event loop a chance to deliver the `bound`
    * acknowledgement after the gate eval has answered. */
  private val BoundTimeoutMs = 5_000
  private val BoundPollMs = 10

  /** The same, for the `hello` that precedes a captured definition. */
  private val HelloTimeoutMs = 5_000
  private val HelloPollMs = 5

  /** The prefix of the one line [[auk.library.LoopRegistry.runCheck]] prints to carry a
    * checker's verdict out of the gate session. Must match `LoopImpl.CheckMarker`. */
  private[runtime] val CheckMarker: String = "auk:loop:check"

  /** How much of a generation's patch the evaluator is shown. A diff past this is a
    * diff nobody reads in full anyway, and the artifact plus the checker's metrics
    * carry the measurable part of the story. */
  private[runtime] val MaxDiffChars: Int = 64 * 1024

  /** How many accepted generations the lineage digest names. */
  private[runtime] val LineageDigest: Int = 5

  /** A per-process temp socket path for the bridge (the server unlinks any stale file
    * on bind). */
  def defaultSocketPath(): String =
    val os = js.Dynamic.global.require("node:os")
    val pid = js.Dynamic.global.process.pid
    s"${os.tmpdir().asInstanceOf[String]}/auk-loop-$pid.sock"

  // -- what a generation's worker is stamped with -------------------------------------

  /** The owner tag one generation's worker creates team members under
    * (`AUK_TEAM_OWNER`), and the key the sweep at the end of that generation retires
    * by. Per generation, not per loop: a member is hired to reason about ONE tree. */
  def ownerTag(loopId: String, gen: Int): String = s"loop:$loopId:gen-$gen"

  /** How a generation's worker describes itself when something it may not do fails
    * (`AUK_LOOP_WORKER`) — the library turns it into the nested-loop refusal, so this
    * reads as a phrase inside a sentence rather than as an identifier. */
  def workerLabel(loopId: String, gen: Int): String = s"generation $gen of loop '$loopId'"

  // -- snapshot ids ---------------------------------------------------------------------

  /** The snapshot id holding the tree a loop measures itself against. Namespaced under
    * the loop so everything it ever records is reachable from one prefix. */
  def baselineId(loopId: String): String = s"loop/$loopId/baseline"

  /** The snapshot id of one submitted attempt. The accepted attempt's ref IS the
    * generation's durable record — there is no second, generation-level ref — and the
    * ones that lost are deleted when their generation settles. */
  def attemptId(loopId: String, gen: Int, attempt: Int): String = s"loop/$loopId/gen-$gen-a$attempt"

  /** The snapshot id holding what an abandoned generation left on disk, captured before
    * the tree is rolled back so discarded work is still recoverable by hand. */
  def abandonedId(loopId: String, gen: Int): String = s"loop/$loopId/gen-$gen-abandoned"

  /** How a park reason reads as a phase string. */
  def phaseFor(reason: ParkReason): String = reason match
    case ParkReason.GoalReached       => s"${ParkedPrefix}goal reached"
    case ParkReason.BudgetExhausted   => s"${ParkedPrefix}budget exhausted"
    case ParkReason.PatienceExhausted => s"${ParkedPrefix}patience exhausted"
    case ParkReason.UserRequested     => s"${ParkedPrefix}user requested"
    case ParkReason.ApiFailure(d)     => s"${ParkedPrefix}api failure: $d"
    case ParkReason.Anomaly(d)        => s"${ParkedPrefix}anomaly: $d"

  // -- the check call ---------------------------------------------------------------------

  /** The Scala source the bridge evaluates in a loop's gate session to run its checker.
    *
    * It is a single call with three string literals, because that is the only shape
    * that survives the trip: the gate session compiles this text with nothing but the
    * REPL preamble in scope, and the JSON it carries is arbitrary — a description
    * holding quotes, a backslash, a newline. So the payloads go through
    * [[scalaLiteral]] rather than through interpolation or triple quotes, both of which
    * have content that can close them.
    */
  private[runtime] def checkSource(loopId: String, prevJson: Option[String], candJson: String): String =
    val prev = prevJson.map(scalaLiteral).getOrElse("null")
    s"auk.library.LoopRegistry.runCheck(${scalaLiteral(loopId)}, $prev, ${scalaLiteral(candJson)})"

  /** `s` as a Scala string literal, escaped so that any string round-trips.
    *
    * Backslash and double quote are escaped because they would end the literal or the
    * escape; everything below space (and DEL) becomes a `\\uXXXX` escape because a raw
    * newline or tab in a single-quoted literal is a syntax error, and because the REPL
    * carries source as a JSON field where a raw control character is not legal either.
    * Everything else — including non-ASCII — passes through as itself: the REPL
    * protocol is UTF-8 JSON in both directions, so a `é` needs no help.
    */
  private[runtime] def scalaLiteral(s: String): String =
    val out = new StringBuilder("\"")
    s.foreach: c =>
      if c == '\\' then out ++= "\\\\"
      else if c == '"' then out ++= "\\\""
      else if c.toInt < 0x20 || c.toInt == 0x7f then out ++= unicodeEscape(c)
      else out += c
    out += '"'
    out.toString

  /** A character as a `\\uXXXX` escape, built rather than formatted so this file's own
    * source holds no control character to describe one. */
  private def unicodeEscape(c: Char): String =
    val hex = Integer.toHexString(c.toInt).nn
    "\\u" + "0" * (4 - hex.length) + hex

  /** The newest accepted generation, as the checker's `prev` is handed to it. */
  private[runtime] def prevPayload(record: GenerationRecord): String =
    Json.Obj(List(
      "artifact" -> record.artifact,
      "gen" -> Json.num(record.gen),
      "description" -> Json.Str(record.description),
      "commit" -> Json.Str(record.commit),
      "metrics" -> metricsJson(record.metrics)
    )).render

  /** The attempt under check, as the checker's `cand` is handed to it. */
  private[runtime] def candPayload(artifact: Json, description: String, commits: List[String]): String =
    Json.Obj(List(
      "artifact" -> artifact,
      "description" -> Json.Str(description),
      "commits" -> Json.Arr(commits.map(Json.Str.apply))
    )).render

  private def metricsJson(metrics: Map[String, Double]): Json =
    Json.Obj(metrics.toList.sortBy(_._1).map((k, v) => k -> Json.num(v)))

  /** Read a checker's verdict out of a gate eval's stdout. The LAST marker line wins:
    * a checker is free to print, and only the marker the call itself wrote is a
    * verdict. `Left` is a loop that could not be checked at all — an unregistered
    * checker, an artifact the schema cannot decode, or no marker whatsoever. */
  private[runtime] def parseCheckMarker(stdout: String): Either[String, CheckReport] =
    val prefix = CheckMarker + ":"
    stdout.linesIterator.filter(_.startsWith(prefix)).toList.lastOption match
      case None => Left("the checker printed no result marker")
      case Some(line) =>
        Json.parse(line.substring(prefix.length).nn) match
          case Left(error) => Left(s"the checker's result was not JSON: $error")
          case Right(o: Json.Obj) =>
            o.get("error") match
              case Some(Json.Str(detail)) => Left(detail)
              case _ =>
                Right(CheckReport(
                  passed = o.get("passed").collect { case Json.Bool(b) => b }.getOrElse(false),
                  reasons = o.get("reasons").collect { case Json.Arr(es) => es.collect { case Json.Str(s) => s } }.getOrElse(Nil),
                  metrics = o.get("metrics").collect {
                    case Json.Obj(fs) => fs.collect { case (k, Json.Num(n)) => k -> n.toDouble }.toMap
                  }.getOrElse(Map.empty)
                ))
          case Right(_) => Left("the checker's result was not a JSON object")

  // -- submit tools -------------------------------------------------------------------

  /** The parameters of `submit_generation`: the loop's own artifact schema, plus the
    * handoff note and the optional knowledge replacement that ride with it. */
  private[runtime] def generationSchema(artifactSchema: Json): Json =
    Json.Obj(List(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(List(
        "artifact" -> artifactSchema,
        "description" -> Json.Obj(List("type" -> Json.Str("string"))),
        "knowledge" -> Json.Obj(List("type" -> Json.Str("string")))
      )),
      "required" -> Json.Arr(List(Json.Str("artifact"), Json.Str("description")))
    ))

  /** The parameters of `submit_verdict`. */
  private[runtime] val VerdictSchema: Json =
    Json.Obj(List(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(List(
        "accepted" -> Json.Obj(List("type" -> Json.Str("boolean"))),
        "feedback" -> Json.Obj(List("type" -> Json.Str("string"))),
        "goalReached" -> Json.Obj(List("type" -> Json.Str("boolean")))
      )),
      "required" -> Json.Arr(List(Json.Str("accepted"), Json.Str("feedback"), Json.Str("goalReached")))
    ))

  private[runtime] def submitGenerationDescription(artifactSchema: Json): String =
    "Submit this generation's work. Call this exactly once, when the working tree holds " +
      "the improvement you want judged. Fields:\n" +
      "  - `artifact`: what this generation measured or produced, matching exactly this schema:\n" +
      artifactSchema.render + "\n" +
      "  - `description`: the handoff note the NEXT generation reads as its account of the " +
      "current state — what you changed, what you tried that did not work, and where the " +
      "next improvement most likely is. Write it for an agent that cannot see this session.\n" +
      "  - `knowledge` (optional): the FULL replacement text of the loop's knowledge file. " +
      "It is applied only if this generation is accepted, and it REPLACES what is there, so " +
      "include everything still worth keeping.\n" +
      "If the call returns an error describing the problem, fix it and call `submit_generation` again."

  private[runtime] val SubmitVerdictDescription: String =
    "Submit your verdict on the candidate. Call this exactly once, with:\n" +
      "  - `accepted`: whether this generation should become the new standard the next one builds on.\n" +
      "  - `feedback`: why. On a rejection this is what the worker is shown when it retries, so " +
      "be concrete and actionable; on an acceptance it is the record of what convinced you.\n" +
      "  - `goalReached`: whether the loop's GOAL as a whole is now met. This is a separate " +
      "question from acceptance — a good generation that leaves work to do is `accepted: true, " +
      "goalReached: false`. Saying true stops the loop."

  /** A tool that captures an agent's structured submission, validating it against a
    * JSON-schema object before accepting it.
    *
    * Modelled on [[WorkflowBridge.SubmitResult]] and differing in one way: the schema
    * describes the CALL's parameters rather than a wrapped `result` field, since a loop
    * submission is several fields with different lifetimes (the artifact is checked,
    * the description is handed on, the knowledge is applied only on acceptance) rather
    * than one value. The decoder stays permissive so [[execute]] always runs and can
    * answer with a precise, path-qualified error the agent can act on; [[rejections]]
    * counts the misses so the caller can stop after [[maxRetries]].
    */
  private[runtime] final class SubmitFields(
      val name: String,
      val description: String,
      schema: Json,
      maxRetries: Int
  ) extends Tool:
    type Params = Json
    /** The validated submission, set once a call passes. */
    var captured: Option[Json.Obj] = None
    /** How many submissions have been rejected as not matching the schema. */
    var rejections: Int = 0
    /** The most recent rejection message, for a clean report if retries run out. */
    var lastError: Option[String] = None
    val input: ToolInput[Json] = ToolInput.instance(WorkflowBridge.jsonToSchema(schema))(j => Right(j))
    def execute(params: Json)(using RuntimeContext, Async): ToolResult =
      if captured.isDefined then ToolResult.ok("already recorded")
      else
        ResultSchema.validate(schema, params) match
          case Right(()) =>
            params match
              case o: Json.Obj =>
                captured = Some(o)
                lastError = None
                ToolResult.ok("recorded")
              case other =>
                reject(s"expected an object, got ${other.typeName}")
          case Left(error) => reject(error)

    private def reject(error: String): ToolResult =
      rejections += 1
      lastError = Some(error)
      ToolResult.error(
        if rejections >= maxRetries then
          s"Your submission is still invalid: $error. You have used all $maxRetries attempts; " +
            "no further submissions will be accepted."
        else
          s"Your submission is invalid: $error. Correct it and call `$name` again " +
            s"(attempt $rejections of $maxRetries)."
      )

  /** Appended as a user message when a loop agent ends its turn without submitting. */
  private[runtime] def nudgeMessage(tool: String): String =
    s"You ended your turn without calling the `$tool` tool. That call is the only thing " +
      s"this step produces, so nothing you did counts until you make it. Call `$tool` now."

  /** What an agent that never submitted is reported as. */
  private[runtime] def noSubmission(tool: String, lastError: Option[String], rejections: Int): String =
    if rejections > 0 then
      s"the agent's `$tool` submissions never matched the required schema after $rejections attempt(s)" +
        lastError.fold("")(e => s"; last error: $e")
    else s"the agent finished without calling `$tool`"

  // -- prompts -----------------------------------------------------------------------

  /** The loop section appended to a worker's system prompt: everything the worker needs
    * to know that is not in the base prompt, ordered from the standing (what the loop is
    * for) to the immediate (why the last attempt was rejected).
    *
    * The lineage is a digest — the last [[LineageDigest]] accepted generations, one line
    * each — while the newest accepted generation's description is reproduced IN FULL,
    * because that one is a handoff written for this reader by the agent that came
    * before, and summarising it would throw away the thing it was written for.
    */
  private[runtime] def workerSection(
      loopId: String,
      goal: String,
      rubric: String,
      gen: Int,
      maxGenerations: Int,
      attempt: Int,
      maxAttempts: Int,
      lineage: List[GenerationRecord],
      knowledge: String,
      feedback: Option[String]
  ): String =
    val parts = List.newBuilder[String]
    parts += s"## The loop you are working in"
    parts +=
      s"""You are working on ONE generation of the refinement loop '$loopId'. A loop pursues a
         |single goal over many generations; each one starts from the tree the last ACCEPTED
         |generation left behind and tries to improve on it. Your changes go in the working tree
         |as ordinary edits — the host snapshots the tree when you submit, so you neither commit
         |nor stash anything.
         |
         |Goal: $goal
         |
         |How a generation is judged: $rubric
         |
         |You are generation $gen of at most $maxGenerations, attempt $attempt of $maxAttempts within it.
         |Two gates stand between your work and the lineage: a mechanical checker written in Scala
         |when the loop was defined, and then an evaluator agent reading the rubric. Failing either
         |costs an attempt; running out of attempts abandons the generation and the tree is rolled
         |back to the last accepted state.""".stripMargin
    parts +=
      s"""### Delegating
         |
         |You may orchestrate from inside this generation: `lib.wf.start` fans work out to
         |sub-agents, and `lib.team` hires members that hold a thread of reasoning across evals.
         |Both are bounded by this generation — anything you start ends when the generation does,
         |and team members you create are retired then, so do not plan work that outlives your
         |submission. A member id stays reserved for the rest of the session even after the
         |member is retired, so name yours for this generation (`scout-gen-$gen`); a name an
         |earlier generation used is refused.
         |You may NOT start or amend a loop from here; loops do not nest.""".stripMargin
    parts += lineageSection(lineage)
    lineage.lastOption.foreach: latest =>
      parts += s"### Handoff from generation ${latest.gen}\n\n${latest.description}"
    if knowledge.trim.nonEmpty then
      parts += s"### What this loop has learned so far\n\n${knowledge.trim}"
    feedback.filter(_.trim.nonEmpty).foreach: text =>
      parts += s"### Why the previous attempt was rejected\n\n${text.trim}"
    parts.result().mkString("\n\n")

  /** The accepted lineage, one line per generation, newest last. */
  private[runtime] def lineageSection(lineage: List[GenerationRecord]): String =
    if lineage.isEmpty then
      "### Accepted so far\n\nNothing has been accepted yet: you are the first generation, working " +
        "from the loop's baseline."
    else
      val shown = lineage.takeRight(LineageDigest)
      val omitted = lineage.length - shown.length
      val head =
        if omitted <= 0 then "### Accepted so far"
        else s"### Accepted so far (the most recent ${shown.length}; $omitted earlier ones omitted)"
      s"$head\n\n${shown.map(digestLine).mkString("\n")}"

  private def digestLine(record: GenerationRecord): String =
    val summary = oneLine(record.description)
    val measured = if record.metrics.isEmpty then "" else s" [${metricsLine(record.metrics)}]"
    s"- gen ${record.gen}: $summary$measured"

  /** Metrics as a compact, stable one-liner. */
  private[runtime] def metricsLine(metrics: Map[String, Double]): String =
    metrics.toList.sortBy(_._1).map((k, v) => s"$k=${trimNumber(v)}").mkString(", ")

  private def trimNumber(v: Double): String =
    if v == v.floor && v.abs < 1e15 then v.toLong.toString else v.toString

  private def oneLine(text: String): String =
    val flat = text.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")
    if flat.length > 160 then flat.take(157) + "…" else flat

  /** The user message that opens a generation. */
  private[runtime] def workerTask(loopId: String, gen: Int): String =
    s"Work generation $gen of loop '$loopId'. Make the improvement the goal asks for, leave it " +
      "in the working tree, then call `submit_generation` exactly once."

  /** The user message that opens a retry, carrying the rejection verbatim. */
  private[runtime] def retryMessage(gen: Int, attempt: Int, maxAttempts: Int, rejection: String): String =
    s"Attempt $attempt of generation $gen was rejected:\n\n$rejection\n\n" +
      s"Your work is still in the working tree. Address what was called out and call " +
      s"`submit_generation` again — this is attempt ${attempt + 1} of $maxAttempts, and running out " +
      "abandons the generation."

  /** The evaluator's role, appended to its system prompt. */
  private[runtime] def evaluatorSection(loopId: String, goal: String, rubric: String): String =
    s"""## What you are judging
       |
       |You are the judgement gate of the refinement loop '$loopId'. A worker agent has produced a
       |candidate generation and it has already passed the loop's mechanical checker; your job is to
       |decide whether it should become the new standard every later generation builds on, and to say
       |why in terms the worker can act on if it should not.
       |
       |The loop's goal: $goal
       |
       |The rubric you judge against: $rubric
       |
       |You may read the working tree, which holds the candidate exactly as the worker left it — but
       |judge it, do not change it. Anything you write there is discarded before the next generation
       |starts. Be strict about the rubric and lenient about style: a generation that moves the goal
       |forward and passes the rubric is worth accepting even if you would have done it differently,
       |because the loop improves by accumulating accepted work.
       |
       |Reserve `goalReached` for the goal being genuinely met — it STOPS the loop.""".stripMargin

  /** The user message carrying one candidate's whole case: what was measured, what the
    * worker says it did, and the patch. */
  private[runtime] def evaluatorCase(
      gen: Int,
      rubric: String,
      lineage: List[GenerationRecord],
      report: CheckReport,
      artifact: Json,
      description: String,
      diff: String
  ): String =
    val (patch, truncated) =
      if diff.length <= MaxDiffChars then (diff, false) else (diff.take(MaxDiffChars), true)
    val patchBody =
      if patch.trim.isEmpty then "(the candidate changed nothing in the working tree)"
      else patch + (if truncated then s"\n[patch truncated at $MaxDiffChars characters]" else "")
    val measured = if report.metrics.isEmpty then "(the checker measured nothing)" else metricsLine(report.metrics)
    s"""Judge generation $gen against the rubric:
       |
       |$rubric
       |
       |${lineageSection(lineage)}
       |
       |### The checker's report
       |
       |It passed. Measured: $measured
       |
       |### The artifact the worker submitted
       |
       |${artifact.render}
       |
       |### The worker's own account
       |
       |$description
       |
       |### The change, as a patch against the generation it builds on
       |
       |```diff
       |$patchBody
       |```
       |
       |Decide, then call `submit_verdict`.""".stripMargin

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

  /** The system notice the lead receives when an amendment is attached. */
  def amendedNotice(loopId: String, version: Int): String =
    s"Loop '$loopId' has a new definition (version $version): its checker, goal, rubric and budgets " +
      "are the ones you just wrote. A generation already in flight finishes under the definition it " +
      "started with; the new checker judges the next one."

  /** The system notice the lead receives when an amendment is refused. The loop is
    * untouched either way, which is the part worth saying first. */
  def amendRefusedNotice(loopId: String, reason: String): String =
    s"Loop '$loopId' was NOT amended, and is exactly as it was.\n$reason"

  /** The system notice the lead receives when a loop is retuned in place. */
  def reconfiguredNotice(loopId: String, goal: Option[String], rubric: Option[String], budgets: Option[Budgets]): String =
    val changed = List(
      goal.map(g => s"goal: $g"),
      rubric.map(r => s"rubric: $r"),
      budgets.map(b =>
        s"budgets: at most ${b.maxGenerations} generations, patience ${b.patience}, " +
          s"${b.maxAttemptsPerGeneration} attempts per generation")
    ).flatten
    s"Loop '$loopId' was reconfigured; it keeps its checker and its whole history. " +
      s"Now in force from its next prompt:\n${changed.map("- " + _).mkString("\n")}"

  /** The system notice the lead receives when a loop from an earlier session is picked
    * up. It says where the loop stands, since the lead may never have seen it run. */
  def adoptedNotice(loopId: String, state: LoopState, fromDeadSession: Boolean): String =
    val where =
      state.latestAccepted match
        case Some(record) =>
          val measured = if record.metrics.isEmpty then "" else s" (${metricsLine(record.metrics)})"
          s"Its lineage stands at generation ${record.gen}$measured."
        case None => "Nothing has been accepted on it yet."
    val gap =
      if fromDeadSession then
        " Its previous session ended while it was still running, which is recorded on the ledger as " +
          "the gap it was; whatever generation was in flight is rescued and abandoned before it goes on."
      else ""
    s"Loop '$loopId' was picked up from an earlier session and is running again. $where$gap"

  /** The system notice the lead receives when a loop cannot be picked up. */
  def adoptionRefusedNotice(loopId: String, reason: String): String =
    s"Loop '$loopId' could NOT be resumed and stays where it is.\n$reason"

  /** How an artifact schema that no longer matches the ledger's is reported. Both are
    * quoted: the difference is the whole message, and only the reader can see which of
    * the two is the one that moved. */
  def driftDetail(recorded: Json, derived: Json): String =
    "the definition's artifact type no longer matches the one this loop's lineage was built on, so " +
      "its generations could not be compared with each other. Start a new loop for the new shape.\n" +
      s"The ledger records: ${recorded.render}\nThe definition now derives: ${derived.render}"

  def resumedNotice(loopId: String): String =
    s"Loop '$loopId' is running again."

  /** The system notice the lead receives when a generation joins the lineage. */
  def acceptedNotice(loopId: String, gen: Int, metrics: Map[String, Double], goalReached: Boolean): String =
    val measured = if metrics.isEmpty then "" else s" Measured: ${metricsLine(metrics)}."
    val ending = if goalReached then " The evaluator judged the goal reached, so the loop is stopping." else ""
    s"Loop '$loopId' accepted generation $gen.$measured$ending"

  /** The system notice the lead receives when a generation is thrown away. */
  def abandonedNotice(loopId: String, gen: Int, attempts: Int, rescueId: Option[String], why: String): String =
    val tries = if attempts == 1 then "1 attempt" else s"$attempts attempts"
    val reason = if why.trim.isEmpty then "" else s" Last rejection: ${why.trim}"
    val rescue = rescueId.fold("")(id => s" What it left on disk is kept as snapshot '$id'.")
    s"Loop '$loopId' abandoned generation $gen after $tries; the tree is back to the last accepted state." +
      s"$reason$rescue"

  /** The system notice the lead receives when the driver parks the loop itself. */
  def parkedForNotice(loopId: String, reason: ParkReason): String =
    val tail = reason match
      case ParkReason.GoalReached =>
        "the evaluator judged the goal reached"
      case ParkReason.BudgetExhausted =>
        "it has started as many generations as its budget allows"
      case ParkReason.PatienceExhausted =>
        "too many generations in a row were abandoned; the approach may need rethinking"
      case ParkReason.UserRequested => "you asked it to"
      case ParkReason.ApiFailure(d) => s"the model API kept failing ($d)"
      case ParkReason.Anomaly(d)    => s"the driver hit something it could not make sense of: $d"
    s"Loop '$loopId' parked: $tail. It keeps its whole history; resume it with " +
      s"lib.loop.get(\"$loopId\").resume()."

  // Where a loop stands is the loop PANEL's job — it draws the phase, the generation
  // strip and the stage in flight, live — and what a loop does is the lead's, through
  // the system notices above. So `onNotice` carries neither: it pins a line above the
  // input box until the session ends, which is right for a warning that has nowhere
  // else to surface and wrong for anything that passes. The one such warning is below.
  private[runtime] def gitlinkNotice(loopId: String, paths: List[String]): String =
    s"[loop] '$loopId' could not restore ${paths.length} submodule(s) — ${paths.mkString(", ")} — " +
      "they are left exactly as they are"

  /** The detail an ignored-file collision parks with. It names the files themselves,
    * because those are what a human needs to see to decide what to do. */
  private[runtime] def clobberDetail(what: String, paths: List[String]): String =
    s"$what would have destroyed ignored files no snapshot holds, so the tree was left " +
      s"untouched: ${paths.mkString(", ")}"

  /** A checker's reasons, as the worker is shown them. */
  private[runtime] def reasonsText(reasons: List[String]): String =
    if reasons.isEmpty then "The checker rejected the candidate without giving a reason."
    else s"The loop's checker rejected it:\n${reasons.map("- " + _).mkString("\n")}"

  /** An evaluator's feedback, as the worker is shown it. */
  private[runtime] def verdictText(feedback: String): String =
    if feedback.trim.isEmpty then "The evaluator rejected the candidate without giving a reason."
    else s"The evaluator rejected it:\n${feedback.trim}"

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
