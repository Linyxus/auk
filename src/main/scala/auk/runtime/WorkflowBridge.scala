package auk.runtime

import scala.collection.mutable

import gears.async.{Async, CancellationException, Future, UnboundedChannel}

import auk.workflow.{Forest, NodeStatus, OrchestrationEvent, TranscriptEvent, WireCodec, WireMessage}
import auk.llm.provider.ModelSession
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, Schema, Json}
import auk.platform.js.SocketServer
import auk.runtime.repl.ScalaRepl
import auk.session.{JsonlLog, SessionProvider, SessionRef}

/** The host side of the workflow bridge: a Unix-domain-socket server the
  * orchestrator worker connects to (`auk.library.WorkflowClient`). It runs the
  * actual sub-agents — it owns the LLM endpoints, tools, and the orchestration
  * event stream — and answers each `agent_call`.
  *
  * Runs are background and concurrent: each `wf.start` opens its own connection
  * and tags every message with its (worker-minted) run id, so the bridge no
  * longer serialises one run at a time. Concurrency of *sub-agents* is still
  * capped by a global permit channel shared across runs; each sub-agent leases a
  * [[ScalaRepl]] from `pool` for its own `eval_scala`. A `schema` on a call turns
  * on typed output: a `submit_result` tool is injected whose params wrap the
  * requested schema, and the recorded args' `result` field is returned as the
  * value. When a run settles (by `done`, a dropped worker, or shutdown) the
  * bridge emits [[OrchestrationEvent.WorkflowFinished]] and calls [[onComplete]]
  * exactly once (the host wires that to a completion system notice).
  *
  * Pause/resume converge here from two command sources: the UI ([[pause]]/[[resume]]
  * via the `control` channel) and the model (`WorkflowRun.pause/resume`, arriving as
  * worker `pause`/`resume` request messages). Both reach the one [[handlePause]] /
  * [[handleResume]]; the host is the single lifecycle authority and answers the worker
  * with the `paused` / `relaunch` directives.
  */
final class WorkflowBridge(
    val socketPath: String,
    models: ModelSession,
    pool: ReplPool,
    baseTools: ScalaRepl => List[Tool],
    systemPrompt: String,
    context: RuntimeContext,
    onEvent: OrchestrationEvent => Unit,
    maxConcurrent: Int,
    onActivity: TranscriptEvent => Unit = _ => (),
    onComplete: (String, Either[String, String]) => Unit = (_, _) => (),
    maxResultRetries: Int = WorkflowBridge.MaxResultRetries,
    sessionRef: Option[SessionRef] = None,
    /** Out-of-band host notices (e.g. "run auto-paused after persistent API
      * errors") — surfaced to the user's UI, never to the model. */
    onNotice: String => Unit = _ => (),
    /** Each sub-agent's API retry schedule (tests shrink it). */
    retryDelaysMs: List[Long] = auk.llm.endpoint.Endpoint.RetryDelaysMs
):
  import WorkflowBridge.*

  private val incoming = UnboundedChannel[(SocketServer.Conn, Json)]()
  // UI-driven pause/resume requests, handled on the dispatch fiber (which has the
  // Async.Spawn scope a resume's worker launch needs); see [[start]].
  private val control = UnboundedChannel[Control]()
  private var server: SocketServer.Handle | Null = null
  // Background, concurrent runs: a connection is bound to its run on the run's
  // `hello`. `connRuns` maps a live connection to its run id (so a disconnect can
  // resolve which run dropped); `activeRuns` is the set of not-yet-settled runs —
  // removing from it is the guard that makes a run finish exactly once (the first
  // of done / disconnect / shutdown wins).
  private val connRuns = mutable.Map.empty[SocketServer.Conn, String]
  private val activeRuns = mutable.Set.empty[String]
  // Per-run in-memory state (nothing is persisted — resume re-runs the worker's
  // stored closure within this session). `runForests` is folded from every emitted
  // event; `runResults` is the sub-agent-output cache (a finished node's value, and
  // the resume short-circuit source); `runFibers` tracks in-flight sub-agent futures
  // so pause can cancel them; `pausedRuns` holds runs suspended by the UI (kept out
  // of `activeRuns` so neither shutdown nor the kept-open connection settles them).
  private val runForests = mutable.Map.empty[String, Forest]
  private val runResults = mutable.Map.empty[String, Map[String, Json]]
  private val runFibers = mutable.Map.empty[String, mutable.Set[Future[Unit]]]
  private val pausedRuns = mutable.Set.empty[String]

  /** Emit an orchestration event: fold it into the run's persisted forest, then
    * forward it to the live UIs. Every emission goes through here so `runForests`
    * (and thus the saved record) always matches what the UIs show. */
  private def publish(ev: OrchestrationEvent): Unit =
    runForests(ev.runId) = runForests.getOrElse(ev.runId, Forest.empty).update(ev)
    onEvent(ev)

  /** Record a finished sub-agent's result in the in-memory cache (the resume
    * short-circuit source). */
  private def recordResult(runId: String, nodeId: String, value: Json): Unit =
    runResults(runId) = runResults.getOrElse(runId, Map.empty) + (nodeId -> value)

  /** Initialise the run's (empty) result cache the first time the run is seen.
    * Idempotent — a resumed run keeps the cache it built before pausing. Called from
    * the first message that mentions the run: the worker sends its node/call messages
    * *before* `hello`, so this must run on a `call` too, not only on `hello`. */
  private def ensureRunLoaded(runId: String): Unit =
    if !runResults.contains(runId) then runResults(runId) = Map.empty

  /** Settle a run exactly once: emit `WorkflowFinished` (so a live UI drops it)
    * and call [[onComplete]] with the outcome. The `activeRuns` guard collapses
    * the done / disconnect / shutdown race to the first caller. */
  private def finishRun(runId: String, outcome: Either[String, String]): Unit =
    if runId.nonEmpty && activeRuns.remove(runId) then
      connRuns.filterInPlace((_, r) => r != runId)
      pausedRuns -= runId
      val summary = outcome.fold(identity, identity)
      publish(OrchestrationEvent.WorkflowFinished(runId, outcome.isRight, summarizeText(summary)))
      runFibers.remove(runId)
      onComplete(runId, outcome)

  /** Announce a run's source code (the `eval_scala` body) so the dashboard can show
    * it in the workflow-code tab. Not persisted — resume re-runs the worker's stored
    * closure, not this text. */
  def announceCode(runId: String, code: String): Unit =
    publish(OrchestrationEvent.WorkflowCode(runId, code))

  // -- pause / resume ---------------------------------------------------------

  /** Pause (kill) a run from the UI: non-blocking; handled on the dispatch fiber. */
  def pause(runId: String): Unit = control.sendImmediately(Control.Pause(runId))

  /** Resume a paused run from the UI: non-blocking; handled on the dispatch fiber. */
  def resume(runId: String): Unit = control.sendImmediately(Control.Resume(runId))

  private def handlePause(runId: String)(using Async): Unit =
    // Suspend the run: mark its in-flight nodes interrupted, hard-cancel their
    // sub-agent fibers, and signal the worker — but KEEP the connection open (it is
    // the run's control channel for resume) and keep its `connRuns` binding. Move it
    // out of `activeRuns` into `pausedRuns` so neither shutdown nor the kept-open
    // connection ever settles it as finished.
    if activeRuns.remove(runId) then
      pausedRuns += runId
      // Mark in-flight nodes interrupted (not failed) *before* cancelling, so this
      // dispatch fiber owns their final status synchronously — the cancelled
      // sub-agent fibers then unwind quietly (see handleCall's catch) rather than
      // racing to report a failure. Interrupted nodes re-run on resume.
      runForests.get(runId).foreach: f =>
        f.nodes
          .filter(n => n.status == NodeStatus.Running || n.status == NodeStatus.Queued)
          .foreach(n => publish(OrchestrationEvent.NodeInterrupted(runId, n.id)))
      runFibers.remove(runId).foreach(_.toList.foreach(_.cancel()))
      // Tell the worker it was paused so its `WorkflowRun` handle reports `Paused`
      // and it stops writing; the connection stays open to carry the later `resume`.
      connRuns.find(_._2 == runId).map(_._1).foreach: conn =>
        conn.write(Json.Obj(List("t" -> Json.Str("paused"), "run" -> Json.Str(runId))).render)
      publish(OrchestrationEvent.WorkflowPaused(runId))

  private def handleResume(runId: String)(using Async): Unit =
    // Resume by directing the run's still-open worker connection to relaunch: the
    // worker re-runs the stored closure, re-sending `hello` (which re-activates the
    // run) and re-issuing its node calls. Finished sub-agents short-circuit from
    // `runResults`; only interrupted ones re-run. The `pausedRuns.remove` guard makes
    // this the single point that fires one relaunch, whoever requested the resume.
    if pausedRuns.remove(runId) then
      connRuns.find(_._2 == runId).map(_._1).foreach: conn =>
        publish(OrchestrationEvent.WorkflowResumed(runId))
        conn.write(Json.Obj(List("t" -> Json.Str("relaunch"), "run" -> Json.Str(runId))).render)

  /** Bind the socket and start servicing calls. Spawns a consumer fiber in the
    * caller's scope; returns immediately. */
  def start(onReady: () => Unit = () => ())(using Async.Spawn): Unit =
    val permits = UnboundedChannel[Unit]()
    var i = 0
    while i < maxConcurrent.max(1) do
      permits.sendImmediately(())
      i += 1
    server = SocketServer.listen(socketPath, onReady): conn =>
      // A run id is read per-message; a dropped connection fails whatever run it
      // was bound to (on `hello`), if any.
      conn.onClose(() => connRuns.get(conn).foreach(runId => finishRun(runId, Left("workflow worker disconnected"))))
      conn.onLine: line =>
        Json.parse(line) match
          case Right(msg) => incoming.sendImmediately((conn, msg))
          case Left(_)    => ()
    Future:
      var running = true
      while running do
        Async.select(
          incoming.readSource.handle {
            case Right((conn, msg)) => dispatch(conn, msg, permits)
            case Left(_)            => running = false
          },
          control.readSource.handle {
            case Right(Control.Pause(runId))  => handlePause(runId)
            case Right(Control.Resume(runId)) => handleResume(runId)
            case Left(_)                      => ()
          }
        )

  private def dispatch(
      conn: SocketServer.Conn,
      msg: Json,
      permits: UnboundedChannel[Unit]
  )(using Async.Spawn): Unit =
    val runId = str(msg, "run")
    field(msg, "t") match
      case Some("hello") =>
        // Bind this connection to its run and track it as active. `hello` is the
        // run's first message (sent after a successful build), so binding here
        // means a failed build — which never sends hello — registers nothing.
        if runId.nonEmpty then
          connRuns(conn) = runId
          activeRuns += runId
          ensureRunLoaded(runId)
      case Some("group") =>
        publish(OrchestrationEvent.GroupDeclared(runId, str(msg, "id"), str(msg, "name"), str(msg, "desc"), opt(msg, "parent")))
      case Some("node") =>
        publish(OrchestrationEvent.NodeDeclared(runId, str(msg, "id"), opt(msg, "group"), strList(msg, "deps")))
      case Some("log") =>
        publish(OrchestrationEvent.Log(runId, str(msg, "msg")))
      case Some("call") =>
        handleCall(conn, runId, str(msg, "id"), str(msg, "prompt"), schemaField(msg), permits)
      case Some("done") =>
        finishRun(runId, if boolField(msg, "ok") then Right(str(msg, "value")) else Left(str(msg, "error")))
      // Programmatic lifecycle requests from `WorkflowRun.pause/resume`: the same
      // handlers the UI drives through `pause`/`resume`, so both controllers share one
      // state machine (the `activeRuns`/`pausedRuns` guards make any redundant or
      // wrong-state request a no-op).
      case Some("pause")  => handlePause(runId)
      case Some("resume") => handleResume(runId)
      case _              => ()

  private def handleCall(
      conn: SocketServer.Conn,
      runId: String,
      id: String,
      prompt: String,
      schemaJson: Json,
      permits: UnboundedChannel[Unit]
  )(using Async.Spawn): Unit =
    // Per-sub-agent log: tee this node's lifecycle + transcript to
    // `.auk/sessions/<session>/subagents/<run>__<node>.jsonl` as the same
    // `WireMessage` JSONL the dashboard consumes, so the file replays into a
    // forest/transcript later. The session is pinned now, so a later `/new` or
    // `/resume` can't split one node's log across two session dirs. Best-effort:
    // a write failure never disturbs the run. `None` (tests) disables it.
    val logPath: Option[String] = sessionRef.map: ref =>
      SessionRef.subagentLog(context.resolve(SessionProvider.RelativePath), ref.id, s"${runId}__$id")
    def log(m: WireMessage): Unit = logPath.foreach: p =>
      JsonlLog.append(p, WireCodec.encode(m))
      ()
    def emit(ev: OrchestrationEvent): Unit =
      publish(ev)
      log(WireMessage.Event(ev))
    def emitActivity(a: HeadlessAgent.Activity): Unit =
      val ev = transcriptOf(a, runId, id)
      onActivity(ev)
      log(WireMessage.Activity(ev))
    // The cache may not have been loaded yet — a `call` arrives before `hello`.
    ensureRunLoaded(runId)
    runResults.get(runId).flatMap(_.get(id)) match
      // Resume short-circuit: this node already finished in a prior run, so settle
      // it instantly from the cache — no queue, permit, REPL, or model call.
      case Some(value) =>
        emit(OrchestrationEvent.NodeStarted(runId, id, prompt))
        emit(OrchestrationEvent.NodeFinished(runId, id, true, summarize(value)))
        conn.write(resultMsg(id, value).render)
      case None =>
        // A resumed sub-agent re-runs an interrupted attempt from scratch; truncate
        // its log first so the fresh lifecycle/transcript replaces the discarded one
        // rather than splicing onto it (matches the dashboard's transcript reset).
        val wasInterrupted = runForests.get(runId).exists(_.nodes.exists(n => n.id == id && n.status == NodeStatus.Interrupted))
        if wasInterrupted then logPath.foreach(p => { JsonlLog.reset(p); () })
        // Admitted, but not yet running: emit `queued` now and `started` only once a
        // concurrency permit is in hand, so the UI shows the cap throttling at work
        // (queued agents are distinct from the ≤ maxConcurrent actually executing).
        emit(OrchestrationEvent.NodeQueued(runId, id))
        // Tracked so a pause can hard-cancel it; the whole set is dropped when the
        // run settles (cancelling an already-finished future is a harmless no-op).
        val fiber: Future[Unit] = Future:
          permits.read() // acquire a concurrency slot (backpressure)
          emit(OrchestrationEvent.NodeStarted(runId, id, prompt))
          val repl = pool.lease()
          try
            given RuntimeContext = context
            val submit = new SubmitResult(Schema.obj(List("result" -> jsonToSchema(schemaJson)), List("result")), schemaJson, maxResultRetries)
            val registry = ToolRegistry.of((baseTools(repl) :+ submit)*)
            var lastIn = 0L
            var lastOut = 0L
            var nudges = 0
            val o = HeadlessAgent.run(
              prompt,
              models,
              registry,
              systemPrompt,
              onRound = (in, out) =>
                lastIn = in; lastOut = out
                emit(OrchestrationEvent.NodeProgress(runId, id, in, out, None)),
              onTools = ts =>
                ts.filterNot(_ == "submit_result").headOption
                  .foreach(t => emit(OrchestrationEvent.NodeProgress(runId, id, lastIn, lastOut, Some(t)))),
              onActivity = a => emitActivity(a),
              // Nudge a model that ends without a result back toward submit_result,
              // but only while it still might help: not once a result is in hand, not
              // once invalid submissions are spent (we fail those), and at most
              // `maxResultRetries` times.
              finishGuard = () =>
                if submit.captured.isDefined || submit.rejections >= maxResultRetries || nudges >= maxResultRetries then None
                else { nudges += 1; Some(NudgeMessage) },
              // Stop the loop hard once too many invalid results have been submitted.
              haltAfterTools = () => submit.rejections >= maxResultRetries,
              retryDelaysMs = retryDelaysMs
            )
            o.llmError match
              case Some(err) if err.transient =>
                // The API kept failing through the whole retry schedule — a
                // provider outage, not this node's fault. Failing the node would
                // fail the workflow; instead suspend the run exactly as a user
                // pause would (results so far are kept, in-flight nodes —
                // including this one, still marked Running — become Interrupted
                // and re-run on resume). `pause` is the non-blocking control
                // message; the dispatch fiber owns the actual state change, so
                // this fiber just returns without settling the node.
                onNotice(s"Workflow $runId paused itself: the model API kept failing " +
                  s"(${err.description}). Resume it from the workflow page once the provider recovers.")
                pause(runId)
              case Some(err) =>
                emit(OrchestrationEvent.NodeFinished(runId, id, false, err.description))
                conn.write(errorMsg(id, err.description).render)
              case None =>
                // submit.captured holds the sub-agent's result value, set only once a
                // submission passed host-side schema validation (see SubmitResult).
                submit.captured match
                  case Some(value) =>
                    emit(OrchestrationEvent.NodeFinished(runId, id, true, summarize(value)))
                    recordResult(runId, id, value)
                    conn.write(resultMsg(id, value).render)
                  case None if submit.rejections >= maxResultRetries =>
                    // The sub-agent kept submitting results that don't match the
                    // schema; we've returned the parse error `maxResultRetries` times
                    // and given up. Fail the node with the last, specific reason.
                    val err = invalidResultError(id, submit.lastError, submit.rejections)
                    emit(OrchestrationEvent.NodeFinished(runId, id, false, err))
                    conn.write(errorMsg(id, err).render)
                  case None =>
                    // The sub-agent finished without ever submitting (even after being
                    // nudged). A plain String result can still be taken from its prose;
                    // any other type cannot, so fail this node with a clear, diagnosable
                    // error rather than sending prose that decodes to a cryptic
                    // "expected object" on the worker (failing the whole workflow opaquely).
                    if isPlainStringSchema(schemaJson) then
                      val value = Json.Str(o.finalText)
                      emit(OrchestrationEvent.NodeFinished(runId, id, true, summarize(value)))
                      recordResult(runId, id, value)
                      conn.write(resultMsg(id, value).render)
                    else
                      val err = noSubmitResultError(id, o.finalText)
                      emit(OrchestrationEvent.NodeFinished(runId, id, false, err))
                      conn.write(errorMsg(id, err).render)
          catch
            // A pause cancels this fiber: `handlePause` already marked the node
            // interrupted and closed the worker, so unwind quietly — don't report
            // it as a failure or write to the dead connection.
            case _: CancellationException => ()
            case t: Throwable =>
              val err = Option(t.getMessage).getOrElse("agent failed")
              emit(OrchestrationEvent.NodeFinished(runId, id, false, err))
              conn.write(errorMsg(id, err).render)
          finally
            pool.release(repl)
            permits.sendImmediately(()) // release the slot
        runFibers.getOrElseUpdate(runId, mutable.Set.empty) += fiber

  def close()(using Async): Unit =
    activeRuns.toList.foreach(id => finishRun(id, Left("workflow bridge closed")))
    if server != null then server.nn.close()
    pool.close()

object WorkflowBridge:
  /** UI-driven control requests, handled on the dispatch fiber. */
  private enum Control:
    case Pause(runId: String)
    case Resume(runId: String)

  /** The system-notice text a settled run is delivered to the agent as (the host
    * wires this into [[WorkflowBridge.onComplete]] → the steering inbox). Names the
    * run id so the agent can match it to its stored `WorkflowRun.id`, and carries
    * the full result/error — the same content the blocking API used to return. */
  def completionNotice(runId: String, outcome: Either[String, String]): String =
    outcome match
      case Right(value) =>
        val body = if value.isEmpty then "(no result)" else value
        s"Workflow $runId finished. Result:\n$body"
      case Left(error) =>
        s"Workflow $runId failed. Error: $error"

  /** A per-process temp socket path for the bridge (the server unlinks any stale
    * file on bind). */
  def defaultSocketPath(): String =
    val os = scala.scalajs.js.Dynamic.global.require("node:os")
    val pid = scala.scalajs.js.Dynamic.global.process.pid
    s"${os.tmpdir().asInstanceOf[String]}/auk-wf-$pid.sock"

  /** Cap shared by both result-recovery retries: how many invalid `submit_result`
    * submissions are tolerated (each answered with the parse error so the agent can
    * correct it), and how many times an agent that finishes without submitting is
    * nudged to do so. Beyond it, the node fails with a clear, specific reason. */
  val MaxResultRetries: Int = 5

  /** Appended as a user message when a sub-agent ends its turn without submitting a
    * result, to push it into calling `submit_result`. */
  private val NudgeMessage: String =
    "You ended your turn without calling the `submit_result` tool. You must call " +
      "`submit_result` exactly once, passing your final answer as its `result` field " +
      "and matching the requested schema. Do not answer in prose — call `submit_result` now."

  /** A clear, diagnosable error for a sub-agent that exhausted its result retries:
    * it kept submitting values that don't match the requested schema. */
  private def invalidResultError(id: String, lastError: Option[String], attempts: Int): String =
    val tail = lastError.fold("")(e => s"; last error: $e")
    s"sub-agent '$id' did not submit a result matching the requested schema after " +
      s"$attempts attempt(s)$tail"

  /** Tag a headless run's [[HeadlessAgent.Activity]] with its run + node id,
    * yielding the wire [[TranscriptEvent]] streamed to the web UI. Pure (tested). */
  private[runtime] def transcriptOf(a: HeadlessAgent.Activity, runId: String, nodeId: String): TranscriptEvent =
    a match
      case HeadlessAgent.Activity.Text(t)                 => TranscriptEvent.Said(runId, nodeId, t)
      case HeadlessAgent.Activity.Thinking(t)             => TranscriptEvent.Thought(runId, nodeId, t)
      case HeadlessAgent.Activity.ToolStarted(id, n, in)  => TranscriptEvent.ToolCalled(runId, nodeId, id, n, in)
      case HeadlessAgent.Activity.ToolEnded(id, out, err) => TranscriptEvent.ToolReturned(runId, nodeId, id, out, err)
      // A retry stall is prose in the transcript (no dedicated wire event): the
      // marker explains the silence and why the preceding partial repeats.
      case HeadlessAgent.Activity.Retrying(attempt, maxAttempts, delayMs, reason) =>
        TranscriptEvent.Said(
          runId,
          nodeId,
          s"\n[api error — retrying in ${delayMs / 1000}s (attempt $attempt/$maxAttempts): $reason]\n"
        )

  // -- message field accessors ------------------------------------------------

  private def field(j: Json, k: String): Option[String] = j match
    case o: Json.Obj => o.get(k).collect { case Json.Str(s) => s }
    case _           => None
  private def boolField(j: Json, k: String): Boolean = j match
    case o: Json.Obj => o.get(k).collect { case Json.Bool(b) => b }.getOrElse(false)
    case _           => false
  private def str(j: Json, k: String): String = field(j, k).getOrElse("")
  private def opt(j: Json, k: String): Option[String] = field(j, k)
  private def strList(j: Json, k: String): List[String] = j match
    case o: Json.Obj =>
      o.get(k) match
        case Some(Json.Arr(es)) => es.collect { case Json.Str(s) => s }
        case _                  => Nil
    case _ => Nil
  private def schemaField(j: Json): Json = j match
    case o: Json.Obj => o.get("schema").getOrElse(Json.Null)
    case _           => Json.Null

  // -- reply messages ---------------------------------------------------------

  private def resultMsg(id: String, value: Json): Json =
    Json.Obj(List("t" -> Json.Str("result"), "id" -> Json.Str(id), "ok" -> Json.Bool(true), "value" -> value))
  private def errorMsg(id: String, error: String): Json =
    Json.Obj(List("t" -> Json.Str("result"), "id" -> Json.Str(id), "ok" -> Json.Bool(false), "error" -> Json.Str(error)))

  private def summarize(v: Json): String =
    summarizeText(v.render)

  /** Truncate a rendered string for a short event summary (e.g. the
    * `WorkflowFinished` summary or a node result). */
  private def summarizeText(s: String): String =
    if s.length > 200 then s.take(200) + "…" else s

  /** True for a bare `String` result schema (`{type:"string"}` without an `enum`),
    * the one result type whose value can be salvaged from a sub-agent's prose when
    * it skips submit_result (an enum would need an exact-label match). */
  private def isPlainStringSchema(j: Json): Boolean = j match
    case o: Json.Obj => o.get("type").contains(Json.Str("string")) && o.get("enum").isEmpty
    case _           => false

  /** A clear, diagnosable error for a sub-agent that finished without submitting a
    * structured result, quoting (a prefix of) its prose answer. */
  private def noSubmitResultError(id: String, prose: String): String =
    val trimmed = prose.trim
    val shown =
      if trimmed.isEmpty then "(no message)"
      else if trimmed.length > 300 then trimmed.take(300) + "…"
      else trimmed
    s"sub-agent '$id' did not call submit_result and answered in prose instead; " +
      "expected a structured result matching the requested schema. The agent said: " + shown

  /** Convert the worker's JSON-schema fragment into the host's [[Schema]]. */
  def jsonToSchema(j: Json): Schema = j match
    case Json.Obj(fields) =>
      val m = fields.toMap
      Schema(
        `type` = m.get("type").collect { case Json.Str(s) => s },
        items = m.get("items").map(jsonToSchema),
        properties = m.get("properties") match
          case Some(Json.Obj(ps)) => ps.map((k, v) => k -> jsonToSchema(v))
          case _                  => Nil,
        required = m.get("required") match
          case Some(Json.Arr(rs)) => rs.collect { case Json.Str(s) => s }
          case _                  => Nil,
        enumValues = m.get("enum") match
          case Some(Json.Arr(es)) => es
          case _                  => Nil
      )
    case _ => Schema()

  /** A tool that captures the model's structured final result, validating it
    * against the requested schema before accepting it.
    *
    * Its parameters are the requested schema wrapped as `{ result: <schema> }`.
    * The wrapper decoder stays permissive (it accepts whatever the model sent) so
    * that [[execute]] always runs and can validate `result` itself: a value that
    * matches is recorded in [[captured]] (the unwrapped result); one that does not
    * is rejected with a precise, path-qualified error returned as an error
    * `ToolResult`, which the model sees and can correct. [[rejections]] counts
    * those misses so the bridge can stop after [[maxRetries]] and fail the node
    * cleanly instead of shipping a value the worker cannot decode. */
  final class SubmitResult(wrappedSchema: Schema, resultSchema: Json, maxRetries: Int) extends Tool:
    type Params = Json
    /** The validated result value (the `result` field), set once a submission passes. */
    var captured: Option[Json] = None
    /** How many submissions have been rejected as not matching the schema. */
    var rejections: Int = 0
    /** The most recent rejection message, for a clean node failure if retries run out. */
    var lastError: Option[String] = None
    val name = "submit_result"
    // The full result schema goes in the description because the advertised
    // (structured) tool schema flattens nested objects (see ToolBridge); the
    // description is the authoritative, depth-independent contract for `result`.
    val description =
      "Submit your final result. Call this exactly once when done, with a `result` " +
        "field that is a JSON value matching exactly this schema:\n" + resultSchema.render +
        "\nIf the value does not match, the call returns an error describing the " +
        "problem; fix it and call `submit_result` again."
    val input: ToolInput[Json] = ToolInput.instance(wrappedSchema)(j => Right(j))
    def execute(params: Json)(using RuntimeContext, Async): ToolResult =
      if captured.isDefined then ToolResult.ok("result already recorded")
      else
        val extracted: Either[String, Json] = params match
          case o: Json.Obj =>
            o.get("result") match
              case Some(v) => Right(v)
              case None    => Left("missing required field 'result'")
          case _ => Left("expected an object with a 'result' field")
        extracted.flatMap(v => ResultSchema.validate(resultSchema, v).map(_ => v)) match
          case Right(value) =>
            captured = Some(value)
            lastError = None
            ToolResult.ok("result recorded")
          case Left(err) =>
            rejections += 1
            lastError = Some(err)
            ToolResult.error(rejectionMessage(err))

    private def rejectionMessage(err: String): String =
      if rejections >= maxRetries then
        s"Your `result` is still invalid: $err. You have used all $maxRetries attempts; " +
          "no further submissions will be accepted."
      else
        s"Your `result` is invalid: $err. Correct it and call `submit_result` again " +
          s"(attempt $rejections of $maxRetries)."
