package auk.runtime

import gears.async.{Async, Future, UnboundedChannel}

import auk.workflow.{OrchestrationEvent, TranscriptEvent}
import auk.llm.provider.ModelSession
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, Schema, Json}
import auk.platform.js.SocketServer
import auk.runtime.repl.ScalaRepl

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
    maxResultRetries: Int = WorkflowBridge.MaxResultRetries
):
  import WorkflowBridge.*

  private val incoming = UnboundedChannel[(SocketServer.Conn, Json)]()
  private var server: SocketServer.Handle | Null = null
  // Background, concurrent runs: a connection is bound to its run on the run's
  // `hello`. `connRuns` maps a live connection to its run id (so a disconnect can
  // resolve which run dropped); `activeRuns` is the set of not-yet-settled runs —
  // removing from it is the guard that makes a run finish exactly once (the first
  // of done / disconnect / shutdown wins).
  private val connRuns = scala.collection.mutable.Map.empty[SocketServer.Conn, String]
  private val activeRuns = scala.collection.mutable.Set.empty[String]

  /** Settle a run exactly once: emit `WorkflowFinished` (so a live UI drops it)
    * and call [[onComplete]] with the outcome. The `activeRuns` guard collapses
    * the done / disconnect / shutdown race to the first caller. */
  private def finishRun(runId: String, outcome: Either[String, String]): Unit =
    if runId.nonEmpty && activeRuns.remove(runId) then
      connRuns.filterInPlace((_, r) => r != runId)
      val summary = outcome.fold(identity, identity)
      onEvent(OrchestrationEvent.WorkflowFinished(runId, outcome.isRight, summarizeText(summary)))
      onComplete(runId, outcome)

  /** Announce a run's source code (the `eval_scala` body) so the dashboard can
    * show it. A plain emission through `onEvent`, like the worker's own events. */
  def announceCode(runId: String, code: String): Unit =
    onEvent(OrchestrationEvent.WorkflowCode(runId, code))

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
        incoming.read() match
          case Right((conn, msg)) => dispatch(conn, msg, permits)
          case Left(_)            => running = false

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
      case Some("group") =>
        onEvent(OrchestrationEvent.GroupDeclared(runId, str(msg, "id"), str(msg, "name"), str(msg, "desc"), opt(msg, "parent")))
      case Some("node") =>
        onEvent(OrchestrationEvent.NodeDeclared(runId, str(msg, "id"), opt(msg, "group"), strList(msg, "deps")))
      case Some("log") =>
        onEvent(OrchestrationEvent.Log(runId, str(msg, "msg")))
      case Some("call") =>
        handleCall(conn, runId, str(msg, "id"), str(msg, "prompt"), schemaField(msg), permits)
      case Some("done") =>
        finishRun(runId, if boolField(msg, "ok") then Right(str(msg, "value")) else Left(str(msg, "error")))
      case _ => ()

  private def handleCall(
      conn: SocketServer.Conn,
      runId: String,
      id: String,
      prompt: String,
      schemaJson: Json,
      permits: UnboundedChannel[Unit]
  )(using Async.Spawn): Unit =
    // Admitted, but not yet running: emit `queued` now and `started` only once a
    // concurrency permit is in hand, so the UI shows the cap throttling at work
    // (queued agents are distinct from the ≤ maxConcurrent actually executing).
    onEvent(OrchestrationEvent.NodeQueued(runId, id))
    Future:
      permits.read() // acquire a concurrency slot (backpressure)
      onEvent(OrchestrationEvent.NodeStarted(runId, id, prompt))
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
            onEvent(OrchestrationEvent.NodeProgress(runId, id, in, out, None)),
          onTools = ts =>
            ts.filterNot(_ == "submit_result").headOption
              .foreach(t => onEvent(OrchestrationEvent.NodeProgress(runId, id, lastIn, lastOut, Some(t)))),
          onActivity = a => onActivity(transcriptOf(a, runId, id)),
          // Nudge a model that ends without a result back toward submit_result,
          // but only while it still might help: not once a result is in hand, not
          // once invalid submissions are spent (we fail those), and at most
          // `maxResultRetries` times.
          finishGuard = () =>
            if submit.captured.isDefined || submit.rejections >= maxResultRetries || nudges >= maxResultRetries then None
            else { nudges += 1; Some(NudgeMessage) },
          // Stop the loop hard once too many invalid results have been submitted.
          haltAfterTools = () => submit.rejections >= maxResultRetries
        )
        o.llmError match
          case Some(err) =>
            onEvent(OrchestrationEvent.NodeFinished(runId, id, false, err))
            conn.write(errorMsg(id, err).render)
          case None =>
            // submit.captured holds the sub-agent's result value, set only once a
            // submission passed host-side schema validation (see SubmitResult).
            submit.captured match
              case Some(value) =>
                onEvent(OrchestrationEvent.NodeFinished(runId, id, true, summarize(value)))
                conn.write(resultMsg(id, value).render)
              case None if submit.rejections >= maxResultRetries =>
                // The sub-agent kept submitting results that don't match the
                // schema; we've returned the parse error `maxResultRetries` times
                // and given up. Fail the node with the last, specific reason.
                val err = invalidResultError(id, submit.lastError, submit.rejections)
                onEvent(OrchestrationEvent.NodeFinished(runId, id, false, err))
                conn.write(errorMsg(id, err).render)
              case None =>
                // The sub-agent finished without ever submitting (even after being
                // nudged). A plain String result can still be taken from its prose;
                // any other type cannot, so fail this node with a clear, diagnosable
                // error rather than sending prose that decodes to a cryptic
                // "expected object" on the worker (failing the whole workflow opaquely).
                if isPlainStringSchema(schemaJson) then
                  val value = Json.Str(o.finalText)
                  onEvent(OrchestrationEvent.NodeFinished(runId, id, true, summarize(value)))
                  conn.write(resultMsg(id, value).render)
                else
                  val err = noSubmitResultError(id, o.finalText)
                  onEvent(OrchestrationEvent.NodeFinished(runId, id, false, err))
                  conn.write(errorMsg(id, err).render)
      catch
        case t: Throwable =>
          val err = Option(t.getMessage).getOrElse("agent failed")
          onEvent(OrchestrationEvent.NodeFinished(runId, id, false, err))
          conn.write(errorMsg(id, err).render)
      finally
        pool.release(repl)
        permits.sendImmediately(()) // release the slot

  def close()(using Async): Unit =
    activeRuns.toList.foreach(id => finishRun(id, Left("workflow bridge closed")))
    if server != null then server.nn.close()
    pool.close()

object WorkflowBridge:
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
