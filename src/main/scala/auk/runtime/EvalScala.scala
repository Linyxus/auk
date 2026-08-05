package auk.runtime

import gears.async.{Async, Future}

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}
import auk.platform.js.Interop
import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** Arguments for the [[EvalScala]] tool. */
case class EvalScalaParams(
    @desc(
      "Scala 3 code to evaluate as one REPL entry. It may contain several " +
        "statements and definitions; the value of the trailing expression is " +
        "rendered back."
    )
    code: String,
    @desc(
      "Optional timeout in milliseconds. By default there is NO timeout — the " +
        "evaluation runs until it completes (a workflow may take minutes). " +
        "Set this only to bound a " +
        "specific call. Hitting a timeout kills the REPL session, so accumulated " +
        "definitions are lost."
    )
    timeoutMs: Option[Int] = None
) derives ToolInput

/** Evaluate Scala code in a persistent REPL session.
  *
  * The heavy lifting lives in [[auk.runtime.repl.ScalaRepl]] (worker lifecycle,
  * serialisation, timeouts); this tool guards the call — empty code, approval,
  * timeout capping — and renders the structured response for the model:
  * captured stdout first (what the code printed), then the REPL's own
  * rendering (`val xs: List[Int] = …` or a compile diagnostic), then — when the
  * line failed and that rendering does not already say why ([[shownIn]]) — an
  * `error:` line naming the reason, then a labelled stderr block. ANSI colour is stripped and the result is truncated
  * past [[EvalScala.MaxOutputBytes]].
  *
  * Result conventions:
  *   - `ok` response            → success; output as described above.
  *   - compile failure          → `isError`; the diagnostic is the output.
  *   - runtime failure          → `isError`; whatever the line rendered before
  *     it threw, then the `error:` line carrying the exception.
  *   - timed out                → `isError`; the session was killed and the
  *     next call starts fresh (flagged by a leading note on that call).
  *   - a death the worker can account for → a trailing `liveness: …` line on
  *     that timeout or failure, saying what the worker was doing when the
  *     request died. Present only for a worker that speaks the liveness
  *     protocol; a legacy one is rendered exactly as it always was.
  *   - [[ToolResult.metadata]] carries `stateVersion`, `timedOut`, and
  *     `restarted` — plus [[ScalaRepl.MetaLiveness]] carrying that same
  *     sentence on a diagnosed death.
  *
  * While the eval is outstanding this tool also *watches* it: a poller fiber
  * reads [[ScalaRepl.liveness]] about once a second and reports it through
  * [[RuntimeContext.progress]], so a UI can say what a long eval looks like it
  * is doing instead of only showing a spinner. A legacy worker has no liveness
  * to read, and then not a single update is emitted.
  *
  * Evaluated code can do anything the agent process can, so it consults
  * [[RuntimeContext.approvals]] with the full code as the summary.
  */
final class EvalScala(
    replRef: () => ScalaRepl,
    bridge: Option[WorkflowBridge] = None,
    loopBridge: Option[LoopBridge] = None
) extends Tool:
  import EvalScala.*

  /** Read fresh on every call: the lead session is swapped by a successful
    * skill change (see [[auk.runtime.skills.SkillManager]]), and this
    * indirection is what makes the swap reach the tool. */
  private def repl: ScalaRepl = replRef()

  type Params = EvalScalaParams

  val name = "eval_scala"

  val description = Description

  val input: ToolInput[EvalScalaParams] = ToolInput[EvalScalaParams]

  def execute(params: EvalScalaParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    if params.code.trim.isEmpty then ToolResult.error("empty code")
    else if !ctx.approvals.request(ApprovalRequest(name, params.code)) then
      ToolResult.error(s"code evaluation not approved")
    else
      // No default and no cap: a long eval or workflow runs until it finishes or
      // the user interrupts (Ctrl+C). A caller may still set a positive timeout
      // to bound a specific call.
      val timeout = params.timeoutMs.filter(_ > 0)
      // The eval owns this fiber until the worker answers, so what the worker is
      // doing meanwhile can only be observed from a second one. `Async.group`
      // is what bounds it: when its block's last expression completes — the eval
      // returning, timing out, or throwing — the group cancels every child still
      // running and waits for it to unwind, so the poller cannot outlive the
      // eval it describes. The `finally` only makes that immediate and explicit.
      val result = Async.group:
        val poller = Future(pollLiveness())
        try repl.eval(params.code, timeout)
        finally poller.cancel()
      // `wf.start` launches a BACKGROUND run and returns immediately; it prints an
      // in-band marker line carrying the run id. We don't wait for the run (it
      // reports its result later via a system notice) — we just announce each
      // run's source to the dashboard and render the eval's value (the
      // `WorkflowRun(...)` handle the model stores).
      bridge.foreach(b => workflowRunIds(result).foreach(runId => b.announceCode(runId, params.code)))
      // `lib.loop.start` prints its own marker. The whole eval is the loop's
      // definition — the checker is a closure that cannot be sent over the bridge's
      // socket — so the source goes over now that the eval has finished, and the
      // bridge validates it before the loop exists at all. This blocks: the verdict
      // belongs in the same turn as the code that caused it.
      loopBridge.foreach(b => loopIds(result).foreach(loopId => b.announceDef(loopId, params.code)))
      // A loop handle's `amend` travels the same way and for the same reason — a redefinition
      // is a checker closure too — and blocks for the same one: whether the amendment
      // was accepted belongs in the turn that wrote it.
      loopBridge.foreach(b => loopAmendIds(result).foreach(loopId => b.announceAmend(loopId, params.code)))
      render(result)

  /** Watch the outstanding eval, reporting what the worker looks like to be
    * doing until this fiber is cancelled.
    *
    * Every poll that finds a state reports it, not only the ones that change
    * it: `elapsedMs` grows between two identical states, and a live counter is
    * most of what makes a slow eval bearable to watch. A poll that finds `None`
    * reports nothing at all — that is a legacy worker (or a moment with no
    * request in flight), and for those this tool must be as silent as it was
    * before liveness existed.
    *
    * Sleeping first rather than last means the poller costs a fast eval no
    * update and no work; [[Interop.sleep]] is cancellation-safe, so the wait is
    * where the cancellation lands and the timer never outlives it. */
  private def pollLiveness()(using ctx: RuntimeContext, async: Async): Unit =
    while true do
      Interop.sleep(LivenessPollMs.toDouble)
      repl.liveness.foreach(state => ctx.reportProgress(livenessUpdate(state)))

  /** One liveness observation in [[ToolResult.metadata]]'s vocabulary. The
    * silence is carried only where it means something — a working worker's last
    * signal is by definition recent — the pid only while a worker is up to have
    * one, and the phase only once the worker has named one (never, for a worker
    * that does not report them). A named phase brings its own clock: how long
    * that stage has been running, which is a different question from how long
    * the request has, and the one a reader watching a slow eval is asking. */
  private def livenessUpdate(state: ScalaRepl.Liveness): Map[String, String] =
    val base = state match
      case ScalaRepl.Liveness.AwaitingAck(elapsedMs) =>
        Map(ScalaRepl.MetaState -> "awaiting-ack", ScalaRepl.MetaElapsedMs -> elapsedMs.toString)
      case ScalaRepl.Liveness.Working(elapsedMs) =>
        Map(ScalaRepl.MetaState -> "working", ScalaRepl.MetaElapsedMs -> elapsedMs.toString)
      case ScalaRepl.Liveness.Blocked(elapsedMs, silentForMs) =>
        Map(
          ScalaRepl.MetaState -> "blocked",
          ScalaRepl.MetaElapsedMs -> elapsedMs.toString,
          ScalaRepl.MetaSilentMs -> silentForMs.toString
        )
    val withPid = repl.workerPid.fold(base)(pid => base + (ScalaRepl.MetaWorkerPid -> pid.toString))
    repl.phase.fold(withPid): phase =>
      val withPhase = withPid + (ScalaRepl.MetaPhase -> phase)
      repl.phaseAgeMs.fold(withPhase)(ms => withPhase + (ScalaRepl.MetaPhaseMs -> ms.toString))

  /** The worker's account of how the request stood when it died, as its own
    * line under the failure it explains. Empty for a legacy worker, which has
    * nothing honest to say and therefore says nothing. */
  private def diagnosisLine(result: ScalaRepl.EvalResult): String =
    result.diagnosis.fold("")(d => s"\nliveness: $d")

  private def diagnosisMeta(result: ScalaRepl.EvalResult): Map[String, String] =
    result.diagnosis.fold(Map.empty[String, String])(d => Map(ScalaRepl.MetaLiveness -> d))

  private def render(result: ScalaRepl.EvalResult): ToolResult =
    val note =
      if result.restartedSession then
        "[note: the REPL session was restarted — definitions from earlier " +
          "eval_scala calls are gone]\n"
      else ""
    val restarted = result.restartedSession.toString
    result.status match
      case ScalaRepl.Status.Completed(r) =>
        val sections = List(
          r.stdout,
          r.output,
          r.error.filter(e => !r.ok && !shownIn(e, r.output)).map(e => s"error: $e\n").getOrElse(""),
          if r.stderr.isEmpty then "" else s"[stderr]\n${r.stderr}"
        ).filter(_.nonEmpty)
        val text = stripMarkers(ReplProtocol.stripAnsi(sections.mkString))
        val (kept, truncated) = truncate(text)
        val body = if kept.isEmpty then "(no output)" else kept
        ToolResult(
          output = note + body +
            (if truncated then s"\n[output truncated to $MaxOutputBytes bytes]" else ""),
          isError = !r.ok,
          metadata = Map(
            "stateVersion" -> r.stateVersion.toString,
            "timedOut" -> "false",
            "restarted" -> restarted
          )
        )
      case ScalaRepl.Status.TimedOut(ms) =>
        ToolResult.error(
          note + s"evaluation timed out after ${ms}ms and the REPL session was " +
            "killed; definitions accumulated so far are lost and the next call " +
            "starts fresh" + diagnosisLine(result),
          metadata = Map("timedOut" -> "true", "restarted" -> restarted) ++ diagnosisMeta(result)
        )
      case ScalaRepl.Status.Failed(reason) =>
        ToolResult.error(
          note + reason + diagnosisLine(result),
          metadata = Map("timedOut" -> "false", "restarted" -> restarted) ++ diagnosisMeta(result)
        )

  /** Whether the REPL's rendering of a failed line already says what went wrong.
    *
    * A failed eval reports the reason twice over: `output` holds the REPL's own
    * rendering and `error` holds a summary of it. For a COMPILE failure the
    * rendering is the diagnostic and the summary is a slice of it, so printing
    * both just repeats the message. For a RUNTIME failure they are unrelated —
    * the rendering is whatever the line defined before it threw and only the
    * summary names the exception — and an eval that defines something and then
    * throws (every `lib` refusal has this shape) used to reach the model as
    * `// defined case class Perf` with an error flag and no reason at all.
    *
    * Line by line and ANSI-stripped, because the diagnostic carries its message
    * inside a source-quoting gutter that the summary has no trace of. */
  private def shownIn(error: String, output: String): Boolean =
    val rendered = ReplProtocol.stripAnsi(output)
    ReplProtocol
      .stripAnsi(error)
      .linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .forall(rendered.contains)

  private def truncate(s: String): (String, Boolean) =
    if s.length <= MaxOutputBytes then (s, false)
    else (s.substring(0, MaxOutputBytes).nn, true)

object EvalScala:
  /** Fixed-session convenience — the common case for sub-agents, team members,
    * and tests, whose REPL never swaps. */
  def apply(
      repl: ScalaRepl,
      bridge: Option[WorkflowBridge] = None,
      loopBridge: Option[LoopBridge] = None
  ): EvalScala =
    new EvalScala(() => repl, bridge, loopBridge)

  /** The prefix `auk.library.Workflow.start` prints to stdout — as a line
    * `"$WorkflowStartMarker:$runId"` per launched run — so this tool knows a
    * workflow ran (and its run id) and can announce its source. Must match the
    * constant in `auk.library.Workflow`. */
  private[runtime] val WorkflowStartMarker: String = "auk:workflow:start"

  /** The same mechanism for `auk.library.LoopImpl.start`, whose marker line carries
    * the loop id: the eval's whole source is that loop's definition, so the tool
    * hands it to the loop bridge once the eval completes. Must match the constant in
    * `auk.library.LoopImpl`. */
  private[runtime] val LoopStartMarker: String = "auk:loop:start"

  /** The same again for `auk.library.LoopImpl.amend`, whose eval is a REDEFINITION of a
    * loop that already exists rather than a new one. Must match the constant in
    * `auk.library.LoopImpl`. */
  private[runtime] val LoopAmendMarker: String = "auk:loop:amend"

  /** Matches one start marker — marker, then `:` and the id (up to whitespace), then
    * an optional trailing newline. `group(1)` is the id. */
  private val MarkerRegex = (WorkflowStartMarker + ":(\\S+)\\n?").r
  private val LoopMarkerRegex = (LoopStartMarker + ":(\\S+)\\n?").r
  private val LoopAmendRegex = (LoopAmendMarker + ":(\\S+)\\n?").r

  /** Matches a marker of ANY kind, for stripping: no marker line, of any kind, may
    * reach the model. */
  private val AnyMarkerRegex =
    (s"(?:$WorkflowStartMarker|$LoopStartMarker|$LoopAmendMarker)" + ":\\S+\\n?").r

  /** The run ids of every `wf.start` in a completed eval (none for a non-workflow
    * eval), read from the marker lines in its captured stdout. */
  private[runtime] def workflowRunIds(result: ScalaRepl.EvalResult): List[String] =
    idsFrom(result, MarkerRegex)

  /** The loop ids of every `lib.loop.start` in a completed eval. */
  private[runtime] def loopIds(result: ScalaRepl.EvalResult): List[String] =
    idsFrom(result, LoopMarkerRegex)

  /** The loop ids of every `LoopHandle.amend` in a completed eval. */
  private[runtime] def loopAmendIds(result: ScalaRepl.EvalResult): List[String] =
    idsFrom(result, LoopAmendRegex)

  private def idsFrom(result: ScalaRepl.EvalResult, regex: scala.util.matching.Regex): List[String] =
    result.status match
      case ScalaRepl.Status.Completed(r) => regex.findAllMatchIn(r.stdout).map(_.group(1).nn).toList
      case _                             => Nil

  /** Strip every start-marker line (marker + id + trailing newline) from rendered
    * output so neither the marker nor the id leaks to the model. */
  private[runtime] def stripMarkers(text: String): String =
    AnyMarkerRegex.replaceAllIn(text, "")

  /** Rendered output is truncated past this many characters. */
  val MaxOutputBytes = 100_000

  /** How often the running eval's liveness is read. A second is fast enough
    * that a counter built from it looks live and a wedge is named while the
    * user is still asking why, and slow enough that watching a multi-minute
    * workflow costs nothing worth measuring. Reading is pull-based, so this
    * resolution is entirely this tool's choice — [[ScalaRepl]] runs no timer of
    * its own for it. */
  private[runtime] val LivenessPollMs = 1_000

  val Description: String =
    "Evaluate Scala 3 code in a persistent REPL session. Definitions — vals, " +
      "defs, classes, givens, imports — accumulate across calls, so later " +
      "calls can build on earlier ones. The code runs on Scala.js under " +
      "Node.js: JavaScript and Node APIs are reachable through " +
      "scala.scalajs.js interop, and a global `require` is available, e.g. " +
      "js.Dynamic.global.require(\"node:fs\").readFileSync(path, \"utf8\"). " +
      "The auk runtime library is preloaded and bound as `lib` by the session " +
      "preamble — its interface and the preamble are in the system prompt. " +
      "Compile errors come back as the tool result, so you can fix the code " +
      "and retry. The first call pays a couple of seconds of REPL startup. " +
      "Definitions here are SCRATCH — they die with the session. If you " +
      "define a helper and use it more than once, or get a fiddly procedure " +
      "working after retries, crystallise it with skill_save (see the Skills " +
      "section) so future sessions start with it."
