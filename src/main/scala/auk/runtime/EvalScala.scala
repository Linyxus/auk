package auk.runtime

import gears.async.Async

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, ApprovalRequest, desc}
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
  * rendering (`val xs: List[Int] = …` or a compile diagnostic), then a
  * labelled stderr block. ANSI colour is stripped and the result is truncated
  * past [[EvalScala.MaxOutputBytes]].
  *
  * Result conventions:
  *   - `ok` response            → success; output as described above.
  *   - compile/runtime failure  → `isError`; the diagnostic is the output.
  *   - timed out                → `isError`; the session was killed and the
  *     next call starts fresh (flagged by a leading note on that call).
  *   - [[ToolResult.metadata]] carries `stateVersion`, `timedOut`, and
  *     `restarted`.
  *
  * Evaluated code can do anything the agent process can, so it consults
  * [[RuntimeContext.approvals]] with the full code as the summary.
  */
final class EvalScala(repl: ScalaRepl, bridge: Option[WorkflowBridge] = None) extends Tool:
  import EvalScala.*

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
      // If this call carries a run id and a workflow bridge is wired, register the
      // run so the orchestrator worker's connection + events attribute to it.
      ctx.callId.foreach(id => bridge.foreach(_.beginRun(id)))
      val result = repl.eval(params.code, timeout)
      // A workflow eval returns immediately with a pending Future; `wf.start`
      // prints an in-band marker and reports the real result over the side
      // channel. When we see the marker, wait for the bridge's outcome and return
      // that instead of the REPL's `Future(<not completed>)` render.
      (ctx.callId, bridge) match
        case (Some(id), Some(b)) if startedWorkflow(result) =>
          b.awaitDone(id) match
            case Right(value) => ToolResult.ok(if value.isEmpty then "(workflow produced no result)" else value)
            case Left(err)    => ToolResult.error(s"workflow error: $err")
        case _ => render(result)

  private def render(result: ScalaRepl.EvalResult): ToolResult =
    val note =
      if result.restartedSession then
        "[note: the REPL session was restarted — definitions from earlier " +
          "eval_scala calls are gone]\n"
      else ""
    val restarted = result.restartedSession.toString
    result.status match
      case ScalaRepl.Status.Completed(r) =>
        // `output` already carries the full diagnostic when the line failed;
        // the short `error` summary is only needed when there is nothing else.
        val sections = List(
          r.stdout,
          r.output,
          r.error.filter(_ => !r.ok && r.output.isEmpty).map(e => s"error: $e\n").getOrElse(""),
          if r.stderr.isEmpty then "" else s"[stderr]\n${r.stderr}"
        ).filter(_.nonEmpty)
        val text = ReplProtocol.stripAnsi(sections.mkString).replace(WorkflowStartMarker, "")
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
            "starts fresh",
          metadata = Map("timedOut" -> "true", "restarted" -> restarted)
        )
      case ScalaRepl.Status.Failed(reason) =>
        ToolResult.error(
          note + reason,
          metadata = Map("timedOut" -> "false", "restarted" -> restarted)
        )

  private def truncate(s: String): (String, Boolean) =
    if s.length <= MaxOutputBytes then (s, false)
    else (s.substring(0, MaxOutputBytes).nn, true)

object EvalScala:
  /** Printed to stdout by `auk.library.Workflow.start` so this tool knows a
    * workflow ran and should await its result from the bridge (instead of the
    * REPL's immediate `Future(<not completed>)` render). Must match the constant
    * in `auk.library.Workflow`. */
  private[runtime] val WorkflowStartMarker: String = "auk:workflow:start"

  /** True if `result` is a completed eval whose captured stdout carries the
    * workflow marker. */
  private def startedWorkflow(result: ScalaRepl.EvalResult): Boolean =
    result.status match
      case ScalaRepl.Status.Completed(r) => r.stdout.contains(WorkflowStartMarker)
      case _                             => false

  /** Rendered output is truncated past this many characters. */
  val MaxOutputBytes = 100_000

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
      "and retry. The first call pays a couple of seconds of REPL startup."
