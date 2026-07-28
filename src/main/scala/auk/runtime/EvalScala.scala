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
final class EvalScala(replRef: () => ScalaRepl, bridge: Option[WorkflowBridge] = None) extends Tool:
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
      val result = repl.eval(params.code, timeout)
      // `wf.start` launches a BACKGROUND run and returns immediately; it prints an
      // in-band marker line carrying the run id. We don't wait for the run (it
      // reports its result later via a system notice) — we just announce each
      // run's source to the dashboard and render the eval's value (the
      // `WorkflowRun(...)` handle the model stores).
      bridge.foreach(b => workflowRunIds(result).foreach(runId => b.announceCode(runId, params.code)))
      render(result)

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
        val text = stripWorkflowMarkers(ReplProtocol.stripAnsi(sections.mkString))
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
  /** Fixed-session convenience — the common case for sub-agents, team members,
    * and tests, whose REPL never swaps. */
  def apply(repl: ScalaRepl, bridge: Option[WorkflowBridge] = None): EvalScala =
    new EvalScala(() => repl, bridge)

  /** The prefix `auk.library.Workflow.start` prints to stdout — as a line
    * `"$WorkflowStartMarker:$runId"` per launched run — so this tool knows a
    * workflow ran (and its run id) and can announce its source. Must match the
    * constant in `auk.library.Workflow`. */
  private[runtime] val WorkflowStartMarker: String = "auk:workflow:start"

  /** Matches one start marker — marker, then `:` and the run id (up to
    * whitespace), then an optional trailing newline. `group(1)` is the run id. */
  private val MarkerRegex = (WorkflowStartMarker + ":(\\S+)\\n?").r

  /** The run ids of every `wf.start` in a completed eval (none for a non-workflow
    * eval), read from the marker lines in its captured stdout. */
  private[runtime] def workflowRunIds(result: ScalaRepl.EvalResult): List[String] =
    result.status match
      case ScalaRepl.Status.Completed(r) => MarkerRegex.findAllMatchIn(r.stdout).map(_.group(1).nn).toList
      case _                             => Nil

  /** Strip every start-marker line (marker + run id + trailing newline) from
    * rendered output so neither the marker nor the run id leaks to the model. */
  private[runtime] def stripWorkflowMarkers(text: String): String =
    MarkerRegex.replaceAllIn(text, "")

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
