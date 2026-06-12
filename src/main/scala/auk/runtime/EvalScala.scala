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
      "Timeout in milliseconds before the evaluation is aborted. Defaults to " +
        "30000 and is capped at 600000. Aborting kills the REPL session, so " +
        "accumulated definitions are lost."
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
final class EvalScala(repl: ScalaRepl) extends Tool:
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
      val timeout = params.timeoutMs.getOrElse(DefaultTimeoutMs).max(1).min(MaxTimeoutMs)
      render(repl.eval(params.code, timeout))

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
        val text = ReplProtocol.stripAnsi(sections.mkString)
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
  /** Timeout applied when the caller does not specify one. */
  val DefaultTimeoutMs = 30_000

  /** Upper bound on any requested timeout, to keep a turn from hanging. */
  val MaxTimeoutMs = 600_000

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
