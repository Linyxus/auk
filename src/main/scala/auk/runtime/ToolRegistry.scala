package auk.runtime

import scala.util.control.NonFatal
import gears.async.{Async, Future}

import auk.llm.tools.{Tool, RuntimeContext, ToolResult}
import auk.llm.endpoint.{ToolSchema, Content}
import auk.utils.Unicode

/** The tool-calling runtime: the set of tools a model may invoke, plus the
  * machinery to advertise them and to route the model's calls back to them.
  *
  * It collapses three responsibilities that were previously scattered through
  * the agent loop into one seam:
  *
  *   1. '''Advertise''' — [[schemas]] renders every tool into the endpoint's
  *      wire format for `LLMConfig.tools`.
  *   2. '''Dispatch''' — [[dispatch]] takes one model tool call, finds the
  *      matching tool, decodes its arguments, runs it under the shared
  *      [[RuntimeContext]], and packages the outcome as a `ToolResult` content
  *      block (carrying the `isError` flag and the original tool-use id).
  *   3. '''Fan out''' — [[runToolCalls]] runs every call in an assistant
  *      message concurrently and collects the results in order.
  *
  * Unknown tool names and exceptions thrown by a tool are turned into error
  * results rather than propagated, so one bad call can't abort the turn.
  */
final class ToolRegistry(val tools: List[Tool], extra: () => List[Tool] = () => Nil):
  private val byName: Map[String, Tool] =
    tools.map(t => t.name -> t).toMap

  /** Look up a tool by the name the model uses. Static tools always win: only if
    * no static tool matches is the dynamic supplier consulted (a fresh snapshot,
    * so late-added tools resolve without rebuilding the registry). */
  def get(name: String): Option[Tool] =
    byName.get(name).orElse(extra().find(_.name == name))

  /** Every tool's advertisement, for `LLMConfig.tools`: the static tools in
    * declaration order followed by the dynamic supplier's current snapshot. The
    * supplier is evaluated fresh on each call, and the engine re-reads `schemas`
    * every round, so tools that appear after startup (e.g. MCP tools once their
    * server responds) are advertised on the next round with no engine changes. */
  def schemas: List[ToolSchema] = (tools ++ extra()).map(ToolBridge.toToolSchema)

  /** Run a single model tool call, returning the tool's full [[ToolResult]] —
    * including its `metadata` side-channel (token totals, exit codes, …).
    *
    * Never throws: a missing tool or a thrown exception becomes an error
    * [[ToolResult]].
    */
  def run(call: Content.ToolUse)(using RuntimeContext, Async): ToolResult =
    val result = get(call.name) match
      case None =>
        ToolResult.error(s"unknown tool '${call.name}'")
      case Some(tool) =>
        try tool.call(call.input)
        catch
          case NonFatal(e) =>
            ToolResult.error(s"tool '${call.name}' failed: ${e.getMessage}")
    // Scrub lone UTF-16 surrogates from tool output here — the one point every
    // caller funnels through (the main loop's `runTools` and sub-agents' `dispatch`
    // both call `run`). `eval_scala` can print a lone surrogate, which can't be
    // JSON-encoded for the API; left in the history it 400s every later request,
    // wedging the conversation. See [[auk.utils.Unicode]].
    result.copy(output = Unicode.replaceLoneSurrogates(result.output))

  /** Run a single model tool call to completion, as a result content block
    * (the metadata is dropped — see [[run]] to keep it).
    *
    * Never throws: a missing tool or a thrown exception becomes an error
    * [[ToolResult]] addressed to the same `toolUseId`.
    */
  def dispatch(call: Content.ToolUse)(using RuntimeContext, Async): Content.ToolResult =
    val result = run(call) // `run` already scrubbed the output of lone surrogates
    Content.ToolResult(call.id, result.output, isError = result.isError)

  /** Run every tool call in `toolUses` concurrently, collecting results in the
    * original order. Concurrency is safe because tools are stateless and share
    * only the read-mostly [[RuntimeContext]]; order is preserved so results
    * line up with the calls the model made.
    */
  def runToolCalls(
      toolUses: List[Content.ToolUse]
  )(using RuntimeContext, Async.Spawn): List[Content.ToolResult] =
    toolUses.map(tu => Future(dispatch(tu))).map(_.await)

object ToolRegistry:
  /** A registry holding the given (static) tools. */
  def of(tools: Tool*): ToolRegistry = new ToolRegistry(tools.toList)

  /** A registry whose `static` tools are joined by whatever `extra` yields when
    * asked — for a long-lived agent (the main session) that gains tools after
    * startup, such as MCP tools once their server responds. Static tools take
    * precedence on name lookup; the supplier is re-read each round. */
  def withExtra(extra: () => List[Tool])(static: Tool*): ToolRegistry =
    new ToolRegistry(static.toList, extra)
