package auk.runtime

import scala.util.control.NonFatal
import gears.async.{Async, Future}

import auk.llm.tools.{Tool, RuntimeContext, ToolResult}
import auk.llm.endpoint.{ToolSchema, Content}

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
final class ToolRegistry(val tools: List[Tool]):
  private val byName: Map[String, Tool] =
    tools.map(t => t.name -> t).toMap

  /** Look up a tool by the name the model uses. */
  def get(name: String): Option[Tool] = byName.get(name)

  /** Every tool's advertisement, in declaration order, for `LLMConfig.tools`. */
  def schemas: List[ToolSchema] = tools.map(ToolBridge.toToolSchema)

  /** Run a single model tool call to completion, as a result content block.
    *
    * Never throws: a missing tool or a thrown exception becomes an error
    * [[ToolResult]] addressed to the same `toolUseId`.
    */
  def dispatch(call: Content.ToolUse)(using RuntimeContext, Async): Content.ToolResult =
    val result =
      get(call.name) match
        case None =>
          ToolResult.error(s"unknown tool '${call.name}'")
        case Some(tool) =>
          try tool.call(call.input)
          catch
            case NonFatal(e) =>
              ToolResult.error(s"tool '${call.name}' failed: ${e.getMessage}")
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
  /** A registry holding the given tools. */
  def of(tools: Tool*): ToolRegistry = new ToolRegistry(tools.toList)
