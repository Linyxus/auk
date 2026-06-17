package auk.agent

/** Live structure + status of a running workflow's agent forest, emitted by the
  * host `WorkflowBridge` and folded into the TUI (and, later, a WebUI).
  *
  * `runId` is the `eval_scala` tool-use id the workflow runs under, so a UI
  * attaches the forest to that tool block. The graph is mostly known up front
  * (eager + single-threaded build), with `flatMap` frontiers declaring late.
  */
enum OrchestrationEvent:
  /** The eval_scala run this event belongs to (the tool-use id). */
  def runId: String

  /** A group (phase) was declared; `parent` set for nested groups. */
  case GroupDeclared(runId: String, groupId: String, name: String, description: String, parent: Option[String])
  /** A node (sub-agent) entered the graph, in `group`, depending on `deps`. */
  case NodeDeclared(runId: String, nodeId: String, group: Option[String], deps: List[String])
  /** The node's sub-agent was admitted to the host (its `call` arrived) and is
    * waiting for a concurrency slot — it is not running yet. Distinguishes a
    * queued sub-agent from one actually executing under the concurrency cap. */
  case NodeQueued(runId: String, nodeId: String)
  /** The node's sub-agent acquired a slot and started running. */
  case NodeStarted(runId: String, nodeId: String, prompt: String)
  /** Live progress: cumulative tokens, and the currently running tool if any. */
  case NodeProgress(runId: String, nodeId: String, inputTokens: Long, outputTokens: Long, currentTool: Option[String])
  /** The node finished; `summary` is a short rendering of the result or error. */
  case NodeFinished(runId: String, nodeId: String, ok: Boolean, summary: String)
  /** A `log(...)` line from the workflow. */
  case Log(runId: String, message: String)
