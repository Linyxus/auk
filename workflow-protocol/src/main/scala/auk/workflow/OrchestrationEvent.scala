package auk.workflow

/** Live structure + status of a running workflow's agent forest, emitted by the
  * host `WorkflowBridge` and folded into the TUI and the web UI.
  *
  * `runId` is the worker-minted id of the workflow run (a `wf.start` is a
  * background run, no longer tied to its launching `eval_scala` call), so a UI
  * keys the live forest by it. The graph is mostly known up front (eager +
  * single-threaded build), with `flatMap` frontiers declaring late.
  *
  * This type lives in the shared `workflow-protocol` module so the host (which
  * emits it), the TUI (which folds it), and the web UI (which receives it over
  * the wire) all agree on one definition. See [[WireCodec]] for the JSON form.
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
  /** The workflow's source code (the `eval_scala` body that started this run),
    * announced once by the host so the dashboard can show it. */
  case WorkflowCode(runId: String, code: String)
  /** The whole workflow settled — by `done`, a dropped worker, or shutdown. `ok`
    * is the terminal outcome and `summary` a short rendering of the result/error.
    * A live UI uses this to drop the run from its active panel (the full result is
    * delivered separately, e.g. as a system notice). */
  case WorkflowFinished(runId: String, ok: Boolean, summary: String)
