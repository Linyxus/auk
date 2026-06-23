package auk.workflow

/** The live status of one sub-agent in a workflow forest. `Pending` = declared in
  * the graph; `Queued` = admitted, waiting for a concurrency slot; `Running` =
  * executing under the cap; `Interrupted` = was in flight when the run was paused
  * (killed, not failed) and will re-run on resume. */
enum NodeStatus:
  case Pending, Queued, Running, Done, Failed, Interrupted

/** A whole run's status. `Running` = live; `Paused` = killed by the user but
  * resumable (finished sub-agents are cached); `Done`/`Failed` = settled. A
  * paused run stays in the UI (with a resume affordance); a finished one drops. */
enum RunStatus:
  case Running, Paused, Done, Failed

/** One sub-agent node in a workflow forest (see [[Forest]]). `prompt` is the task
  * the sub-agent was started with (set on `NodeStarted`), shown above its
  * transcript in the web UI. */
final case class ForestNode(
    id: String,
    group: Option[String],
    deps: List[String],
    status: NodeStatus,
    inputTokens: Long = 0,
    outputTokens: Long = 0,
    currentTool: Option[String] = None,
    summary: Option[String] = None,
    prompt: Option[String] = None
)

/** A declared group (phase) in a workflow forest. */
final case class ForestGroup(id: String, name: String, description: String)

/** The live forest of a running workflow, identified by a run id. Folded from
  * [[OrchestrationEvent]]s as they arrive; nodes are kept in declaration order so
  * the view is stable. The TUI and the web UI share this exact fold, so they can
  * never drift. */
final case class Forest(
    groups: Vector[ForestGroup] = Vector.empty,
    nodes: Vector[ForestNode] = Vector.empty,
    logs: Vector[String] = Vector.empty,
    code: Option[String] = None,
    status: RunStatus = RunStatus.Running
):
  def update(ev: OrchestrationEvent): Forest =
    import OrchestrationEvent.*
    ev match
      case GroupDeclared(_, gid, name, desc, _) =>
        if groups.exists(_.id == gid) then this else copy(groups = groups :+ ForestGroup(gid, name, desc))
      case NodeDeclared(_, id, group, deps) =>
        if nodes.exists(_.id == id) then this
        else copy(nodes = nodes :+ ForestNode(id, group, deps, NodeStatus.Pending))
      case NodeQueued(_, id) =>
        upsert(id)(_.copy(status = NodeStatus.Queued))
      case NodeStarted(_, id, prompt) =>
        upsert(id)(_.copy(status = NodeStatus.Running, prompt = Some(prompt)))
      case NodeProgress(_, id, in, out, tool) =>
        upsert(id)(n => n.copy(inputTokens = in, outputTokens = out, currentTool = tool.orElse(n.currentTool)))
      case NodeFinished(_, id, ok, summary) =>
        upsert(id)(_.copy(status = if ok then NodeStatus.Done else NodeStatus.Failed, currentTool = None, summary = Some(summary)))
      // A sub-agent killed by a pause: distinct from a failure (it never settled),
      // so the UI can show it as interrupted and resume re-runs it.
      case NodeInterrupted(_, id) =>
        upsert(id)(_.copy(status = NodeStatus.Interrupted, currentTool = None))
      case Log(_, msg) =>
        copy(logs = logs :+ msg)
      case WorkflowCode(_, c) =>
        copy(code = Some(c))
      case WorkflowPaused(_) =>
        copy(status = RunStatus.Paused)
      case WorkflowResumed(_) =>
        copy(status = RunStatus.Running)
      // A run's terminal outcome only sets the run status (a UI-panel concern) —
      // it never touches nodes, so a late event can't corrupt the forest.
      case WorkflowFinished(_, ok, _) =>
        copy(status = if ok then RunStatus.Done else RunStatus.Failed)

  /** If `ev` re-admits a node that is currently [[NodeStatus.Interrupted]] — i.e. a
    * resumed run re-running it from scratch — the node's id. A transcript store uses
    * this to drop the discarded attempt's transcript before the fresh one streams in,
    * so the two attempts aren't spliced together. `None` for any other event (and for
    * a first run, where the node isn't interrupted). */
  def restartsInterrupted(ev: OrchestrationEvent): Option[String] =
    import OrchestrationEvent.*
    val nid = ev match
      case NodeQueued(_, id)     => Some(id)
      case NodeStarted(_, id, _) => Some(id)
      case _                     => None
    nid.filter(id => nodes.exists(n => n.id == id && n.status == NodeStatus.Interrupted))

  private def upsert(id: String)(f: ForestNode => ForestNode): Forest =
    if nodes.exists(_.id == id) then copy(nodes = nodes.map(n => if n.id == id then f(n) else n))
    else copy(nodes = nodes :+ f(ForestNode(id, None, Nil, NodeStatus.Pending)))

object Forest:
  val empty: Forest = Forest()
