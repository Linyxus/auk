package auk.workflow

/** The live status of one sub-agent in a workflow forest. `Pending` = declared in
  * the graph; `Queued` = admitted, waiting for a concurrency slot; `Running` =
  * executing under the cap. */
enum NodeStatus:
  case Pending, Queued, Running, Done, Failed

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
    code: Option[String] = None
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
      case Log(_, msg) =>
        copy(logs = logs :+ msg)
      case WorkflowCode(_, c) =>
        copy(code = Some(c))

  private def upsert(id: String)(f: ForestNode => ForestNode): Forest =
    if nodes.exists(_.id == id) then copy(nodes = nodes.map(n => if n.id == id then f(n) else n))
    else copy(nodes = nodes :+ f(ForestNode(id, None, Nil, NodeStatus.Pending)))

object Forest:
  val empty: Forest = Forest()
