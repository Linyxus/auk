package auk.workflow

import scala.scalajs.js

/** JSON codec for [[WireMessage]] (host -> browser, one object per SSE frame),
  * built on `scala.scalajs.js.JSON` so it links under both the WasmGC host and
  * the plain-JS web UI. Modeled on `auk.runtime.repl.ReplProtocol`.
  *
  * Wire shape — every message has a `kind`:
  *   - event:    `{"kind":"event","t":"<case>","runId":…, …case fields}`
  *   - activity: `{"kind":"activity","t":"<case>","runId":…,"nodeId":…, …case fields}`
  *   - snapshot: `{"kind":"snapshot","forests":[{"runId":…,"groups":[…],"nodes":[…],"logs":[…]}]}`
  * `NodeStatus` serializes as its lowercase name; `Option[String]` as the string
  * or JSON `null`; token counts as JSON numbers.
  *
  * [[decode]] never throws — malformed input (non-JSON, missing/unknown
  * discriminator, truncated) yields `Left`, so the SSE client can log-and-continue.
  */
object WireCodec:

  // -- encode -----------------------------------------------------------------

  def encode(m: WireMessage): String = js.JSON.stringify(toJs(m))

  private def toJs(m: WireMessage): js.Any = m match
    case WireMessage.Event(ev)    => encodeEvent(ev)
    case WireMessage.Activity(ev) => encodeActivity(ev)
    case WireMessage.Snapshot(forests) =>
      js.Dynamic.literal(
        kind = "snapshot",
        forests = js.Array[js.Any](forests.map((rid, f) => encodeForest(rid, f))*)
      )

  private def encodeActivity(ev: TranscriptEvent): js.Any = ev match
    case TranscriptEvent.Said(runId, nodeId, text) =>
      js.Dynamic.literal(kind = "activity", t = "said", runId = runId, nodeId = nodeId, text = text)
    case TranscriptEvent.Thought(runId, nodeId, text) =>
      js.Dynamic.literal(kind = "activity", t = "thought", runId = runId, nodeId = nodeId, text = text)
    case TranscriptEvent.ToolCalled(runId, nodeId, callId, tool, input) =>
      js.Dynamic.literal(kind = "activity", t = "toolCalled", runId = runId, nodeId = nodeId,
        callId = callId, tool = tool, input = input)
    case TranscriptEvent.ToolReturned(runId, nodeId, callId, output, isError) =>
      js.Dynamic.literal(kind = "activity", t = "toolReturned", runId = runId, nodeId = nodeId,
        callId = callId, output = output, isError = isError)

  private def encodeEvent(ev: OrchestrationEvent): js.Any = ev match
    case OrchestrationEvent.GroupDeclared(runId, groupId, name, description, parent) =>
      js.Dynamic.literal(kind = "event", t = "groupDeclared", runId = runId,
        groupId = groupId, name = name, description = description, parent = jsOpt(parent))
    case OrchestrationEvent.NodeDeclared(runId, nodeId, group, deps) =>
      js.Dynamic.literal(kind = "event", t = "nodeDeclared", runId = runId,
        nodeId = nodeId, group = jsOpt(group), deps = jsArr(deps))
    case OrchestrationEvent.NodeQueued(runId, nodeId) =>
      js.Dynamic.literal(kind = "event", t = "nodeQueued", runId = runId, nodeId = nodeId)
    case OrchestrationEvent.NodeStarted(runId, nodeId, prompt) =>
      js.Dynamic.literal(kind = "event", t = "nodeStarted", runId = runId, nodeId = nodeId, prompt = prompt)
    case OrchestrationEvent.NodeProgress(runId, nodeId, inT, outT, tool) =>
      js.Dynamic.literal(kind = "event", t = "nodeProgress", runId = runId, nodeId = nodeId,
        inputTokens = inT.toDouble, outputTokens = outT.toDouble, currentTool = jsOpt(tool))
    case OrchestrationEvent.NodeFinished(runId, nodeId, ok, summary) =>
      js.Dynamic.literal(kind = "event", t = "nodeFinished", runId = runId, nodeId = nodeId, ok = ok, summary = summary)
    case OrchestrationEvent.Log(runId, message) =>
      js.Dynamic.literal(kind = "event", t = "log", runId = runId, message = message)
    case OrchestrationEvent.WorkflowPaused(runId) =>
      js.Dynamic.literal(kind = "event", t = "workflowPaused", runId = runId)
    case OrchestrationEvent.WorkflowResumed(runId) =>
      js.Dynamic.literal(kind = "event", t = "workflowResumed", runId = runId)
    case OrchestrationEvent.WorkflowCode(runId, code) =>
      js.Dynamic.literal(kind = "event", t = "workflowCode", runId = runId, code = code)
    case OrchestrationEvent.WorkflowFinished(runId, ok, summary) =>
      js.Dynamic.literal(kind = "event", t = "workflowFinished", runId = runId, ok = ok, summary = summary)

  private def encodeForest(runId: String, f: Forest): js.Any =
    js.Dynamic.literal(
      runId = runId,
      groups = js.Array[js.Any](f.groups.map(g =>
        js.Dynamic.literal(id = g.id, name = g.name, description = g.description).asInstanceOf[js.Any])*),
      nodes = js.Array[js.Any](f.nodes.map(encodeNode)*),
      logs = jsArr(f.logs),
      code = jsOpt(f.code),
      status = runStatusName(f.status)
    )

  private def runStatusName(s: RunStatus): String = s match
    case RunStatus.Running => "running"
    case RunStatus.Paused  => "paused"
    case RunStatus.Done    => "done"
    case RunStatus.Failed  => "failed"

  private def decodeRunStatus(s: String): RunStatus = s match
    case "paused" => RunStatus.Paused
    case "done"   => RunStatus.Done
    case "failed" => RunStatus.Failed
    case _        => RunStatus.Running

  private def encodeNode(n: ForestNode): js.Any =
    js.Dynamic.literal(
      id = n.id,
      group = jsOpt(n.group),
      deps = jsArr(n.deps),
      status = statusName(n.status),
      inputTokens = n.inputTokens.toDouble,
      outputTokens = n.outputTokens.toDouble,
      currentTool = jsOpt(n.currentTool),
      summary = jsOpt(n.summary),
      prompt = jsOpt(n.prompt)
    )

  private def statusName(s: NodeStatus): String = s match
    case NodeStatus.Pending => "pending"
    case NodeStatus.Queued  => "queued"
    case NodeStatus.Running => "running"
    case NodeStatus.Done    => "done"
    case NodeStatus.Failed  => "failed"

  // -- decode -----------------------------------------------------------------

  def decode(line: String): Either[String, WireMessage] =
    try
      val d = js.JSON.parse(line)
      str(d.kind) match
        case Some("event")    => decodeEvent(d)
        case Some("activity") => decodeActivity(d)
        case Some("snapshot") => Right(WireMessage.Snapshot(decodeForests(d.forests)))
        case Some(other)      => Left(s"unknown wire kind: $other")
        case None             => Left(s"wire message without a kind: $line")
    catch case _: Throwable => Left(s"unparseable wire message: $line")

  private def decodeEvent(d: js.Dynamic): Either[String, WireMessage] =
    str(d.t) match
      case None => Left("event without a t")
      case Some(t) =>
        val rid = str(d.runId).getOrElse("")
        val ev: Either[String, OrchestrationEvent] = t match
          case "groupDeclared" =>
            Right(OrchestrationEvent.GroupDeclared(rid, str(d.groupId).getOrElse(""),
              str(d.name).getOrElse(""), str(d.description).getOrElse(""), strOpt(d.parent)))
          case "nodeDeclared" =>
            Right(OrchestrationEvent.NodeDeclared(rid, str(d.nodeId).getOrElse(""), strOpt(d.group), strList(d.deps)))
          case "nodeQueued" =>
            Right(OrchestrationEvent.NodeQueued(rid, str(d.nodeId).getOrElse("")))
          case "nodeStarted" =>
            Right(OrchestrationEvent.NodeStarted(rid, str(d.nodeId).getOrElse(""), str(d.prompt).getOrElse("")))
          case "nodeProgress" =>
            Right(OrchestrationEvent.NodeProgress(rid, str(d.nodeId).getOrElse(""),
              num(d.inputTokens).map(_.toLong).getOrElse(0L), num(d.outputTokens).map(_.toLong).getOrElse(0L),
              strOpt(d.currentTool)))
          case "nodeFinished" =>
            Right(OrchestrationEvent.NodeFinished(rid, str(d.nodeId).getOrElse(""), bool(d.ok), str(d.summary).getOrElse("")))
          case "log" =>
            Right(OrchestrationEvent.Log(rid, str(d.message).getOrElse("")))
          case "workflowPaused" =>
            Right(OrchestrationEvent.WorkflowPaused(rid))
          case "workflowResumed" =>
            Right(OrchestrationEvent.WorkflowResumed(rid))
          case "workflowCode" =>
            Right(OrchestrationEvent.WorkflowCode(rid, str(d.code).getOrElse("")))
          case "workflowFinished" =>
            Right(OrchestrationEvent.WorkflowFinished(rid, bool(d.ok), str(d.summary).getOrElse("")))
          case other => Left(s"unknown event t: $other")
        ev.map(WireMessage.Event(_))

  private def decodeActivity(d: js.Dynamic): Either[String, WireMessage] =
    str(d.t) match
      case None => Left("activity without a t")
      case Some(t) =>
        val rid = str(d.runId).getOrElse("")
        val nid = str(d.nodeId).getOrElse("")
        val ev: Either[String, TranscriptEvent] = t match
          case "said" =>
            Right(TranscriptEvent.Said(rid, nid, str(d.text).getOrElse("")))
          case "thought" =>
            Right(TranscriptEvent.Thought(rid, nid, str(d.text).getOrElse("")))
          case "toolCalled" =>
            Right(TranscriptEvent.ToolCalled(rid, nid, str(d.callId).getOrElse(""),
              str(d.tool).getOrElse(""), str(d.input).getOrElse("")))
          case "toolReturned" =>
            Right(TranscriptEvent.ToolReturned(rid, nid, str(d.callId).getOrElse(""),
              str(d.output).getOrElse(""), bool(d.isError)))
          case other => Left(s"unknown activity t: $other")
        ev.map(WireMessage.Activity(_))

  private def decodeForests(v: js.Dynamic): List[(String, Forest)] =
    arr(v).map: fd =>
      val runId = str(fd.runId).getOrElse("")
      val groups = arr(fd.groups).map(decodeGroup).toVector
      val nodes = arr(fd.nodes).map(decodeNode).toVector
      val logs = strList(fd.logs).toVector
      (runId, Forest(groups, nodes, logs, strOpt(fd.code), decodeRunStatus(str(fd.status).getOrElse("running"))))

  private def decodeGroup(d: js.Dynamic): ForestGroup =
    ForestGroup(str(d.id).getOrElse(""), str(d.name).getOrElse(""), str(d.description).getOrElse(""))

  private def decodeNode(d: js.Dynamic): ForestNode =
    ForestNode(
      id = str(d.id).getOrElse(""),
      group = strOpt(d.group),
      deps = strList(d.deps),
      status = decodeStatus(str(d.status).getOrElse("pending")),
      inputTokens = num(d.inputTokens).map(_.toLong).getOrElse(0L),
      outputTokens = num(d.outputTokens).map(_.toLong).getOrElse(0L),
      currentTool = strOpt(d.currentTool),
      summary = strOpt(d.summary),
      prompt = strOpt(d.prompt)
    )

  private def decodeStatus(s: String): NodeStatus = s match
    case "queued"  => NodeStatus.Queued
    case "running" => NodeStatus.Running
    case "done"    => NodeStatus.Done
    case "failed"  => NodeStatus.Failed
    case _         => NodeStatus.Pending

  // -- js.Dynamic field accessors (mirror ReplProtocol) ------------------------

  private def jsOpt(o: Option[String]): js.Any = o match
    case Some(s) => s
    case None    => null.asInstanceOf[js.Any]

  private def jsArr(ss: Seq[String]): js.Array[js.Any] =
    js.Array[js.Any](ss.map(_.asInstanceOf[js.Any])*)

  private def str(v: js.Dynamic): Option[String] =
    if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None
  private def strOpt(v: js.Dynamic): Option[String] = str(v)
  private def bool(v: js.Dynamic): Boolean =
    js.typeOf(v) == "boolean" && v.asInstanceOf[Boolean]
  private def num(v: js.Dynamic): Option[Double] =
    if js.typeOf(v) == "number" then Some(v.asInstanceOf[Double]) else None
  private def arr(v: js.Dynamic): List[js.Dynamic] =
    if js.Array.isArray(v) then v.asInstanceOf[js.Array[js.Dynamic]].toList else Nil
  private def strList(v: js.Dynamic): List[String] =
    if js.Array.isArray(v) then
      v.asInstanceOf[js.Array[js.Any]].toList.flatMap: e =>
        if js.typeOf(e) == "string" then Some(e.asInstanceOf[String]) else None
    else Nil
