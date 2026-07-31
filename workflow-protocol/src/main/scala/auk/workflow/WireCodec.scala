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
  *   - loopSnapshot: `{"kind":"loopSnapshot","loops":[<loop>…]}`
  *   - loop:     `{"kind":"loop","loop":<loop>}`
  * `NodeStatus` serializes as its lowercase name; `Option[String]` as the string
  * or JSON `null`; token counts as JSON numbers. A loop's metrics travel as
  * `[{"key":…,"value":…}]` rather than as an object, because a JS engine reorders
  * an object's numeric-looking keys and their order is meaningful.
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
    case WireMessage.LoopSnapshot(loops) =>
      js.Dynamic.literal(kind = "loopSnapshot", loops = js.Array[js.Any](loops.map(encodeLoop)*))
    case WireMessage.Loop(loop) =>
      js.Dynamic.literal(kind = "loop", loop = encodeLoop(loop))

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
    case TranscriptEvent.Received(runId, nodeId, from, text) =>
      js.Dynamic.literal(kind = "activity", t = "received", runId = runId, nodeId = nodeId,
        from = from, text = text)

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
    case OrchestrationEvent.NodeInterrupted(runId, nodeId) =>
      js.Dynamic.literal(kind = "event", t = "nodeInterrupted", runId = runId, nodeId = nodeId)
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

  private def encodeLoop(l: LoopWire): js.Any =
    js.Dynamic.literal(
      id = l.id,
      phase = l.phase,
      goal = l.goal,
      rubric = l.rubric,
      budgets = js.Dynamic.literal(
        maxGenerations = l.budgets.maxGenerations,
        patience = l.budgets.patience,
        maxAttemptsPerGeneration = l.budgets.maxAttemptsPerGeneration
      ),
      defSource = l.defSource,
      defVersion = l.defVersion,
      held = l.held,
      parked = jsOpt(l.parked),
      orphaned = l.orphaned,
      activity = jsOpt(l.activity),
      liveLabel = jsOpt(l.liveLabel),
      generations = js.Array[js.Any](l.generations.map(encodeGeneration)*),
      createdAt = l.createdAt
    )

  private def encodeGeneration(g: LoopGenerationWire): js.Any =
    js.Dynamic.literal(
      gen = g.gen,
      parent = jsOptInt(g.parent),
      state = g.state,
      description = g.description,
      metrics = encodeMetrics(g.metrics),
      commit = jsOpt(g.commit),
      attempts = js.Array[js.Any](g.attempts.map(encodeAttempt)*),
      startedAt = g.startedAt,
      settledAt = jsOpt(g.settledAt)
    )

  private def encodeAttempt(a: LoopAttemptWire): js.Any =
    js.Dynamic.literal(
      attempt = a.attempt,
      description = a.description,
      artifact = a.artifact,
      hasSnapshot = a.hasSnapshot,
      check = a.check match
        case Some(c) =>
          js.Dynamic.literal(pass = c.pass, reasons = jsArr(c.reasons), metrics = encodeMetrics(c.metrics))
        case None => null.asInstanceOf[js.Any]
      ,
      verdict = a.verdict match
        case Some(v) => js.Dynamic.literal(accepted = v.accepted, feedback = v.feedback, goalReached = v.goalReached)
        case None    => null.asInstanceOf[js.Any]
      ,
      at = a.at
    )

  private def encodeMetrics(metrics: List[(String, Double)]): js.Array[js.Any] =
    js.Array[js.Any](metrics.map((k, v) => js.Dynamic.literal(key = k, value = v).asInstanceOf[js.Any])*)

  private def statusName(s: NodeStatus): String = s match
    case NodeStatus.Pending => "pending"
    case NodeStatus.Queued  => "queued"
    case NodeStatus.Running => "running"
    case NodeStatus.Done    => "done"
    case NodeStatus.Failed  => "failed"
    case NodeStatus.Interrupted => "interrupted"

  // -- decode -----------------------------------------------------------------

  def decode(line: String): Either[String, WireMessage] =
    try
      val d = js.JSON.parse(line)
      str(d.kind) match
        case Some("event")    => decodeEvent(d)
        case Some("activity") => decodeActivity(d)
        case Some("snapshot") => Right(WireMessage.Snapshot(decodeForests(d.forests)))
        case Some("loopSnapshot") => Right(WireMessage.LoopSnapshot(arr(d.loops).map(decodeLoop)))
        case Some("loop")     => Right(WireMessage.Loop(decodeLoop(d.loop)))
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
          case "nodeInterrupted" =>
            Right(OrchestrationEvent.NodeInterrupted(rid, str(d.nodeId).getOrElse("")))
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
          case "received" =>
            Right(TranscriptEvent.Received(rid, nid, str(d.from).getOrElse(""), str(d.text).getOrElse("")))
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

  /** Missing fields read as their empty value rather than failing the frame: a loop
    * whose ledger is one field poorer than the browser expects is still a loop worth
    * drawing, and the alternative — dropping the whole snapshot — would blank the
    * dashboard over a detail. */
  private def decodeLoop(d: js.Dynamic): LoopWire =
    LoopWire(
      id = str(d.id).getOrElse(""),
      phase = str(d.phase).getOrElse(""),
      goal = str(d.goal).getOrElse(""),
      rubric = str(d.rubric).getOrElse(""),
      budgets = obj(d.budgets).map(decodeBudgets).getOrElse(LoopBudgetsWire(0, 0, 0)),
      defSource = str(d.defSource).getOrElse(""),
      defVersion = int(d.defVersion),
      held = bool(d.held),
      parked = strOpt(d.parked),
      orphaned = bool(d.orphaned),
      activity = strOpt(d.activity),
      liveLabel = strOpt(d.liveLabel),
      generations = arr(d.generations).map(decodeGeneration),
      createdAt = str(d.createdAt).getOrElse("")
    )

  private def decodeBudgets(d: js.Dynamic): LoopBudgetsWire =
    LoopBudgetsWire(int(d.maxGenerations), int(d.patience), int(d.maxAttemptsPerGeneration))

  private def decodeGeneration(d: js.Dynamic): LoopGenerationWire =
    LoopGenerationWire(
      gen = int(d.gen),
      parent = num(d.parent).map(_.toInt),
      state = str(d.state).getOrElse(""),
      description = str(d.description).getOrElse(""),
      metrics = decodeMetrics(d.metrics),
      commit = strOpt(d.commit),
      attempts = arr(d.attempts).map(decodeAttempt),
      startedAt = str(d.startedAt).getOrElse(""),
      settledAt = strOpt(d.settledAt)
    )

  private def decodeAttempt(d: js.Dynamic): LoopAttemptWire =
    LoopAttemptWire(
      attempt = int(d.attempt),
      description = str(d.description).getOrElse(""),
      artifact = str(d.artifact).getOrElse(""),
      hasSnapshot = bool(d.hasSnapshot),
      check = obj(d.check).map(c => LoopCheckWire(bool(c.pass), strList(c.reasons), decodeMetrics(c.metrics))),
      verdict = obj(d.verdict).map(v =>
        LoopVerdictWire(bool(v.accepted), str(v.feedback).getOrElse(""), bool(v.goalReached))),
      at = str(d.at).getOrElse("")
    )

  private def decodeMetrics(v: js.Dynamic): List[(String, Double)] =
    arr(v).flatMap(m => str(m.key).flatMap(k => num(m.value).map(k -> _)))

  private def decodeStatus(s: String): NodeStatus = s match
    case "queued"  => NodeStatus.Queued
    case "running" => NodeStatus.Running
    case "done"    => NodeStatus.Done
    case "failed"  => NodeStatus.Failed
    case "interrupted" => NodeStatus.Interrupted
    case _         => NodeStatus.Pending

  // -- js.Dynamic field accessors (mirror ReplProtocol) ------------------------

  private def jsOpt(o: Option[String]): js.Any = o match
    case Some(s) => s
    case None    => null.asInstanceOf[js.Any]

  private def jsOptInt(o: Option[Int]): js.Any = o match
    case Some(n) => n
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
  private def int(v: js.Dynamic): Int = num(v).map(_.toInt).getOrElse(0)
  /** A nested object, or `None` when the field is absent — `js.typeOf(null)` is
    * `"object"` too, so the null check is not redundant. */
  private def obj(v: js.Dynamic): Option[js.Dynamic] =
    if js.typeOf(v) == "object" && v != null then Some(v) else None
  private def arr(v: js.Dynamic): List[js.Dynamic] =
    if js.Array.isArray(v) then v.asInstanceOf[js.Array[js.Dynamic]].toList else Nil
  private def strList(v: js.Dynamic): List[String] =
    if js.Array.isArray(v) then
      v.asInstanceOf[js.Array[js.Any]].toList.flatMap: e =>
        if js.typeOf(e) == "string" then Some(e.asInstanceOf[String]) else None
    else Nil
