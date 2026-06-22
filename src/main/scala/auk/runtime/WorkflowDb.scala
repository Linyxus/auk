package auk.runtime

import java.io.IOException

import auk.llm.tools.Json
import auk.llm.tools.Json.get
import auk.platform.{FileSystem, PathOps, Platform}
import auk.session.SessionRef
import auk.workflow.{Forest, ForestGroup, ForestNode, NodeStatus, RunStatus}

/** One workflow's persisted state: enough to display it (the [[Forest]]) and to
  * resume it (the source [[code]] and the per-node [[results]] cache). */
final case class WorkflowRecord(
    id: String,
    status: RunStatus,
    code: String,
    results: Map[String, Json],
    forest: Forest
)

/** Race-safe JSON persistence for workflows, one file per run at
  * `<sessionsDir>/<sessionId>/workflows/<wid>.json` (see [[SessionRef.workflowFile]]).
  *
  * The cache lives at the sub-agent-output level: when a node finishes, its result
  * value is saved under its id, so a resumed run (which re-executes the same code
  * under the same run id) can return finished nodes instantly and only re-run the
  * rest.
  *
  * Each operation is `lock.synchronized` over a *synchronous* read/parse/write, so
  * concurrent finishers can't interleave a read-modify-write. The bridge owns the
  * in-memory source of truth and always writes a whole record, so there is no
  * partial-update hazard either.
  */
final class WorkflowDb(sessionsDir: String, fs: FileSystem = Platform.fs):
  private val lock = new AnyRef

  /** Write `record` for `sessionId`, creating the workflows directory on demand. */
  def save(sessionId: String, record: WorkflowRecord): Either[String, Unit] =
    lock.synchronized:
      val path = SessionRef.workflowFile(sessionsDir, sessionId, record.id)
      try
        PathOps.parent(path).foreach(fs.createDirectories)
        fs.writeString(path, WorkflowDb.encode(record).render)
        Right(())
      catch case e: IOException => Left(s"failed to save workflow '${record.id}': ${e.getMessage}")

  /** Load the record for `wid`, or `None` if no file exists / it can't be parsed. */
  def load(sessionId: String, wid: String): Option[WorkflowRecord] =
    lock.synchronized:
      val path = SessionRef.workflowFile(sessionsDir, sessionId, wid)
      if !fs.exists(path) then None
      else
        try Json.parse(fs.readString(path)).toOption.flatMap(WorkflowDb.decode)
        catch case _: IOException => None

  /** Every paused workflow for `sessionId`, most-relevant order unspecified — the
    * basis for restoring resumable runs on startup. */
  def listPaused(sessionId: String): List[WorkflowRecord] =
    lock.synchronized:
      val dir = SessionRef.workflowsDir(sessionsDir, sessionId)
      fs.listDir(dir)
        .filter(e => e.isFile && e.name.endsWith(".json"))
        .flatMap(e => loadFile(PathOps.join(dir, e.name)))
        .filter(_.status == RunStatus.Paused)

  private def loadFile(path: String): Option[WorkflowRecord] =
    try Json.parse(fs.readString(path)).toOption.flatMap(WorkflowDb.decode)
    catch case _: IOException => None

object WorkflowDb:
  // -- record codec -----------------------------------------------------------

  def encode(r: WorkflowRecord): Json =
    Json.Obj(List(
      "id"      -> Json.Str(r.id),
      "status"  -> Json.Str(runStatusName(r.status)),
      "code"    -> Json.Str(r.code),
      "results" -> Json.Obj(r.results.toList.map((id, v) => id -> v)),
      "forest"  -> encodeForest(r.forest)
    ))

  def decode(json: Json): Option[WorkflowRecord] = json match
    case o: Json.Obj =>
      Some(WorkflowRecord(
        id = strOr(o, "id", ""),
        status = parseRunStatus(strOr(o, "status", "running")),
        code = strOr(o, "code", ""),
        results = o.get("results") match
          case Some(r: Json.Obj) => r.fields.toMap
          case _                 => Map.empty,
        forest = o.get("forest") match
          case Some(f: Json.Obj) => decodeForest(f)
          case _                 => Forest.empty
      ))
    case _ => None

  // -- forest codec (Json AST; mirrors WireCodec's js form) -------------------

  private def encodeForest(f: Forest): Json =
    Json.Obj(List(
      "groups" -> Json.Arr(f.groups.toList.map(g =>
        Json.Obj(List("id" -> Json.Str(g.id), "name" -> Json.Str(g.name), "description" -> Json.Str(g.description))))),
      "nodes"  -> Json.Arr(f.nodes.toList.map(encodeNode)),
      "logs"   -> Json.Arr(f.logs.toList.map(Json.Str(_))),
      "code"   -> f.code.map(Json.Str(_)).getOrElse(Json.Null),
      "status" -> Json.Str(runStatusName(f.status))
    ))

  private def decodeForest(o: Json.Obj): Forest =
    Forest(
      groups = arr(o, "groups").collect { case g: Json.Obj =>
        ForestGroup(strOr(g, "id", ""), strOr(g, "name", ""), strOr(g, "description", "")) }.toVector,
      nodes = arr(o, "nodes").collect { case n: Json.Obj => decodeNode(n) }.toVector,
      logs = arr(o, "logs").collect { case Json.Str(s) => s }.toVector,
      code = strOpt(o, "code"),
      status = parseRunStatus(strOr(o, "status", "running"))
    )

  private def encodeNode(n: ForestNode): Json =
    Json.Obj(List(
      "id"           -> Json.Str(n.id),
      "group"        -> n.group.map(Json.Str(_)).getOrElse(Json.Null),
      "deps"         -> Json.Arr(n.deps.map(Json.Str(_))),
      "status"       -> Json.Str(nodeStatusName(n.status)),
      "inputTokens"  -> Json.num(n.inputTokens),
      "outputTokens" -> Json.num(n.outputTokens),
      "currentTool"  -> n.currentTool.map(Json.Str(_)).getOrElse(Json.Null),
      "summary"      -> n.summary.map(Json.Str(_)).getOrElse(Json.Null),
      "prompt"       -> n.prompt.map(Json.Str(_)).getOrElse(Json.Null)
    ))

  private def decodeNode(o: Json.Obj): ForestNode =
    ForestNode(
      id = strOr(o, "id", ""),
      group = strOpt(o, "group"),
      deps = arr(o, "deps").collect { case Json.Str(s) => s },
      status = parseNodeStatus(strOr(o, "status", "pending")),
      inputTokens = longOr(o, "inputTokens", 0L),
      outputTokens = longOr(o, "outputTokens", 0L),
      currentTool = strOpt(o, "currentTool"),
      summary = strOpt(o, "summary"),
      prompt = strOpt(o, "prompt")
    )

  private def nodeStatusName(s: NodeStatus): String = s match
    case NodeStatus.Pending => "pending"
    case NodeStatus.Queued  => "queued"
    case NodeStatus.Running => "running"
    case NodeStatus.Done    => "done"
    case NodeStatus.Failed  => "failed"

  private def parseNodeStatus(s: String): NodeStatus = s match
    case "queued"  => NodeStatus.Queued
    case "running" => NodeStatus.Running
    case "done"    => NodeStatus.Done
    case "failed"  => NodeStatus.Failed
    case _         => NodeStatus.Pending

  private def runStatusName(s: RunStatus): String = s match
    case RunStatus.Running => "running"
    case RunStatus.Paused  => "paused"
    case RunStatus.Done    => "done"
    case RunStatus.Failed  => "failed"

  private def parseRunStatus(s: String): RunStatus = s match
    case "paused" => RunStatus.Paused
    case "done"   => RunStatus.Done
    case "failed" => RunStatus.Failed
    case _        => RunStatus.Running

  // -- Json.Obj accessors -----------------------------------------------------

  private def strOpt(o: Json.Obj, key: String): Option[String] =
    o.get(key) match
      case Some(Json.Str(s)) => Some(s)
      case _                 => None
  private def strOr(o: Json.Obj, key: String, default: String): String = strOpt(o, key).getOrElse(default)
  private def longOr(o: Json.Obj, key: String, default: Long): Long =
    o.get(key) match
      case Some(Json.Num(n)) => n.toLong
      case _                 => default
  private def arr(o: Json.Obj, key: String): List[Json] =
    o.get(key) match
      case Some(Json.Arr(es)) => es
      case _                  => Nil
