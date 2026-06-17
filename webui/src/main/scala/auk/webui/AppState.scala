package auk.webui

import auk.workflow.{Forest, Transcript, WireMessage}

/** Connection state of the SSE stream, surfaced as a small badge in the UI. */
enum ConnStatus:
  case Connecting, Open, Closed
  case Error(message: String)

/** The whole UI state: one [[Forest]] per run, the per-`(run, node)` streamed
  * [[Transcript]]s, the run insertion order (for stable tabs), the selected run +
  * node, and the connection status.
  *
  * The reducer [[reduce]] and the small mutators are pure, so they are unit-tested
  * without a DOM. The Laminar layer only holds an `AppState` in a `Var` and calls
  * these.
  */
final case class AppState(
    forests: Map[String, Forest] = Map.empty,
    transcripts: Map[String, Map[String, Transcript]] = Map.empty,
    order: Vector[String] = Vector.empty,
    selectedRun: Option[String] = None,
    selectedNode: Option[String] = None,
    conn: ConnStatus = ConnStatus.Connecting
):
  /** Fold one wire message into the state.
    *
    *   - `Snapshot` replaces the forest set (re-validating the run/node selection).
    *   - `Event` folds into its run's forest, creating it — and auto-selecting the
    *     run — when the run is first seen.
    *   - `Activity` folds into the addressed `(run, node)` transcript, creating it
    *     on first sight. It never changes the selection.
    */
  def reduce(msg: WireMessage): AppState = msg match
    case WireMessage.Snapshot(fs) =>
      val m = fs.toMap
      val ord = fs.map(_._1).toVector
      val run = selectedRun.filter(m.contains).orElse(ord.headOption)
      copy(forests = m, order = ord, selectedRun = run, selectedNode = validNode(run, m, selectedNode))
    case WireMessage.Event(ev) =>
      val rid = ev.runId
      val cur = forests.getOrElse(rid, Forest.empty)
      copy(
        forests = forests.updated(rid, cur.update(ev)),
        order = if order.contains(rid) then order else order :+ rid,
        selectedRun = selectedRun.orElse(Some(rid))
      )
    case WireMessage.Activity(ev) =>
      val rid = ev.runId
      val perRun = transcripts.getOrElse(rid, Map.empty)
      val cur = perRun.getOrElse(ev.nodeId, Transcript.empty)
      copy(transcripts = transcripts.updated(rid, perRun.updated(ev.nodeId, cur.update(ev))))

  def withConn(c: ConnStatus): AppState = copy(conn = c)

  /** Switch the selected run, clearing the node selection (a new run's nodes
    * differ). Ignores an unknown id. */
  def selectRun(rid: String): AppState =
    if forests.contains(rid) then copy(selectedRun = Some(rid), selectedNode = None) else this

  /** Select a node within the current run, ignoring an unknown id. */
  def selectNode(nid: String): AppState =
    if selectedRun.exists(r => forests.get(r).exists(_.nodes.exists(_.id == nid)))
    then copy(selectedNode = Some(nid))
    else this

  /** The transcript for the selected node, or empty. */
  def selectedTranscript: Transcript =
    (for r <- selectedRun; n <- selectedNode; t <- transcripts.get(r).flatMap(_.get(n)) yield t)
      .getOrElse(Transcript.empty)

  private def validNode(run: Option[String], fs: Map[String, Forest], node: Option[String]): Option[String] =
    for
      r <- run
      n <- node
      f <- fs.get(r)
      ok <- Option.when(f.nodes.exists(_.id == n))(n)
    yield ok
