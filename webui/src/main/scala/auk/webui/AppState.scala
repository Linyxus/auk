package auk.webui

import auk.workflow.{Forest, Transcript, WireMessage}

/** Connection state of the SSE stream, surfaced as a small badge in the UI. */
enum ConnStatus:
  case Connecting, Open, Closed
  case Error(message: String)

/** What the main panel is focused on within the selected run. */
enum Focus:
  case Unfocused
  case Code
  case Node(id: String)

/** The whole UI state: one [[Forest]] per run, the per-`(run, node)` streamed
  * [[Transcript]]s, the run insertion order (for stable tabs), the selected run,
  * the focused panel (nothing / the workflow code / a node's transcript), and the
  * connection status.
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
    focus: Focus = Focus.Unfocused,
    conn: ConnStatus = ConnStatus.Connecting
):
  /** Fold one wire message into the state.
    *
    *   - `Snapshot` replaces the forest set (re-validating the run/focus).
    *   - `Event` folds into its run's forest, creating it — and auto-selecting the
    *     run — when the run is first seen.
    *   - `Activity` folds into the addressed `(run, node)` transcript, creating it
    *     on first sight. Neither `Event` nor `Activity` changes the focus.
    */
  def reduce(msg: WireMessage): AppState = msg match
    case WireMessage.Snapshot(fs) =>
      val m = fs.toMap
      val ord = fs.map(_._1).toVector
      val run = selectedRun.filter(m.contains).orElse(ord.headOption)
      copy(forests = m, order = ord, selectedRun = run, focus = revalidate(run, m, focus))
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

  /** Switch the selected run, clearing the focus (a new run's nodes differ).
    * Ignores an unknown id. */
  def selectRun(rid: String): AppState =
    if forests.contains(rid) then copy(selectedRun = Some(rid), focus = Focus.Unfocused) else this

  /** Focus a node within the current run, ignoring an unknown id. */
  def selectNode(nid: String): AppState =
    if selectedRun.exists(r => forests.get(r).exists(_.nodes.exists(_.id == nid)))
    then copy(focus = Focus.Node(nid))
    else this

  /** Focus the workflow code (if the selected run has any). */
  def selectCode: AppState =
    if selectedRun.exists(r => forests.get(r).exists(_.code.isDefined)) then copy(focus = Focus.Code) else this

  /** The currently-focused node id, if a node is focused. */
  def focusedNode: Option[String] = focus match
    case Focus.Node(id) => Some(id)
    case _              => None

  /** The transcript for the focused node, or empty. */
  def selectedTranscript: Transcript =
    (for r <- selectedRun; n <- focusedNode; t <- transcripts.get(r).flatMap(_.get(n)) yield t)
      .getOrElse(Transcript.empty)

  private def revalidate(run: Option[String], fs: Map[String, Forest], f: Focus): Focus = f match
    case Focus.Node(id) =>
      if run.exists(r => fs.get(r).exists(_.nodes.exists(_.id == id))) then f else Focus.Unfocused
    case Focus.Code =>
      if run.exists(r => fs.get(r).exists(_.code.isDefined)) then f else Focus.Unfocused
    case Focus.Unfocused => f
