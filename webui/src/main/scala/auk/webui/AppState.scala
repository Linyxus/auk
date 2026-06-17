package auk.webui

import auk.workflow.{Forest, WireMessage}

/** Connection state of the SSE stream, surfaced as a small badge in the UI. */
enum ConnStatus:
  case Connecting, Open, Closed
  case Error(message: String)

/** The whole UI state: one [[Forest]] per run, the run insertion order (for stable
  * tabs), the selected run, and the connection status.
  *
  * The reducer [[apply]] and the small mutators are pure, so they are unit-tested
  * without a DOM. The Laminar layer only holds an `AppState` in a `Var` and calls
  * these.
  */
final case class AppState(
    forests: Map[String, Forest] = Map.empty,
    order: Vector[String] = Vector.empty,
    selected: Option[String] = None,
    conn: ConnStatus = ConnStatus.Connecting
):
  /** Fold one wire message into the state. A `Snapshot` replaces the forest set
    * (keeping the current selection if it survives, else the first run); an
    * `Event` folds into its run's forest, creating it — and auto-selecting it —
    * when the run is first seen. */
  def reduce(msg: WireMessage): AppState = msg match
    case WireMessage.Snapshot(fs) =>
      val m = fs.toMap
      val ord = fs.map(_._1).toVector
      copy(forests = m, order = ord, selected = selected.filter(m.contains).orElse(ord.headOption))
    case WireMessage.Event(ev) =>
      val rid = ev.runId
      val cur = forests.getOrElse(rid, Forest.empty)
      copy(
        forests = forests.updated(rid, cur.update(ev)),
        order = if order.contains(rid) then order else order :+ rid,
        selected = selected.orElse(Some(rid))
      )

  def withConn(c: ConnStatus): AppState = copy(conn = c)

  /** Switch the selected run, ignoring an unknown id. */
  def select(rid: String): AppState = if forests.contains(rid) then copy(selected = Some(rid)) else this
