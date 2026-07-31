package auk.webui.dev

import scala.collection.mutable.LinkedHashMap

import auk.workflow.{Forest, LoopWire, WireCodec, WireMessage}

/** One SSE frame to emit `delayMs` after the stream opens; `data` is the encoded
  * [[WireMessage]]. */
final case class ScheduledLine(delayMs: Int, data: String)

/** Pure scheduling: turn a scripted scenario (a timeline of [[WireMessage]]s) into
  * the SSE frames the server will emit. No timers, no sockets, so it is
  * unit-tested. */
object Scenario:
  /** Encode each `(delayMs, message)`. With `leadSnapshot`, prepend the frames a
    * connect sends — a `Snapshot` folded from all the forest events, then a
    * `LoopSnapshot` of every loop the scenario ends up holding — at delay 0,
    * exercising the late-join path in the host's own order. A scenario with no loops
    * leads with the workflow snapshot alone, since an empty loop snapshot says
    * nothing. */
  def schedule(messages: Vector[(Int, WireMessage)], leadSnapshot: Boolean): Vector[ScheduledLine] =
    val lead =
      if !leadSnapshot then Vector.empty
      else
        ScheduledLine(0, WireCodec.encode(snapshotFrom(messages))) +:
          loopSnapshotFrom(messages).map(m => ScheduledLine(0, WireCodec.encode(m))).toVector
    lead ++ messages.map((t, m) => ScheduledLine(t, WireCodec.encode(m)))

  /** Fold a scenario's forest events into one [[Forest]] per runId, as a
    * `Snapshot`. Transcript activity does not contribute to the forest snapshot. */
  def snapshotFrom(messages: Vector[(Int, WireMessage)]): WireMessage =
    val byRun = LinkedHashMap.empty[String, Forest]
    for (_, m) <- messages do
      m match
        case WireMessage.Event(e) => byRun(e.runId) = byRun.getOrElse(e.runId, Forest.empty).update(e)
        case _                    => ()
    WireMessage.Snapshot(byRun.toList)

  /** The loops a scenario leaves behind, as a `LoopSnapshot` — the last state each
    * one reaches, since a loop is sent whole and a later message simply replaces an
    * earlier one. `None` when the scenario has no loops at all. */
  def loopSnapshotFrom(messages: Vector[(Int, WireMessage)]): Option[WireMessage] =
    val byId = LinkedHashMap.empty[String, LoopWire]
    for (_, m) <- messages do
      m match
        case WireMessage.LoopSnapshot(ls) => ls.foreach(l => byId(l.id) = l)
        case WireMessage.Loop(l)          => byId(l.id) = l
        case _                            => ()
    Option.when(byId.nonEmpty)(WireMessage.LoopSnapshot(byId.values.toList))
