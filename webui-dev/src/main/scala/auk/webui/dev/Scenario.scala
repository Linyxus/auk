package auk.webui.dev

import scala.collection.mutable.LinkedHashMap

import auk.workflow.{Forest, OrchestrationEvent, WireCodec, WireMessage}

/** One SSE frame to emit `delayMs` after the stream opens; `data` is the encoded
  * [[WireMessage]]. */
final case class ScheduledLine(delayMs: Int, data: String)

/** Pure scheduling: turn a scripted scenario into the SSE frames the server will
  * emit. No timers, no sockets, so it is unit-tested. */
object Scenario:
  /** Encode each `(delayMs, event)` as an `Event` frame. With `leadSnapshot`,
    * prepend a `Snapshot` (folded from all the events) at delay 0, exercising the
    * late-join path. */
  def schedule(events: Vector[(Int, OrchestrationEvent)], leadSnapshot: Boolean): Vector[ScheduledLine] =
    val lead =
      if leadSnapshot then Vector(ScheduledLine(0, WireCodec.encode(snapshotFrom(events))))
      else Vector.empty
    lead ++ events.map((t, e) => ScheduledLine(t, WireCodec.encode(WireMessage.Event(e))))

  /** Fold a scenario's events into one [[Forest]] per runId, as a `Snapshot`. */
  def snapshotFrom(events: Vector[(Int, OrchestrationEvent)]): WireMessage =
    val byRun = LinkedHashMap.empty[String, Forest]
    for (_, e) <- events do
      byRun(e.runId) = byRun.getOrElse(e.runId, Forest.empty).update(e)
    WireMessage.Snapshot(byRun.toList)
