package auk.webui.dev

import scala.collection.mutable.LinkedHashMap

import auk.workflow.{Forest, WireCodec, WireMessage}

/** One SSE frame to emit `delayMs` after the stream opens; `data` is the encoded
  * [[WireMessage]]. */
final case class ScheduledLine(delayMs: Int, data: String)

/** Pure scheduling: turn a scripted scenario (a timeline of [[WireMessage]]s) into
  * the SSE frames the server will emit. No timers, no sockets, so it is
  * unit-tested. */
object Scenario:
  /** Encode each `(delayMs, message)`. With `leadSnapshot`, prepend a `Snapshot`
    * (folded from all the forest events) at delay 0, exercising the late-join path. */
  def schedule(messages: Vector[(Int, WireMessage)], leadSnapshot: Boolean): Vector[ScheduledLine] =
    val lead =
      if leadSnapshot then Vector(ScheduledLine(0, WireCodec.encode(snapshotFrom(messages))))
      else Vector.empty
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
