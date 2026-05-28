package auk.tui.app

import gears.async.ReadableChannel

/** A declarative subscription to an event source, re-derived from state after
  * every update. The runtime multiplexes all active subscriptions (plus its own
  * input/timer/frame sources) through one `Async.select`. */
enum Sub[+Msg]:
  case None
  case Batch[Msg](subs: List[Sub[Msg]]) extends Sub[Msg]
  case OnKeyPress[Msg](handler: Key => Option[Msg]) extends Sub[Msg]
  case TimeEveryMs[Msg](ms: Long, msg: Msg) extends Sub[Msg]
  /** Subscribe to a gears channel as a first-class event source. `toMsg` maps a
    * delivered value to a message; `onClosed` fires once when the channel
    * closes. This is how the engine's event stream is consumed. */
  case OnChannel[A, Msg](channel: ReadableChannel[A], toMsg: A => Msg, onClosed: Msg) extends Sub[Msg]

object Sub:
  def none[Msg]: Sub[Msg] = Sub.None
  def batch[Msg](subs: Sub[Msg]*): Sub[Msg] = Sub.Batch(subs.toList)
  def onKeyPress[Msg](handler: Key => Option[Msg]): Sub[Msg] = Sub.OnKeyPress(handler)
  def onChannel[A, Msg](channel: ReadableChannel[A])(toMsg: A => Msg, onClosed: Msg): Sub[Msg] =
    Sub.OnChannel(channel, toMsg, onClosed)

  object time:
    def everyMs[Msg](ms: Long, msg: Msg): Sub[Msg] = Sub.TimeEveryMs(ms, msg)
