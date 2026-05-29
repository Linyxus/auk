package auk.tui.app

/** What `view` produces each frame: the full committed transcript, normally
  * append-only and printed once into native scrollback, plus the still-changing
  * live region.
  *
  * @param committed the whole committed prefix in order. The runtime tracks how
  *   many elements it has already flushed and emits only the new tail while the
  *   [[committedEpoch]] is unchanged, so `view` stays a pure function of state.
  * @param live the region that is still mutating (input box, footer, the live
  *   turn). Re-laid and cell-diffed every frame.
  * @param committedEpoch bump when `committed` is replaced rather than appended
  *   to, so the runtime clears and reprints the whole transcript.
  * @param overlay an optional floating layer composited over the live region
  *   without changing its layout or committing anything to scrollback.
  */
final case class Screen(
    committed: Vector[Element],
    live: Element,
    committedEpoch: Long = 0,
    overlay: Option[Element] = None
)

/** The Elm-architecture contract. Pure: no gears, no terminal I/O — the
  * [[Runtime]] drives it. */
trait App[State, Msg]:
  def init: (State, Cmd[Msg])
  def update(msg: Msg, state: State): (State, Cmd[Msg])
  def subscriptions(state: State): Sub[Msg]
  def view(state: State): Screen
