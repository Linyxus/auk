package auk.tui.app

/** What `view` produces each frame: the full committed transcript (append-only,
  * printed once into native scrollback) plus the still-changing live region.
  *
  * @param committed the whole committed prefix in order, growing monotonically
  *   across frames. The runtime tracks how many elements it has already flushed
  *   and emits only the new tail, so `view` stays a pure function of state.
  * @param live the region that is still mutating (input box, footer, the live
  *   turn). Re-laid and cell-diffed every frame.
  */
final case class Screen(committed: Vector[Element], live: Element)

/** The Elm-architecture contract. Pure: no gears, no terminal I/O — the
  * [[Runtime]] drives it. */
trait App[State, Msg]:
  def init: (State, Cmd[Msg])
  def update(msg: Msg, state: State): (State, Cmd[Msg])
  def subscriptions(state: State): Sub[Msg]
  def view(state: State): Screen
