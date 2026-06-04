package auk.tui

/** Text that arrives in bursts but is revealed smoothly.
  *
  * Streamed model output lands in network-paced bursts; painting each delta the
  * instant it arrives looks jerky. A `Typewriter` separates what has *arrived*
  * (`full`) from what is *shown* (the first `shown` characters) and reveals a
  * little more on every animation tick, for a steady typewriter effect.
  *
  * Pacing is adaptive: each [[advance]] reveals a fixed fraction of the
  * outstanding backlog, never below a one-character floor. A large burst catches
  * up quickly while a trailing trickle still moves, and because the backlog —
  * not a fixed count — sets the speed, the feel is the same whatever the delta
  * sizes are and the reveal never lags unboundedly behind arrival.
  */
final case class Typewriter(full: String, shown: Int):
  /** Append newly-arrived text. The shown prefix is unchanged — the backlog,
    * and thus the reveal speed, simply grows. */
  def append(text: String): Typewriter =
    if text.isEmpty then this else copy(full = full + text)

  /** The portion to display right now. */
  def visible: String = full.take(shown)

  /** Characters that have arrived but are not yet shown. */
  def pending: Int = full.length - shown

  /** True once the shown prefix has caught up with everything that arrived. */
  def settled: Boolean = shown >= full.length

  /** Reveal the next batch toward `full`; a no-op once settled. The cut is
    * snapped to a code-point boundary so a surrogate pair (a non-BMP character
    * such as an emoji) is never split into a broken half. */
  def advance: Typewriter =
    if settled then this
    else
      val raw = math.min(full.length, shown + Typewriter.stepFor(pending))
      copy(shown = Typewriter.snapToCodePoint(full, raw))

  /** Reveal everything at once — for text that should stop animating (e.g.
    * reasoning the moment it collapses to a duration). */
  def settle: Typewriter = if settled then this else copy(shown = full.length)

object Typewriter:
  val empty: Typewriter = Typewriter("", 0)

  /** A typewriter whose text is already fully shown — for committed or loaded
    * text that must not animate. */
  def shown(text: String): Typewriter = Typewriter(text, text.length)

  /** How many characters to reveal for a given backlog: a fixed fraction, never
    * below the floor, so bursts drain fast and the tail still advances. */
  def stepFor(pending: Int): Int =
    math.max(MinStep, math.ceil(pending * DrainFraction).toInt)

  /** Nudge a cut index forward off the low half of a surrogate pair, so the
    * revealed prefix never ends with a dangling half-character. ASCII and BMP
    * text (including Chinese) are unaffected. */
  def snapToCodePoint(s: String, at: Int): Int =
    if at > 0 && at < s.length && Character.isLowSurrogate(s.charAt(at)) && Character.isHighSurrogate(s.charAt(at - 1))
    then at + 1
    else at

  /** Fraction of the backlog revealed per tick. With the UI's reveal cadence
    * (see `ChatApp.RevealMs`) this gives a ~150 ms catch-up: smooth, never
    * laggy. */
  private val DrainFraction = 0.2

  /** The reveal always moves at least this fast, so the final characters of a
    * burst don't crawl as the backlog shrinks toward zero. */
  private val MinStep = 1
