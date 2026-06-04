package auk.tui

/** Text that arrives in bursts but is revealed smoothly, with a glow that
  * trails just behind the reveal.
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
  *
  * A second cursor, `cooled`, chases `shown` from behind: characters in
  * `[cooled, shown)` are freshly revealed and rendered more saliently, fading
  * back to the normal colour as `cooled` catches up (see [[auk.tui.Glow]]). The
  * glow window is bounded to [[GlowTail]] characters during reveal, and once the
  * text is fully shown `cooled` keeps advancing so the glow finishes draining —
  * leaving the text in its settled, normal-coloured form.
  */
final case class Typewriter(full: String, shown: Int, cooled: Int = 0):
  /** Append newly-arrived text. The shown prefix is unchanged — the backlog,
    * and thus the reveal speed, simply grows. */
  def append(text: String): Typewriter =
    if text.isEmpty then this else copy(full = full + text)

  /** The portion to display right now. */
  def visible: String = full.take(shown)

  /** How many of the [[visible]] characters have fully cooled to the normal
    * colour; the glow spans `[coolPrefixLen, visible.length)`. */
  def coolPrefixLen: Int = math.min(cooled, shown)

  /** Characters that have arrived but are not yet shown. */
  def pending: Int = full.length - shown

  /** True once the shown prefix has caught up with everything that arrived. */
  def revealed: Boolean = shown >= full.length

  /** True once everything has been revealed *and* the glow has finished
    * draining — i.e. the text is fully shown in its normal colour. */
  def settled: Boolean = cooled >= full.length

  /** Reveal the next batch toward `full` and advance the cooling cursor behind
    * it; a no-op once settled. Cuts are snapped to a code-point boundary so a
    * surrogate pair (a non-BMP character such as an emoji) is never split. */
  def advance: Typewriter =
    if settled then this
    else
      val newShown =
        if revealed then shown
        else Typewriter.snapToCodePoint(full, math.min(full.length, shown + Typewriter.stepFor(pending)))
      // Cool toward `shown`: at least one step per tick (so the glow drains even
      // when nothing new arrives), and never trailing further than the window.
      val target = math.max(cooled + Typewriter.CoolStep, newShown - Typewriter.GlowTail)
      val newCooled = math.max(cooled, Typewriter.snapToCodePoint(full, math.min(newShown, target)))
      copy(shown = newShown, cooled = newCooled)

  /** Reveal everything at once with no glow — for text that should stop
    * animating (e.g. reasoning the moment it collapses to a duration). */
  def settle: Typewriter = if settled then this else copy(shown = full.length, cooled = full.length)

object Typewriter:
  val empty: Typewriter = Typewriter("", 0, 0)

  /** A typewriter whose text is already fully shown and cooled — for committed
    * or loaded text that must neither animate nor glow. */
  def shown(text: String): Typewriter = Typewriter(text, text.length, text.length)

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

  /** How many freshly-revealed characters glow at once: the cooling cursor is
    * held this far behind the reveal, so the glow is a fixed-length comet tail
    * regardless of how fast text streams in. */
  private val GlowTail = 16

  /** How far the cooling cursor advances per tick once the reveal has caught up.
    * With [[GlowTail]] and the UI's reveal cadence this drains the glow in
    * roughly half a second after the last character lands. */
  private val CoolStep = 1
