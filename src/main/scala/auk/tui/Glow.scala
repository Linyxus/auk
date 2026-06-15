package auk.tui

import auk.tui.render.{Ansi, Attr, Color, Style}

/** Colour effects layered over freshly-revealed streaming text.
  *
  * Two pure string builders, both emitting our own SGR sequences — the layout
  * re-tokenizes these into spans, so character widths and soft-wrapping are
  * unaffected, and stripping the escapes yields the original plain text.
  *
  *   - [[trail]] gives newly-revealed text a gradient glow: the newest code
  *     points are the most salient, fading to the block's normal colour a little
  *     further back. Driven by [[Typewriter]]'s cooling cursor, the glow slides
  *     along just behind the reveal — like ink settling as it dries.
  *   - [[cursor]] is the breathing block that rides the tail of the live text.
  *
  * Colours are RGB triples interpolated per code point. The cool end of each
  * gradient is chosen to sit at the block's normal foreground, so the glow reads
  * as a brightening that decays rather than a colour that pops and vanishes.
  */
object Glow:
  /** Answer text: a soft, faintly minty white at the cursor (echoing Auk's
    * green) easing back to a neutral foreground. */
  val AnswerHot: Rgb = (224, 255, 233)
  val AnswerCool: Rgb = (200, 200, 200)

  /** Reasoning text: muted throughout; the glow lifts the newest words just
    * above the baseline the cooled reasoning settles to. */
  val ThinkHot: Rgb = (205, 205, 205)
  val ThinkCool: Rgb = (135, 135, 135)

  /** The colour cooled reasoning content settles to — equal to [[ThinkCool]] so
    * the glow drains seamlessly into it. */
  val ThinkNormal: Color = rgb(ThinkCool)

  /** The dim frame ("│ thinking ▸") around reasoning, a touch below the text. */
  val ThinkBar: Color = Color.True(100, 100, 100)

  /** Decorate `text` so its trailing region `[coolLen, len)` fades from `cool`
    * (oldest, just past the settled prefix) to `hot` (newest, at the cursor).
    *
    * The settled prefix `[0, coolLen)` is emitted plain, so the caller's base
    * style governs it (and a fully-cooled `text` produces no escapes at all,
    * keeping committed text byte-for-byte plain). Every glowing unit is a whole
    * code point with its own SGR, so a surrogate pair is never split across a
    * colour boundary. */
  def trail(text: String, coolLen: Int, hot: Rgb, cool: Rgb): String =
    val cut = math.max(0, math.min(coolLen, text.length))
    val sb = new StringBuilder
    sb.append(text.take(cut)) // plain prefix — inherits the caller's base style
    val zoneLen = if cut < text.length then text.codePointCount(cut, text.length) else 0
    if zoneLen > 0 then
      var i = cut
      var idx = 0
      while i < text.length do
        val cp = text.codePointAt(i)
        val t = (idx + 1).toDouble / zoneLen // (0, 1], brightest at the newest
        sb.append(Style.fg(mix(cool, hot, t)).setSequence)
        sb.append(new String(Character.toChars(cp)))
        i += Character.charCount(cp)
        idx += 1
      sb.append(Ansi.Reset)
    sb.toString

  /** A working-indicator shimmer: a soft highlight glides left-to-right across the
    * text, brightening and emboldening the glyphs it passes over, then rests
    * briefly before sweeping again. Driven by wall-clock time so its pace is steady
    * whatever the frame cadence. Emits our own SGR (re-tokenised by the layout), so
    * stripping the escapes yields the original text. */
  def sweep(text: String, timeMs: Long): String =
    val n = text.length
    if n == 0 then ""
    else
      // The highlight centre travels across the text and a little past each end,
      // plus a gap of rest — long enough to hide the wrap back to the start.
      val travel = n + 2 * SweepHalf + SweepGap
      val center = (timeMs / SweepMsPerChar) % travel - SweepHalf
      val sb = new StringBuilder
      var i = 0
      while i < n do
        val d = math.abs(i - center)
        // A raised-cosine bump: 1 at the centre, easing to 0 at the band's edge.
        val t = if d <= SweepHalf then 0.5 * (1 + math.cos(math.Pi * d / SweepHalf)) else 0.0
        val attrs = if t > 0.55 then Attr.Bold else 0
        sb.append(Style(fg = mix(SweepRest, SweepGlow, t), attrs = attrs).setSequence)
        sb.append(text.charAt(i))
        i += 1
      sb.append(Ansi.Reset)
      sb.toString

  /** The resting colour of the working indicator, and the colour the shimmer
    * lifts it to (a soft light blue echoing the frame). */
  private val SweepRest: Rgb = (104, 112, 122)
  private val SweepGlow: Rgb = (206, 228, 247)
  private val SweepHalf = 4.0        // half-width of the highlight band, in glyphs
  private val SweepGap = 8.0         // glyphs of rest between sweeps
  private val SweepMsPerChar = 70.0  // travel pace: one glyph every 70 ms

  /** A breathing underline cursor: an underscore glyph whose green gently
    * pulses, so the live region reads as alive without the harsh flicker of a
    * hard blink. The output is a pure function of `floorMod(frame, CursorPeriod)`
    * — exactly `CursorPeriod` distinct strings — so they are precomputed once. */
  def cursor(frame: Int): String = cursorFrames(math.floorMod(frame, CursorPeriod))

  /** A red/green/blue triple in 0..255. */
  type Rgb = (Int, Int, Int)

  private val CursorPeriod = 34 // frames per breath; ~1s at the reveal cadence
  private val CursorDim: Rgb = (58, 132, 74)
  private val CursorBright: Rgb = (150, 240, 170)

  // Declared after CursorPeriod/CursorDim/CursorBright so they are initialized
  // first (object fields init in textual order).
  private val cursorFrames: Array[String] =
    Array.tabulate(CursorPeriod) { f =>
      val phase = f.toDouble / CursorPeriod
      val pulse = 0.5 - 0.5 * math.cos(phase * 2 * math.Pi) // smooth 0 → 1 → 0
      Style.fg(mix(CursorDim, CursorBright, pulse)).setSequence + "_" + Ansi.Reset
    }

  private def rgb(c: Rgb): Color = Color.True(c._1, c._2, c._3)

  private def mix(a: Rgb, b: Rgb, t: Double): Color =
    Color.True(lerp(a._1, b._1, t), lerp(a._2, b._2, t), lerp(a._3, b._3, t))

  private def lerp(a: Int, b: Int, t: Double): Int =
    math.max(0, math.min(255, math.round(a + (b - a) * t).toInt))
