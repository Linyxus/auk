package auk.tui.app

import scala.collection.mutable.ArrayBuffer
import auk.tui.render.{Ansi, Width}

/** A startup byte-stream negotiation with the terminal.
  *
  * The runtime writes [[request]] right after entering raw mode, then routes
  * every input byte through [[feed]] until it reports completion (or the
  * runtime's timeout fires), after which [[settle]] applies whatever was
  * learned. Bytes that turn out not to belong to the handshake (user input
  * racing the terminal's reply) come back via `Step.passthrough` / [[settle]]
  * in arrival order, so no keystroke is ever lost.
  *
  * The first frame is deferred until the handshake settles: what it learns
  * (e.g. glyph widths) must be fixed before any layout is computed or painted.
  */
trait Handshake:
  def request: String

  /** Consume one input byte. `passthrough` returns any bytes recognized as NOT
    * part of the handshake (in order); `done` reports completion. */
  def feed(b: Int): Handshake.Step

  /** Finish the handshake — called exactly once, whether [[feed]] reported done
    * or the runtime timed out — applying any (complete) measurements and
    * returning still-held bytes for the caller to replay as input. */
  def settle(): List[Int]

object Handshake:
  final case class Step(passthrough: List[Int], done: Boolean)
  val Consumed: Step = Step(Nil, false)

/** Measures the terminal's own opinion of the disputed [[Width]] classes.
  *
  * For each probe glyph, [[request]] prints it at column 0 and asks for a
  * cursor position report (`CSI 6n`); the reported column minus one IS the
  * terminal's width for that glyph — ground truth from the only authority that
  * matters, whatever Unicode tables it was built against. The probe line is
  * erased in the same write (and the whole request rides in a DEC-2026 sync
  * wrap, so supporting terminals never even flash it). [[settle]] adopts the
  * measured widths via [[Width.adopt]], gated on the CJK sanity glyph: 一 must
  * measure 2 on any sane UTF-8 terminal — anything else means the replies are
  * untrustworthy (mojibake from a non-UTF-8 stack, a filtering multiplexer)
  * and the modern-terminal defaults are kept.
  *
  * CPR responses (`ESC [ row ; col R`) are recognized with a small state
  * machine; any byte diverging from that shape — a keystroke racing the reply,
  * a mouse report — passes through to the key parser untouched. The inherent
  * DSR ambiguity (modified F3 also arrives as `CSI 1;…R`) is confined to the
  * few-millisecond probe window.
  */
final class WidthProbe extends Handshake:
  import WidthProbe.Glyphs

  private val widths = ArrayBuffer.empty[Int]
  private val pending = ArrayBuffer.empty[Int]
  private var state = 0 // 0 idle, 1 after ESC, 2 inside CSI params

  def request: String =
    val sb = new StringBuilder(Ansi.SyncBegin)
    for cp <- Glyphs do
      sb.append(Ansi.CarriageReturn).appendAll(Character.toChars(cp)).append(Ansi.CSI).append("6n")
    sb.append(Ansi.CarriageReturn).append(Ansi.EraseToEol).append(Ansi.SyncEnd)
    sb.toString

  def feed(b: Int): Handshake.Step =
    state match
      case 0 =>
        if b == 0x1b then { pending += b; state = 1; Handshake.Consumed }
        else Handshake.Step(List(b), false)
      case 1 =>
        if b == '['.toInt then { pending += b; state = 2; Handshake.Consumed }
        else divergence(b)
      case _ =>
        if (b >= '0'.toInt && b <= '9'.toInt) || b == ';'.toInt then
          pending += b
          Handshake.Consumed
        else if b == 'R'.toInt then
          // `ESC [ row ; col R` — the column is the cursor AFTER the glyph
          // printed at column 0, so its width is col - 1. Parse the digits
          // after the last `;` by hand (no String machinery to trip on).
          val semi = pending.lastIndexOf(';'.toInt)
          var col = -1
          if semi >= 2 && semi < pending.length - 1 then
            var v = 0
            var ok = true
            var i = semi + 1
            while i < pending.length do
              val d = pending(i) - '0'.toInt
              if d < 0 || d > 9 then ok = false else v = v * 10 + d
              i += 1
            if ok then col = v
          pending.clear()
          state = 0
          widths += col - 1
          Handshake.Step(Nil, widths.length >= Glyphs.length)
        else divergence(b)

  /** Not a CPR after all: release everything held plus this byte, in order. */
  private def divergence(b: Int): Handshake.Step =
    val leak = pending.toList :+ b
    pending.clear()
    state = 0
    Handshake.Step(leak, false)

  def settle(): List[Int] =
    if widths.length == Glyphs.length && widths(3) == 2 then
      Width.adopt(emojiBmp = widths(0), emojiAstral = widths(1), ambiguous = widths(2))
    val leftover = pending.toList
    pending.clear()
    leftover

object WidthProbe:
  /** ✅ (BMP emoji class), 😀 (astral emoji class), ① (EAW-Ambiguous class),
    * 一 (universal CJK — the sanity gate). */
  private val Glyphs = List(0x2705, 0x1f600, 0x2460, 0x4e00)
