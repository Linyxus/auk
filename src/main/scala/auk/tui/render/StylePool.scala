package auk.tui.render

import scala.collection.mutable

/** Interns [[Style]]s to small integer ids (so cells store a 32-bit id, not a
  * colour) and caches the SGR sequence for each style transition.
  *
  * Single-threaded — owned by the renderer's frame loop. After warmup every
  * distinct style and transition is cached, so emitting a style change on the
  * diff hot path is a map lookup returning an already-built string: zero
  * allocation.
  */
final class StylePool:
  // id 0 is always Style.Default (a plain reset).
  private val ids = mutable.LongMap.empty[Int]
  private val styles = mutable.ArrayBuffer.empty[Style]
  private val transitions = mutable.LongMap.empty[String]

  locally {
    ids(Style.Default.raw) = 0
    styles += Style.Default
  }

  /** The id for `s`, assigning a fresh one on first sight. */
  def intern(s: Style): Int =
    val raw = s.raw
    ids.get(raw) match
      case Some(id) => id
      case None =>
        val id = styles.length
        styles += s
        ids(raw) = id
        id

  def styleOf(id: Int): Style = styles(id)

  /** The SGR string to move from style id `from` to id `to`; empty if equal.
    * Always a self-contained reset+set of `to`, so it is correct independent of
    * any previously emitted attributes. */
  def transition(from: Int, to: Int): String =
    if from == to then ""
    else
      val key = (from.toLong << 32) | (to.toLong & 0xffffffffL)
      transitions.getOrElseUpdate(key, styleOf(to).setSequence)

  /** The SGR string to set style id `to` from an unknown state (full reset+set). */
  def setSequence(to: Int): String =
    if to == 0 then Ansi.Reset else styleOf(to).setSequence
