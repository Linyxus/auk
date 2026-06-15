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
  // setSeqs(id) is the self-contained SGR string for that style, built once at
  // intern time. Both `transition` and `setSequence` index it — the string
  // depends only on the destination style — so the diff hot path is an array
  // read with no per-call StringBuilder or transition-pair map.
  private val setSeqs = mutable.ArrayBuffer.empty[String]

  locally {
    ids(Style.Default.raw) = 0
    styles += Style.Default
    setSeqs += Ansi.Reset // == Style.Default.setSequence (CSI 0m)
  }

  /** The id for `s`, assigning a fresh one on first sight. */
  def intern(s: Style): Int =
    val existing = ids.getOrElse(s.raw, -1) // primitive sentinel: no Some alloc on a hit
    if existing >= 0 then existing
    else
      val id = styles.length
      styles += s
      setSeqs += s.setSequence
      ids(s.raw) = id
      id

  def styleOf(id: Int): Style = styles(id)

  /** The SGR string to move from style id `from` to id `to`; empty if equal.
    * Always a self-contained reset+set of `to`, so it is correct independent of
    * any previously emitted attributes. */
  def transition(from: Int, to: Int): String =
    if from == to then "" else setSeqs(to)

  /** The SGR string to set style id `to` from an unknown state (full reset+set). */
  def setSequence(to: Int): String = setSeqs(to)
