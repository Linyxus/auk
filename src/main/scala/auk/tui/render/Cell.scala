package auk.tui.render

/** A single terminal cell, packed into one `Long` so a cell grid is a flat
  * `Array[Long]` with zero per-cell allocation and a one-instruction equality
  * check on the diff hot path.
  *
  * Bit layout:
  * {{{
  *   bits  0..20  codepoint   (21 bits — covers all of Unicode, max U+10FFFF)
  *   bits 21..52  styleId     (32 bits — index into a StylePool)
  *   bits 53..61  (unused)
  *   bits 62..63  widthClass  (Narrow / Wide / Spacer / ZeroWidth)
  * }}}
  */
object Cell:
  /** Occupies one column. */
  inline val Narrow = 0
  /** Lead cell of a two-column (wide / fullwidth / emoji) glyph. */
  inline val Wide = 1
  /** Trailing column of a [[Wide]] glyph; emits nothing on its own. */
  inline val Spacer = 2
  /** A combining / zero-width mark attached after its base cell. */
  inline val ZeroWidth = 3

  private inline val CpMask = 0x1FFFFFL
  private inline val StyleMask = 0xFFFFFFFFL

  inline def pack(cp: Int, styleId: Int, widthClass: Int): Long =
    (cp.toLong & CpMask) |
      ((styleId.toLong & StyleMask) << 21) |
      ((widthClass.toLong & 0x3L) << 62)

  inline def codePoint(c: Long): Int = (c & CpMask).toInt
  inline def styleId(c: Long): Int = ((c >>> 21) & StyleMask).toInt
  inline def widthClass(c: Long): Int = ((c >>> 62) & 0x3L).toInt

  inline def isSpacer(c: Long): Boolean = widthClass(c) == Spacer

  /** A blank cell: a space in the default style. Doubles as the "erased" value. */
  val Blank: Long = pack(' '.toInt, 0, Narrow)
