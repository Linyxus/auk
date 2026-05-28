package auk.tui.render

/** Display width of Unicode text in terminal columns.
  *
  * The single source of truth shared by the layout engine and the renderer —
  * if these two disagreed on a glyph's width, cells would misalign. Each
  * codepoint is 0 (combining / zero-width), 1 (normal), or 2 (East-Asian wide
  * / fullwidth / emoji) columns.
  *
  * The tables are "correct enough": base characters, combining marks, the
  * East-Asian Wide/Fullwidth ranges, and the common emoji blocks. Grapheme
  * clusters that render as one glyph but sum to >2 (ZWJ emoji families, flag
  * pairs) are over-counted — a documented limitation, isolated here.
  */
object Width:
  def stringWidth(s: String): Int =
    var i = 0
    var w = 0
    val n = s.length
    while i < n do
      val cp = s.codePointAt(i)
      w += displayWidth(cp)
      i += Character.charCount(cp)
    w

  def displayWidth(cp: Int): Int =
    // C0 / C1 control characters never occupy a column in our grid.
    if cp < 0x20 || (cp >= 0x7f && cp < 0xa0) then 0
    else if inTable(cp, ZeroWidthRanges) then 0
    else if inTable(cp, WideRanges) then 2
    else 1

  /** Membership test over a flat, sorted `[lo0, hi0, lo1, hi1, ...]` table. */
  private def inTable(cp: Int, table: Array[Int]): Boolean =
    var lo = 0
    var hi = table.length / 2 - 1
    while lo <= hi do
      val mid = (lo + hi) >>> 1
      val s = table(mid * 2)
      val e = table(mid * 2 + 1)
      if cp < s then hi = mid - 1
      else if cp > e then lo = mid + 1
      else return true
    false

  // Combining marks, zero-width spaces/joiners, variation selectors.
  private val ZeroWidthRanges: Array[Int] = Array(
    0x0300, 0x036f, // combining diacritical marks
    0x0483, 0x0489,
    0x0591, 0x05bd,
    0x05bf, 0x05bf,
    0x05c1, 0x05c2,
    0x05c4, 0x05c5,
    0x0610, 0x061a,
    0x064b, 0x065f,
    0x0670, 0x0670,
    0x06d6, 0x06dc,
    0x06df, 0x06e4,
    0x06e7, 0x06e8,
    0x06ea, 0x06ed,
    0x0711, 0x0711,
    0x0730, 0x074a,
    0x07a6, 0x07b0,
    0x0900, 0x0902,
    0x093c, 0x093c,
    0x0941, 0x0948,
    0x094d, 0x094d,
    0x0e31, 0x0e31,
    0x0e34, 0x0e3a,
    0x0e47, 0x0e4e,
    0x200b, 0x200f, // ZWSP, ZWNJ, ZWJ, LRM, RLM
    0x2028, 0x202e,
    0x2060, 0x2064,
    0xfe00, 0xfe0f, // variation selectors
    0xfeff, 0xfeff, // BOM / zero-width no-break space
    0xe0100, 0xe01ef // variation selectors supplement
  )

  // East-Asian Wide + Fullwidth + common emoji presentation ranges.
  private val WideRanges: Array[Int] = Array(
    0x1100, 0x115f, // Hangul Jamo
    0x2329, 0x232a,
    0x2e80, 0x303e, // CJK radicals .. punctuation
    0x3041, 0x33ff, // Hiragana .. CJK compatibility
    0x3400, 0x4dbf, // CJK Ext A
    0x4e00, 0x9fff, // CJK Unified Ideographs
    0xa000, 0xa4cf, // Yi
    0xa960, 0xa97f, // Hangul Jamo Ext A
    0xac00, 0xd7a3, // Hangul Syllables
    0xf900, 0xfaff, // CJK Compatibility Ideographs
    0xfe10, 0xfe19, // vertical forms
    0xfe30, 0xfe6f, // CJK compatibility forms / small forms
    0xff00, 0xff60, // Fullwidth forms
    0xffe0, 0xffe6, // Fullwidth signs
    0x16fe0, 0x16fe4,
    0x17000, 0x18aff, // Tangut / Khitan
    0x1b000, 0x1b16f, // Kana supplement / extended
    0x1f004, 0x1f004, // mahjong red dragon
    0x1f0cf, 0x1f0cf, // playing card black joker
    0x1f300, 0x1f64f, // misc symbols & pictographs, emoticons
    0x1f900, 0x1faff, // supplemental symbols & pictographs
    0x20000, 0x3fffd // CJK Ext B+ / supplementary ideographic plane
  )
