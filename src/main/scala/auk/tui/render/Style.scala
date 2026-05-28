package auk.tui.render

/** A terminal foreground/background colour. */
enum Color:
  case Default
  /** Standard (0..7) or bright (8..15) palette index. */
  case Named(idx: Int)
  /** 256-colour cube index (0..255). */
  case Indexed(n: Int)
  case TrueColor(r: Int, g: Int, b: Int)

object Color:
  val Black: Color = Named(0)
  val Red: Color = Named(1)
  val Green: Color = Named(2)
  val Yellow: Color = Named(3)
  val Blue: Color = Named(4)
  val Magenta: Color = Named(5)
  val Cyan: Color = Named(6)
  val White: Color = Named(7)

  /** A 24-bit truecolour. */
  def True(r: Int, g: Int, b: Int): Color = TrueColor(r, g, b)

/** SGR attribute bit flags. */
object Attr:
  inline val Bold = 1
  inline val Dim = 2
  inline val Italic = 4
  inline val Underline = 8
  inline val Reverse = 16

/** A visual style — foreground, background, and attribute flags — packed into a
  * single `Long` so it is itself the natural interning key and carries no heap
  * object during layout.
  *
  * {{{
  *   bits  0..7   attrs
  *   bits  8..9   fg kind   (0 Default, 1 Named, 2 Indexed, 3 TrueColor)
  *   bits 10..33  fg data
  *   bits 34..35  bg kind
  *   bits 36..59  bg data
  * }}}
  */
opaque type Style = Long

object Style:
  private inline val FgKindShift = 8
  private inline val FgDataShift = 10
  private inline val BgKindShift = 34
  private inline val BgDataShift = 36
  private inline val KindMask = 0x3L
  private inline val DataMask = 0xffffffL // 24 bits

  /** No colour, no attributes — maps to a plain SGR reset. */
  val Default: Style = 0L

  def apply(fg: Color = Color.Default, bg: Color = Color.Default, attrs: Int = 0): Style =
    val (fk, fd) = encode(fg)
    val (bk, bd) = encode(bg)
    (attrs & 0xff).toLong |
      (fk.toLong << FgKindShift) |
      ((fd.toLong & DataMask) << FgDataShift) |
      (bk.toLong << BgKindShift) |
      ((bd.toLong & DataMask) << BgDataShift)

  def fg(c: Color): Style = apply(fg = c)

  val Bold: Style = apply(attrs = Attr.Bold)
  val Dim: Style = apply(attrs = Attr.Dim)
  val Italic: Style = apply(attrs = Attr.Italic)
  val Underline: Style = apply(attrs = Attr.Underline)
  val Reverse: Style = apply(attrs = Attr.Reverse)

  private def encode(c: Color): (Int, Int) = c match
    case Color.Default        => (0, 0)
    case Color.Named(i)       => (1, i)
    case Color.Indexed(n)     => (2, n)
    case Color.TrueColor(r, g, b) =>
      (3, ((r & 0xff) << 16) | ((g & 0xff) << 8) | (b & 0xff))

  private def decode(kind: Int, data: Int): Color = kind match
    case 0 => Color.Default
    case 1 => Color.Named(data)
    case 2 => Color.Indexed(data)
    case _ => Color.TrueColor((data >> 16) & 0xff, (data >> 8) & 0xff, data & 0xff)

  private def appendColor(sb: StringBuilder, c: Color, isFg: Boolean): Unit =
    c match
      case Color.Default => ()
      case Color.Named(i) =>
        val base =
          if i < 8 then (if isFg then 30 else 40) + i
          else (if isFg then 90 else 100) + (i - 8)
        sb.append(';').append(base)
      case Color.Indexed(n) =>
        sb.append(if isFg then ";38;5;" else ";48;5;").append(n)
      case Color.TrueColor(r, g, b) =>
        sb.append(if isFg then ";38;2;" else ";48;2;")
          .append(r).append(';').append(g).append(';').append(b)

  extension (s: Style)
    /** The underlying packed value, used as an interning key. */
    def raw: Long = s
    def attrs: Int = (s & 0xff).toInt
    def fgColor: Color = decode(((s >>> FgKindShift) & KindMask).toInt, ((s >>> FgDataShift) & DataMask).toInt)
    def bgColor: Color = decode(((s >>> BgKindShift) & KindMask).toInt, ((s >>> BgDataShift) & DataMask).toInt)
    def hasAttr(a: Int): Boolean = (s.attrs & a) != 0

    /** Combine two styles: attribute flags are OR-ed; a non-default colour on
      * `o` overrides `s`. Used to apply an outer `Colored` over inner spans. */
    infix def ++(o: Style): Style =
      val fg = if o.fgColor == Color.Default then s.fgColor else o.fgColor
      val bg = if o.bgColor == Color.Default then s.bgColor else o.bgColor
      apply(fg, bg, s.attrs | o.attrs)

    def withFg(c: Color): Style = apply(c, s.bgColor, s.attrs)
    def withBg(c: Color): Style = apply(s.fgColor, c, s.attrs)
    def withAttr(a: Int): Style = apply(s.fgColor, s.bgColor, s.attrs | a)

    /** The SGR sequence that establishes this style from a reset state, e.g.
      * `CSI 0;1;36m`. Always self-contained (leading reset), so it is correct
      * regardless of the previous style. */
    def setSequence: String =
      val sb = new StringBuilder(Ansi.CSI)
      sb.append('0')
      val a = s.attrs
      if (a & Attr.Bold) != 0 then sb.append(";1")
      if (a & Attr.Dim) != 0 then sb.append(";2")
      if (a & Attr.Italic) != 0 then sb.append(";3")
      if (a & Attr.Underline) != 0 then sb.append(";4")
      if (a & Attr.Reverse) != 0 then sb.append(";7")
      appendColor(sb, s.fgColor, isFg = true)
      appendColor(sb, s.bgColor, isFg = false)
      sb.append('m').toString
