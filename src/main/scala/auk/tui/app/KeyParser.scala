package auk.tui.app

import scala.collection.mutable

/** Incremental decoder from a raw byte stream to [[Key]]s. Fed one byte at a
  * time (bytes arrive split across reads), it buffers partial UTF-8 runes and
  * partial escape sequences and emits keys as they complete. */
final class KeyParser:
  // Pending bytes of an in-progress escape sequence or multi-byte UTF-8 rune.
  private val buf = mutable.ArrayBuffer.empty[Int]
  private var shiftHeld = false
  // Raw bytes still to be dropped after a bare X10 mouse intro (`ESC [ M`). We
  // never enable X10, but a mis-configured terminal emits the intro followed by
  // three coordinate bytes that can be < 0x20 and would otherwise leak into the
  // input box as garbage; this counter drops exactly those three.
  private var swallow = 0

  /** Feed one byte (0..255); returns the keys it completes (usually 0 or 1). */
  def feed(b: Int): List[Key] =
    if swallow > 0 then { swallow -= 1; Nil }
    else if buf.isEmpty then start(b) else continue(b)

  private def start(b: Int): List[Key] =
    if b == 0x1b then { buf += b; Nil } // ESC: begin a sequence
    else if b == 0x0d then
      List(if shiftHeld then Key.Newline else Key.Enter)
    else if b == 0x0a then List(Key.Newline)
    else if b == 0x7f || b == 0x08 then List(Key.Backspace)
    else if b == 0x09 then List(Key.Tab)
    else if b >= 0x01 && b <= 0x1a then List(Key.Ctrl(('A' + b - 1).toChar))
    else if b >= 0x20 && b < 0x80 then List(Key.Char(b.toChar))
    else if b >= 0xc0 then { buf += b; Nil } // UTF-8 lead byte
    else Nil // stray continuation byte or NUL: ignore

  private def continue(b: Int): List[Key] =
    if buf(0) == 0x1b then escape(b) else utf8(b)

  private def escape(b: Int): List[Key] =
    if buf.length == 1 then
      // Just ESC so far.
      if b == '[' || b == 'O' then { buf += b; Nil } // CSI / SS3 introducer
      else { buf.clear(); Key.Esc :: start(b) } // lone ESC, then reprocess b
    else
      buf += b
      if b >= 0x40 && b <= 0x7e then
        val seq = buf.toVector
        buf.clear()
        parseSequence(seq)
      else Nil // still collecting parameters

  /** Decode a complete `ESC [ … final` or `ESC O final` sequence. */
  private def parseSequence(seq: Vector[Int]): List[Key] =
    val finalByte = seq.last.toChar
    val params = seq.slice(2, seq.length - 1).map(_.toChar).mkString
    finalByte match
      case 'A' | 'B' | 'C' | 'D' | 'H' | 'F' =>
        parseFunctional(finalByte, params)
      case '~' =>
        parseTilde(params)
      case 'u' =>
        parseCsiU(params)
      case 'M' | 'm' if seq.length > 2 && seq(2) == '<' =>
        // SGR 1006 mouse report `ESC [ < b ; x ; y M|m`; `params` still carries
        // the leading `<`. Final `M` is a press, `m` a release.
        parseSgrMouse(params, press = finalByte == 'M')
      case 'M' if seq.length == 3 && seq(1) == '[' =>
        swallow = 3 // bare `ESC [ M`: X10 intro, drop its 3 coordinate bytes.
        Nil
      case _ => List(Key.Unknown)

  private def parseFunctional(finalByte: Char, params: String): List[Key] =
    if isRelease(params) then Nil
    else
      finalByte match
        case 'A' => List(Key.Up)
        case 'B' => List(Key.Down)
        case 'C' => List(Key.Right)
        case 'D' => List(Key.Left)
        case 'H' => List(Key.Home)
        case 'F' => List(Key.End)
        case _   => List(Key.Unknown)

  private def parseTilde(params: String): List[Key] =
    if isRelease(params) then Nil
    else
      // Match on the numeric head so modifier-carrying forms (`5;2` etc.) decode
      // to the same key; the tail (modifier/event) is only used by isRelease.
      params.split(";", -1).headOption.getOrElse("") match
        case "1" | "7" => List(Key.Home)
        case "4" | "8" => List(Key.End)
        case "3"       => List(Key.Delete)
        case "5"       => List(Key.PageUp)
        case "6"       => List(Key.PageDown)
        case _         => parseModifyOtherKeys(params)

  /** Decode an SGR-1006 mouse report body `<b;x;y` (`press` is the trailing
    * `M` vs `m`). Only wheel notches and button press/release are surfaced;
    * motion/drag (bit 32) and horizontal wheel are dropped. `x`/`y` are 1-based
    * cells. Malformed bodies parse to a single [[Key.Unknown]]. */
  private def parseSgrMouse(body: String, press: Boolean): List[Key] =
    body.stripPrefix("<").split(";", -1).map(numericPrefix) match
      case Array(Some(b), Some(x), Some(y)) =>
        if (b & 32) != 0 then Nil // motion/drag report
        else if (b & 64) != 0 then
          // Wheel: only presses count; horizontal wheel (2/3) is ignored.
          if !press then Nil
          else
            (b & 3) match
              case 0 => List(Key.WheelUp(x, y))
              case 1 => List(Key.WheelDown(x, y))
              case _ => Nil
        else
          (b & 3) match
            case 3   => Nil // release sentinel with no button
            case btn => List(if press then Key.MousePress(btn, x, y) else Key.MouseRelease(btn, x, y))
      case _ => List(Key.Unknown)

  private def isRelease(params: String): Boolean =
    val parts = params.split(";", -1)
    val (_, eventType) = modifierAndEvent(parts.lift(1).getOrElse(""))
    eventType == 3

  /** xterm modifyOtherKeys form, commonly emitted by tmux unless configured for
    * CSI-u: `CSI 27 ; modifier ; codepoint ~`.
    */
  private def parseModifyOtherKeys(params: String): List[Key] =
    val parts = params.split(";", -1)
    if parts.length < 3 || parts(0) != "27" then List(Key.Unknown)
    else
      val modifier = numericPrefix(parts(1)).getOrElse(1)
      val code = numericPrefix(parts(2)).getOrElse(-1)
      parseModifiedCode(code, modifier)

  /** Decode CSI-u / Kitty key events for the subset the app binds.
    *
    * Shift+Enter is commonly reported as `CSI 13;2u`; unmodified Enter may be
    * reported as `CSI 13u` or `CSI 13;1u` when all-key reporting is enabled.
    */
  private def parseCsiU(params: String): List[Key] =
    val parts = params.split(";", -1)
    val code = parts.headOption.flatMap(numericPrefix).getOrElse(-1)
    val (modifier, eventType) = modifierAndEvent(parts.lift(1).getOrElse(""))
    val text = associatedText(parts)

    if isShiftKey(code) then
      shiftHeld = eventType != 3 && hasShift(modifier)
      List(Key.Unknown)
    else if eventType == 3 then Nil
    else if text.nonEmpty then text
    else parseModifiedCode(code, modifier)

  private def parseModifiedCode(code: Int, modifier: Int): List[Key] =
    val shifted = hasShift(modifier) || shiftHeld
    val ctrl = hasCtrl(modifier)

    code match
      case 13 if shifted =>
        List(Key.Newline)
      case 13             => List(Key.Enter)
      case 9              => List(Key.Tab)
      case 8 | 127        => List(Key.Backspace)
      case 27             => List(Key.Esc)
      case c if ctrl && c >= 'a' && c <= 'z' =>
        List(Key.Ctrl(c.toChar.toUpper))
      case c if isFunctionalKey(c) =>
        List(Key.Unknown)
      case c if c >= 0x20 && c <= 0xffff && !ctrl =>
        List(Key.Char(c.toChar))
      case _ =>
        List(Key.Unknown)

  private def modifierBits(modifier: Int): Int =
    math.max(0, modifier - 1)

  private def hasShift(modifier: Int): Boolean =
    (modifierBits(modifier) & 1) != 0

  private def hasCtrl(modifier: Int): Boolean =
    (modifierBits(modifier) & 4) != 0

  private def associatedText(parts: Array[String]): List[Key] =
    if parts.length < 3 then Nil
    else
      parts(2)
        .split(":", -1)
        .toList
        .flatMap(numericPrefix)
        .filter(cp => cp >= 0x20 && cp <= 0xffff && !isFunctionalKey(cp))
        .map(cp => Key.Char(cp.toChar))

  private def isFunctionalKey(code: Int): Boolean =
    code >= 0xe000 && code <= 0xf8ff

  private def isShiftKey(code: Int): Boolean =
    code == 57441 || code == 57447 || code == 57453 || code == 57454

  private def modifierAndEvent(s: String): (Int, Int) =
    val parts = s.split(":", -1)
    val modifier = parts.headOption.flatMap(numericPrefix).getOrElse(1)
    val eventType = parts.lift(1).flatMap(numericPrefix).getOrElse(1)
    (modifier, eventType)

  private def numericPrefix(s: String): Option[Int] =
    val head = s.takeWhile(ch => ch >= '0' && ch <= '9')
    head.toIntOption

  private def utf8(b: Int): List[Key] =
    if b < 0x80 || b > 0xbf then
      // Malformed: abort the rune and reprocess this byte fresh.
      buf.clear()
      start(b)
    else
      buf += b
      val lead = buf(0)
      val expected = if lead >= 0xf0 then 4 else if lead >= 0xe0 then 3 else 2
      if buf.length >= expected then
        val cp = decode(buf.toVector)
        buf.clear()
        if cp >= 0 && cp <= 0xffff then List(Key.Char(cp.toChar)) else Nil
      else Nil

  private def decode(bytes: Vector[Int]): Int =
    val lead = bytes(0)
    bytes.length match
      case 2 => ((lead & 0x1f) << 6) | (bytes(1) & 0x3f)
      case 3 => ((lead & 0x0f) << 12) | ((bytes(1) & 0x3f) << 6) | (bytes(2) & 0x3f)
      case _ => ((lead & 0x07) << 18) | ((bytes(1) & 0x3f) << 12) | ((bytes(2) & 0x3f) << 6) | (bytes(3) & 0x3f)
