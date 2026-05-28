package auk.tui.app

import scala.collection.mutable

/** Incremental decoder from a raw byte stream to [[Key]]s. Fed one byte at a
  * time (bytes arrive split across reads), it buffers partial UTF-8 runes and
  * partial escape sequences and emits keys as they complete. */
final class KeyParser:
  // Pending bytes of an in-progress escape sequence or multi-byte UTF-8 rune.
  private val buf = mutable.ArrayBuffer.empty[Int]

  /** Feed one byte (0..255); returns the keys it completes (usually 0 or 1). */
  def feed(b: Int): List[Key] =
    if buf.isEmpty then start(b) else continue(b)

  private def start(b: Int): List[Key] =
    if b == 0x1b then { buf += b; Nil } // ESC: begin a sequence
    else if b == 0x0d || b == 0x0a then List(Key.Enter)
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
        List(parseSequence(seq))
      else Nil // still collecting parameters

  /** Decode a complete `ESC [ … final` or `ESC O final` sequence. */
  private def parseSequence(seq: Vector[Int]): Key =
    val finalByte = seq.last.toChar
    finalByte match
      case 'A' => Key.Up
      case 'B' => Key.Down
      case 'C' => Key.Right
      case 'D' => Key.Left
      case 'H' => Key.Home
      case 'F' => Key.End
      case '~' =>
        val params = seq.slice(2, seq.length - 1).map(_.toChar).mkString
        params match
          case "1" | "7" => Key.Home
          case "4" | "8" => Key.End
          case "3"       => Key.Delete
          case _         => Key.Unknown
      case _ => Key.Unknown

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
