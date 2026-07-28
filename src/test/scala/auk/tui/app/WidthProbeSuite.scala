package auk.tui.app

import auk.tui.render.{Ansi, Width}

class WidthProbeSuite extends munit.FunSuite:

  /** Feed a byte string, collecting every passthrough byte and the done flag. */
  private def feedAll(p: WidthProbe, s: String): (List[Int], Boolean) =
    var leaked = List.empty[Int]
    var done = false
    for ch <- s do
      val step = p.feed(ch.toInt)
      leaked = leaked ++ step.passthrough
      if step.done then done = true
    (leaked, done)

  test("the request prints each probe glyph at column 0 and erases the line") {
    val req = WidthProbe().request
    for glyph <- List("✅", "😀", "①", "一") do
      assert(req.contains(Ansi.CarriageReturn + glyph + Ansi.CSI + "6n"), s"missing probe for $glyph")
    assert(req.startsWith(Ansi.SyncBegin), "the probe must ride in a sync wrap (no flash)")
    assert(req.endsWith(Ansi.CarriageReturn + Ansi.EraseToEol + Ansi.SyncEnd), "the probe line must be erased")
  }

  test("a full reply adopts the terminal's widths (old-narrow emoji, ambiguous-wide)") {
    val p = WidthProbe()
    try
      // ✅→1, 😀→1, ①→2, 一→2: an old-wcwidth terminal in a CJK configuration.
      val (leaked, done) = feedAll(p, "\u001b[5;2R\u001b[5;2R\u001b[5;3R\u001b[5;3R")
      assertEquals(leaked, Nil)
      assert(done, "four CPRs must complete the probe")
      assertEquals(p.settle(), Nil)
      assertEquals(Width.displayWidth(0x2705), 1, "✅ must follow the terminal")
      assertEquals(Width.displayWidth(0x1f600), 1, "😀 must follow the terminal")
      assertEquals(Width.displayWidth(0x2460), 2, "① must follow the terminal")
      assertEquals(Width.displayWidth(0x4e00), 2, "CJK is untouched")
    finally Width.resetAdopted()
  }

  test("a reply whose CJK sanity glyph measures wrong is distrusted wholesale") {
    val p = WidthProbe()
    try
      // Plausible-looking emoji answers, but 一 measures 3 — mojibake or a
      // filtering multiplexer; nothing from this terminal can be believed.
      val (_, done) = feedAll(p, "\u001b[5;2R\u001b[5;2R\u001b[5;2R\u001b[5;4R")
      assert(done)
      p.settle()
      assertEquals(Width.displayWidth(0x2705), 2, "a distrusted probe must keep the defaults")
      assertEquals(Width.displayWidth(0x2460), 1)
    finally Width.resetAdopted()
  }

  test("keystrokes racing the reply pass through untouched, including CSI-shaped ones") {
    val p = WidthProbe()
    try
      val (leadingKeys, _) = feedAll(p, "a\u001b[Ab")
      assertEquals(leadingKeys, "a\u001b[Ab".map(_.toInt).toList, "an arrow key must leak intact")
      val (rest, done) = feedAll(p, "\u001b[1;3R\u001b[1;3R\u001b[1;2R\u001b[1;3R")
      assertEquals(rest, Nil)
      assert(done, "the probe must still complete around interleaved input")
      p.settle()
      assertEquals(Width.displayWidth(0x2705), 2)
      assertEquals(Width.displayWidth(0x1f600), 2)
    finally Width.resetAdopted()
  }

  test("a timed-out partial reply keeps the defaults and releases held bytes") {
    val p = WidthProbe()
    try
      val (leaked, done) = feedAll(p, "\u001b[5;2R\u001b[5;2R\u001b[5;")
      assertEquals(leaked, Nil)
      assert(!done)
      assertEquals(p.settle(), "\u001b[5;".map(_.toInt).toList, "held bytes must be released on timeout")
      assertEquals(Width.displayWidth(0x2705), 2, "a partial probe must not adopt anything")
    finally Width.resetAdopted()
  }

  test("a malformed CPR (no column) records an ignored width and cannot poison the classes") {
    val p = WidthProbe()
    try
      val (_, done) = feedAll(p, "\u001b[5R\u001b[5;2R\u001b[5;2R\u001b[5;3R")
      assert(done)
      p.settle()
      // First reply parsed to -1 → width -2 → Width.adopt ignores that class.
      assertEquals(Width.displayWidth(0x2705), 2, "the malformed class must keep its default")
      assertEquals(Width.displayWidth(0x1f600), 1, "well-formed classes still adopt")
    finally Width.resetAdopted()
  }
