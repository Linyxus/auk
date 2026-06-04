package auk.tui

class TypewriterSuite extends munit.FunSuite:

  /** Reveal to completion, collecting every intermediate `visible`. */
  private def frames(t: Typewriter): List[String] =
    Iterator.iterate(t)(_.advance).takeWhile(!_.settled).map(_.visible).toList :+ t.full

  test("append grows the buffer but reveals nothing on its own"):
    val t = Typewriter.empty.append("hi").append(" there")
    assertEquals(t.full, "hi there")
    assertEquals(t.shown, 0)
    assertEquals(t.visible, "")

  test("advance reveals a growing prefix and finally settles on the full text"):
    val fs = frames(Typewriter.empty.append("hello world"))
    assert(fs.length > 1, fs.toString) // revealed gradually, not all at once
    assert(fs.forall("hello world".startsWith), fs.toString)
    assertEquals(fs.last, "hello world")
    assert(Typewriter("hello world", 0).advance.shown >= 1) // floor: always moves

  test("settle reveals everything at once, with no glow"):
    val t = Typewriter.empty.append("abcdef").settle
    assert(t.settled)
    assertEquals(t.visible, "abcdef")
    assertEquals(t.coolPrefixLen, t.visible.length) // nothing left glowing

  test("the glow trails the reveal by a bounded window, then drains to settled"):
    val text = "a long stretch of streamed reasoning that reveals over many ticks"
    // While revealing, the glow zone (shown minus the cooled prefix) stays small.
    val zones = Iterator
      .iterate(Typewriter.empty.append(text))(_.advance)
      .takeWhile(!_.settled)
      .map(t => t.visible.length - t.coolPrefixLen)
      .toList
    assert(zones.forall(_ >= 0), zones.toString)
    assert(zones.forall(_ <= 24), s"glow window grew unbounded: ${zones.max}")
    val settled = Iterator.iterate(Typewriter.empty.append(text))(_.advance).find(_.settled).get
    assertEquals(settled.visible, text)
    assertEquals(settled.coolPrefixLen, text.length)

  test("text is fully revealed before the glow has finished cooling"):
    // A short burst is shown quickly, but stays unsettled until the glow drains.
    var t = Typewriter.empty.append("hi there")
    while !t.revealed do t = t.advance
    assert(!t.settled, "the glow should still be draining once text is revealed")
    assertEquals(t.visible, "hi there")
    while !t.settled do t = t.advance
    assertEquals(t.coolPrefixLen, "hi there".length)

  test("the cooled prefix never runs past the shown text"):
    var t = Typewriter.empty.append("alpha beta gamma delta")
    var guard = 0
    while !t.settled && guard < 1000 do
      assert(t.coolPrefixLen <= t.visible.length, s"${t.coolPrefixLen} > ${t.visible.length}")
      t = t.advance
      guard += 1

  test("Chinese characters reveal one at a time, never garbled"):
    val text = "你好，世界" // 5 BMP code points, one UTF-16 unit each
    val fs = frames(Typewriter.empty.append(text))
    assert(fs.forall(text.startsWith), fs.toString)
    assertEquals(fs.last, text)
    // BMP text advances by whole characters, so every prefix is a clean string.
    assert(fs.forall(p => p.length == p.codePointCount(0, p.length)), fs.toString)

  test("emoji (surrogate pairs) are never split into a broken half-character"):
    val text = "go 🚀🎉 now" // 🚀 and 🎉 are non-BMP, two UTF-16 units each
    var t = Typewriter.empty.append(text)
    var guard = 0
    while !t.settled && guard < 1000 do
      val v = t.visible
      // A revealed prefix must never end on the high half of a surrogate pair.
      assert(v.isEmpty || !Character.isHighSurrogate(v.charAt(v.length - 1)), s"dangling surrogate in '$v'")
      assert(text.startsWith(v), v)
      t = t.advance
      guard += 1
    assertEquals(t.visible, text)
