package auk.library

/** [[Memory]] / `MemoryImpl` — one Markdown file per memory under a base dir, each
  * with a `description` frontmatter. Exercised against a real Node file system via a
  * per-test temp dir (see [[LibSuite]]); the pure helpers are tested directly. */
class MemorySuite extends LibSuite:

  /** A memory store rooted at a fresh `mem/` under the test's temp dir — which does
    * not exist yet, so `write` must create it. */
  private def store(d: FsDir): MemoryImpl = new MemoryImpl((d.path / "mem").toString)

  tmp.test("an empty store lists nothing and overviews as empty"): d =>
    val m = store(d)
    assertEquals(m.all, Nil)
    assertEquals(m.get("x"), None)
    assert(captured(m.overview()).contains("(no memories stored)"))

  tmp.test("write creates the store and round-trips id, description, content"): d =>
    val m = store(d)
    m.write("build", "how to build and test", "Use sbt test.\nWorker suites need vendorRepl.")
    val e = m.get("build").getOrElse(fail("expected the memory to exist"))
    assertEquals(e.id, "build")
    assertEquals(e.description, "how to build and test")
    assertEquals(e.content, "Use sbt test.\nWorker suites need vendorRepl.")
    assertEquals(m.all.map(_.id), List("build"))

  tmp.test("reusing an id overwrites both description and content"): d =>
    val m = store(d)
    m.write("k", "first", "one")
    m.write("k", "second", "two")
    assertEquals(m.all.size, 1)
    val e = m.get("k").get
    assertEquals((e.description, e.content), ("second", "two"))

  tmp.test("delete removes a memory and tolerates a missing id"): d =>
    val m = store(d)
    m.write("k", "d", "c")
    m.delete("k")
    assertEquals(m.get("k"), None)
    m.delete("never-existed") // no throw

  tmp.test("a missing id reads as a not-found note and gets None"): d =>
    val m = store(d)
    assertEquals(m.get("ghost"), None)
    assert(captured(m.read("ghost")).contains("no memory with id 'ghost'"))

  tmp.test("overview lists `id — description`, sorted by id"): d =>
    val m = store(d)
    m.write("zeta", "the last", "z")
    m.write("alpha", "the first", "a")
    val out = captured(m.overview())
    assert(out.contains("alpha — the first"), out)
    assert(out.contains("zeta — the last"), out)
    assert(out.indexOf("alpha") < out.indexOf("zeta"), s"not sorted: $out")

  tmp.test("read prints a header then the full content"): d =>
    val m = store(d)
    m.write("k", "desc", "line1\nline2")
    val out = captured(m.read("k"))
    assert(out.contains("# k — desc"), out)
    assert(out.contains("line1\nline2"), out)

  tmp.test("content that itself contains a --- fence round-trips through disk"): d =>
    val m = store(d)
    m.write("k", "d", "top\n---\nbottom\n\ntail")
    assertEquals(m.get("k").map(_.content), Some("top\n---\nbottom\n\ntail"))

  tmp.test("write rejects an unsafe id and an empty description"): d =>
    val m = store(d)
    interceptContains("invalid id")(m.write("../escape", "d", "c"))
    interceptContains("invalid id")(m.write("a/b", "d", "c"))
    interceptContains("description")(m.write("ok", "  \n ", "c"))

  // -- pure helpers ------------------------------------------------------------

  test("validId accepts slugs and rejects path-y / empty ids"):
    assertEquals(MemoryImpl.validId("ok.slug-1_2"), "ok.slug-1_2")
    interceptContains("invalid id")(MemoryImpl.validId("a/b"))
    interceptContains("invalid id")(MemoryImpl.validId(".."))
    interceptContains("invalid id")(MemoryImpl.validId("."))
    interceptContains("invalid id")(MemoryImpl.validId(""))

  test("oneLine collapses a multi-line description to a single trimmed line"):
    assertEquals(MemoryImpl.oneLine("a\n  b \n\n c"), "a b c")

  test("serialize/parse round-trip, preserving a colon in the description and --- in content"):
    val raw = MemoryImpl.serialize("desc: with a colon", "a\n---\nb")
    val (desc, content) = MemoryImpl.parse(raw)
    assertEquals(desc, "desc: with a colon")
    assertEquals(content, "a\n---\nb")

  test("parse treats a frontmatter-less file as all content"):
    assertEquals(MemoryImpl.parse("just text\nmore"), ("", "just text\nmore"))

  test("renderOverview formats the header and one line per memory"):
    assertEquals(MemoryImpl.renderOverview(Nil), "(no memories stored)")
    val out = MemoryImpl.renderOverview(List(MemoryEntryImpl("a", "first", "x"), MemoryEntryImpl("b", "second", "y")))
    assert(out.startsWith("Memories (2):"), out)
    assert(out.contains("  a — first"), out)
    assert(out.contains("  b — second"), out)
