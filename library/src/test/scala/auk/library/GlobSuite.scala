package auk.library

/** Glob matching ([[FsDir.glob]]) over real directory trees. This drives the
  * glob→regex translation behind `glob`: segment-bounded `*`, segment-spanning
  * `**` (including its zero-directory case), single-char `?`, and the escaping of
  * regex metacharacters so a `.` in a pattern is a literal dot, not "any char".
  */
class GlobSuite extends LibSuite:

  /** Paths of the matches, relative to `dir`, sorted — stable and readable. */
  private def matches(dir: FsDir, pattern: String): List[String] =
    val prefix = dir.path.toString + "/"
    dir.glob(pattern).entries.map(_.path.toString.stripPrefix(prefix)).sorted

  /** Lay down a small mixed tree:
    *   a.scala  b.txt  p/  p/c.scala  p/d.txt  p/q/  p/q/e.scala
    */
  private def tree(d: FsDir): Unit =
    d.file("a.scala").write("")
    d.file("b.txt").write("")
    d.dir("p").makedir()
    d.dir("p").file("c.scala").write("")
    d.dir("p").file("d.txt").write("")
    d.dir("p").dir("q").makedir()
    d.dir("p").dir("q").file("e.scala").write("")

  tmp.test("`*` stays within one segment (top-level only)"): d =>
    tree(d)
    assertEquals(matches(d, "*.scala"), List("a.scala"))

  tmp.test("`*` matches every top-level entry, files and dirs alike"): d =>
    tree(d)
    assertEquals(matches(d, "*"), List("a.scala", "b.txt", "p"))

  tmp.test("`**/` spans segments and also matches zero directories"): d =>
    tree(d)
    assertEquals(matches(d, "**/*.scala"), List("a.scala", "p/c.scala", "p/q/e.scala"))

  tmp.test("a bare `**` matches the entire subtree (files and dirs)"): d =>
    tree(d)
    assertEquals(
      matches(d, "**"),
      List("a.scala", "b.txt", "p", "p/c.scala", "p/d.txt", "p/q", "p/q/e.scala")
    )

  tmp.test("`**` without a following slash still spans segments"): d =>
    tree(d)
    assertEquals(matches(d, "**.scala"), List("a.scala", "p/c.scala", "p/q/e.scala"))

  tmp.test("the zero-directory case of `**/` matches a top-level file"): d =>
    d.file("z.txt").write("")
    d.dir("p").makedir()
    d.dir("p").file("z.txt").write("")
    assertEquals(matches(d, "**/z.txt"), List("p/z.txt", "z.txt"))

  tmp.test("an interior segment pins where the match starts"): d =>
    tree(d)
    assertEquals(matches(d, "p/*.txt"), List("p/d.txt"))

  tmp.test("`?` matches exactly one character within a segment"): d =>
    d.file("abc").write("")
    d.file("axc").write("")
    d.file("ac").write("")
    d.file("abbc").write("")
    assertEquals(matches(d, "a?c"), List("abc", "axc"))

  tmp.test("`.` in a pattern is a literal dot, not any-char"): d =>
    d.file("a.b").write("")
    d.file("axb").write("")
    assertEquals(matches(d, "a.b"), List("a.b"))

  tmp.test("`+` is escaped to a literal plus"): d =>
    d.file("a+b").write("")
    d.file("ab").write("")
    d.file("aab").write("")
    assertEquals(matches(d, "a+b"), List("a+b"))

  tmp.test("parentheses are escaped, not treated as a group"): d =>
    d.file("f(x)").write("")
    d.file("fx").write("")
    assertEquals(matches(d, "f(x)"), List("f(x)"))

  tmp.test("brackets, braces, anchors, and the alternation bar are escaped to literals"): d =>
    d.file("a[b]c").write("")
    d.file("a{b}c").write("")
    d.file("a^c").write("")
    d.file("a$c").write("")
    d.file("a|c").write("")
    assertEquals(matches(d, "a[b]c"), List("a[b]c"))
    assertEquals(matches(d, "a{b}c"), List("a{b}c"))
    assertEquals(matches(d, "a^c"), List("a^c"))
    assertEquals(matches(d, "a$c"), List("a$c"))
    assertEquals(matches(d, "a|c"), List("a|c"))

  tmp.test("`?` does not cross a path separator"): d =>
    d.dir("p").makedir()
    d.dir("p").file("c.scala").write("")
    assertEquals(matches(d, "p?c.scala"), Nil) // ? is [^/], so it cannot match '/'

  tmp.test("an empty glob pattern matches nothing"): d =>
    tree(d)
    assertEquals(matches(d, ""), Nil)

  tmp.test("known wart: a trailing `/**` matches descendants but not the directory itself"): d =>
    // `p/**` is `^p/.*$`, so `p` is excluded while `**/p` would include it. Pinned
    // here to document the asymmetry; flip if globToRegex is ever made symmetric.
    d.dir("p").makedir()
    d.dir("p").file("c.scala").write("")
    assertEquals(matches(d, "p/**"), List("p/c.scala"))

  tmp.test("a pattern that matches nothing returns an empty list"): d =>
    tree(d)
    assertEquals(matches(d, "*.md"), Nil)

  tmp.test("glob never returns the directory itself"): d =>
    d.file("only.txt").write("")
    assert(!d.glob("**").entries.exists(_.path == d.path))
