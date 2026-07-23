package auk.grep

import scala.scalajs.js

/** A differential harness: for the SAME tree and pattern, our engine's matches
  * must equal ripgrep's, on the full sorted set of (relative path, 1-based line,
  * line text). rg is the correctness ORACLE for the matcher, guarding the
  * stage-4/5 rewrite of the per-line matching path.
  *
  * Scope is deliberately narrow — MATCHER equivalence only. Generated trees have
  * no dotfiles, no `.gitignore`, and no `.git`, so pruning is irrelevant and both
  * engines see the same files (asserted per tree as a precondition). Our ignore
  * semantics intentionally diverge from rg's and are pinned in [[GrepSuite]], not
  * here. Content is ASCII with LF-only line endings (our lone-CR handling also
  * diverges from rg and is pinned in [[GrepSuite]]).
  *
  * Everything is seeded (a fixed per-test LCG — no clock, no `Math.random`), so a
  * mismatch is replayable: the failure carries the seed, the pattern, the tree's
  * tmpdir (left on disk on failure), and the first differing entries. On a
  * machine without ripgrep the whole suite skips cleanly via `assume`.
  */
class DifferentialSuite extends munit.FunSuite:

  private def cp = js.Dynamic.global.require("node:child_process")
  private def os = js.Dynamic.global.require("node:os")
  private def join(a: String, b: String): String = Node.path.join(a, b).asInstanceOf[String]
  private def rel(root: String, abs: String): String = Node.path.relative(root, abs).asInstanceOf[String]

  private def rgAvailable(): Boolean =
    try
      val r = cp.spawnSync("rg", js.Array("--version"), js.Dynamic.literal(encoding = "utf8"))
      js.isUndefined(r.error) && r.status.asInstanceOf[Double] == 0.0
    catch case _: Throwable => false

  // -- seeded RNG (the benchmark's LCG family) --------------------------------

  private final class Lcg(private var s: Int):
    def nextInt(bound: Int): Int =
      s = s * 1664525 + 1013904223
      (s >>> 8) % bound
    def between(lo: Int, hi: Int): Int = lo + nextInt(hi - lo + 1) // inclusive
    def bool(): Boolean = nextInt(2) == 0
    def pick[A](xs: IndexedSeq[A]): A = xs(nextInt(xs.length))
    def pickStr(xs: Array[String]): String = xs(nextInt(xs.length))

  // -- tree generation (ASCII, LF-only, names in [a-z0-9_]) -------------------

  private val Words = Array(
    "the", "quick", "brown", "fox", "lorem", "ipsum", "dolor", "alpha", "beta",
    "gamma", "delta", "foo", "bar", "baz", "qux", "data", "node", "list", "map",
    "set", "key", "val", "code", "test", "file", "line", "text", "word", "item",
    "zero", "one", "two", "three", "cat", "dog", "run", "walk", "grep"
  )
  private val Punct = Array(".", ",", "-", "_", "+", "!", "?", ";", "(", ")", "[", "]")
  private val NameLetters: IndexedSeq[Char] = ('a' to 'z').toIndexedSeq

  /** A unique name: 2-4 letters then the running `counter` (letters-only prefix
    * + a numeric suffix makes the split unambiguous, so no two names collide). */
  private def genName(rng: Lcg, counter: Int): String =
    val n = rng.between(2, 4)
    val sb = new StringBuilder
    var i = 0
    while i < n do
      sb.append(rng.pick(NameLetters))
      i += 1
    sb.append(counter).toString

  private def genLine(rng: Lcg): String =
    val n = rng.between(1, 8)
    val sb = new StringBuilder
    var i = 0
    while i < n do
      if i > 0 then sb.append(' ')
      rng.nextInt(6) match
        case 0 => sb.append(rng.between(0, 999).toString)
        case 1 => sb.append(rng.pickStr(Punct))
        case _ => sb.append(rng.pickStr(Words))
      i += 1
    sb.toString

  private def genContent(rng: Lcg): String =
    val lines = rng.between(0, 25)
    val sb = new StringBuilder
    var i = 0
    while i < lines do
      if rng.nextInt(6) != 0 then sb.append(genLine(rng)) // else an empty line
      sb.append('\n')
      i += 1
    val s = sb.toString
    if s.nonEmpty && rng.bool() then s.dropRight(1) else s // sometimes no final newline

  /** Generate a tree under `root` (depth <= 3, ~20-50 files) and return every
    * file's content joined by newlines (for corpus-derived pattern sampling). A
    * single NUL-marked binary is planted in roughly half the trees. */
  private def genTree(root: String, rng: Lcg): String =
    var counter = 0
    var made = 0
    val corpus = new StringBuilder
    val targetFiles = rng.between(20, 50)
    def writeFile(dir: String): Unit =
      val content = genContent(rng)
      Node.fs.writeFileSync(join(dir, genName(rng, counter) + ".txt"), content, "utf8")
      counter += 1
      made += 1
      corpus.append(content).append('\n')
    def go(dir: String, depth: Int): Unit =
      var files = rng.between(3, 7)
      while files > 0 && made < targetFiles do
        writeFile(dir)
        files -= 1
      if depth < 3 && made < targetFiles then
        var subs = rng.between(1, 3)
        while subs > 0 && made < targetFiles do
          val sub = join(dir, genName(rng, counter))
          counter += 1
          Node.fs.mkdirSync(sub, js.Dynamic.literal(recursive = true))
          go(sub, depth + 1)
          subs -= 1
    go(root, 0)
    // A NUL-marked binary sometimes: both engines must skip it. NUL is early (so
    // both detectors fire) and built with 0.toChar, so no raw NUL in this source.
    if rng.bool() then
      val bin = "binary" + 0.toChar + genLine(rng) + " " + rng.pickStr(Words)
      Node.fs.writeFileSync(join(root, "bin" + counter + ".dat"), bin, "utf8")
    corpus.toString

  // -- pattern generation (dialect-safe AST) ----------------------------------

  private val EscPunct = Array(".", "+", "*", "?", "(", ")", "[", "]", "{", "}", "|", "^", "$", "-")
  private val SimpleClasses = Array("[a-z]", "[0-9]", "[aeiou]", "[a-f]", "[^a]", "[^e]")
  private val Letters: IndexedSeq[Char] = ('a' to 'z').toIndexedSeq
  private val Digits: IndexedSeq[Char] = ('0' to '9').toIndexedSeq

  private def genAtom(rng: Lcg): String =
    rng.nextInt(8) match
      case 0 => rng.pick(Letters).toString
      case 1 => rng.pick(Digits).toString
      case 2 => "\\d"
      case 3 => "\\w"
      case 4 => "."
      case 5 => rng.pickStr(SimpleClasses)
      case 6 => "\\" + rng.pickStr(EscPunct)
      case _ => " "

  private def genQuant(rng: Lcg): String =
    rng.nextInt(6) match
      case 0 => "*"
      case 1 => "+"
      case 2 => "?"
      case 3 => "{" + rng.between(0, 2) + "}"
      case 4 =>
        val a = rng.between(0, 2)
        val b = a + rng.between(0, 2)
        "{" + a + "," + b + "}"
      case _ => "" // no quantifier

  /** One concatenation unit. A group is emitted WITHOUT an outer quantifier —
    * never `(...)+` — so our JS-RegExp-backed engine can't hit catastrophic
    * backtracking (rg's Rust engine is linear regardless); only atoms are
    * quantified, keeping match time polynomial on short lines. */
  private def genQuantified(rng: Lcg, depth: Int): String =
    if depth > 0 && rng.nextInt(5) == 0 then
      val inner = genAlt(rng, depth - 1)
      if rng.bool() then "(" + inner + ")" else "(?:" + inner + ")"
    else genAtom(rng) + genQuant(rng)

  private def genConcat(rng: Lcg, depth: Int): String =
    val n = rng.between(1, 3)
    val sb = new StringBuilder
    var i = 0
    while i < n && sb.length < 24 do
      if i > 0 && rng.nextInt(5) == 0 then sb.append("\\b") // an un-quantified boundary
      sb.append(genQuantified(rng, depth))
      i += 1
    if sb.isEmpty then genAtom(rng) else sb.toString

  private def genAlt(rng: Lcg, depth: Int): String =
    val branches = rng.between(1, 2)
    (0 until branches).map(_ => genConcat(rng, depth)).mkString("|")

  /** A full generated pattern: a (possibly anchored) alternation. Valid by
    * construction — never truncated — with bounded depth and unit counts. */
  private def genPattern(rng: Lcg, corpus: Array[String]): String =
    val pre = if rng.nextInt(4) == 0 then "^" else ""
    val post = if rng.nextInt(4) == 0 then "$" else ""
    pre + genAlt(rng, 2) + post

  private def escapeLiteral(s: String): String =
    val sb = new StringBuilder
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if "\\.+*?()[]{}|^$-".indexOf(c.toInt) >= 0 then sb.append('\\')
      sb.append(c)
      i += 1
    sb.toString

  /** A pattern derived from real tree content: an escaped substring of a sampled
    * line, sometimes lightly mutated (append a small quantifier, or replace the
    * last char with `.`). Dialect-safe, and likely to actually match. */
  private def genCorpusPattern(rng: Lcg, corpus: Array[String]): String =
    if corpus.isEmpty then genPattern(rng, corpus)
    else
      val line = rng.pickStr(corpus)
      if line.length < 2 then genPattern(rng, corpus)
      else
        val start = rng.nextInt(line.length - 1)
        val len = rng.between(2, math.min(15, line.length - start))
        val sub = line.substring(start, start + len)
        rng.nextInt(3) match
          case 0 => escapeLiteral(sub)
          case 1 => escapeLiteral(sub) + rng.pickStr(Array("?", "*", "+"))
          case _ => escapeLiteral(sub.substring(0, sub.length - 1)) + "."

  // -- rg oracle --------------------------------------------------------------

  private def rgFiles(root: String): List[String] =
    val r = cp.spawnSync(
      "rg",
      js.Array("--files", "--no-config", "--no-ignore", "--hidden", root),
      js.Dynamic.literal(encoding = "utf8", maxBuffer = 1 << 28)
    )
    if !js.isUndefined(r.error) then fail(s"rg --files failed to run: ${r.error}")
    val status = r.status.asInstanceOf[Double]
    if status >= 2 then fail(s"rg --files exited $status: ${r.stderr}")
    r.stdout.asInstanceOf[String].split("\n").toList.filter(_.nonEmpty).map(p => rel(root, p))

  private def rgSearch(root: String, pattern: String): List[(String, Int, String)] =
    val r = cp.spawnSync(
      "rg",
      js.Array("--no-config", "--no-ignore", "--hidden", "-n", "--no-heading",
        "--color", "never", "-e", pattern, root),
      js.Dynamic.literal(encoding = "utf8", maxBuffer = 1 << 28)
    )
    if !js.isUndefined(r.error) then fail(s"rg failed to run: ${r.error}")
    val status = r.status.asInstanceOf[Double]
    if status >= 2 then
      fail(s"rg rejected pattern (exit $status) — GENERATOR bug: pattern=<$pattern> stderr=${r.stderr}")
    parseRg(r.stdout.asInstanceOf[String], root)

  /** Parse rg `path:line:text` records. A record whose middle field is not an
    * integer (e.g. a "binary file matches" summary) is dropped: our engine skips
    * binaries, so neither side contributes binary matches — but if the engine
    * regressed and started searching them, its extra rows would still fail the
    * comparison. Paths hold no colons (names are [a-z0-9_], tmpdir has none), so
    * splitting on the first two colons is unambiguous. */
  private def parseRg(out: String, root: String): List[(String, Int, String)] =
    out.split("\n").toList.flatMap { record =>
      if record.isEmpty then None
      else
        val c1 = record.indexOf(':')
        val c2 = if c1 < 0 then -1 else record.indexOf(':', c1 + 1)
        if c1 < 0 || c2 < 0 then None
        else
          record.substring(c1 + 1, c2).toIntOption match
            case Some(n) => Some((rel(root, record.substring(0, c1)), n, record.substring(c2 + 1)))
            case None    => None
    }

  // -- the differential check -------------------------------------------------

  private def ours(root: String, pattern: String): List[(String, Int, String)] =
    Grep.search(root, pattern).map(m => (rel(root, m.path), m.line, m.text))

  private def diff(root: String, pattern: String, seed: Int): Unit =
    val a = ours(root, pattern).sorted
    val b = rgSearch(root, pattern).sorted
    // The NUL binary (the only .dat; text files are all .txt) must contribute to
    // NEITHER side. Set equality alone would bless the one drift where both
    // engines start searching binaries together; this pins the invariant itself.
    val binHits = (a ++ b).filter(_._1.endsWith(".dat"))
    assert(binHits.isEmpty, s"binary contributed matches (seed=$seed, pattern=<$pattern>): $binHits")
    if a != b then
      val aSet = a.toSet
      val bSet = b.toSet
      fail(
        s"""differential mismatch (matcher divergence)
           |  seed    = $seed
           |  pattern = <$pattern>
           |  tree    = $root
           |  counts: ours=${a.length} rg=${b.length}
           |  only in ours (first 4): ${a.filterNot(bSet.contains).take(4)}
           |  only in rg   (first 4): ${b.filterNot(aSet.contains).take(4)}""".stripMargin
      )

  /** Precondition: on these dotfile-free trees our (pruning) walk must see the
    * exact same files rg does. A failure means the generator produced something
    * with hidden/ignore semantics — fail loudly, it is not a matcher result. */
  private def precondition(root: String, seed: Int): Unit =
    val ourFiles = Walker.walk(root).filter(!_.dir).map(e => rel(root, e.path)).sorted
    val rgList = rgFiles(root).sorted
    if ourFiles != rgList then
      val oset = ourFiles.toSet
      val rset = rgList.toSet
      fail(
        s"""tree generator broken: Walker.walk file set != rg --files
           |  seed = $seed
           |  tree = $root
           |  only in walk: ${ourFiles.filterNot(rset.contains)}
           |  only in rg  : ${rgList.filterNot(oset.contains)}""".stripMargin
      )

  /** Build a seeded tree, assert the file-set precondition, then run `n`
    * generated patterns against both engines. The tree is removed only on
    * success, so a failure leaves the exact reproducer on disk. */
  private def run(seed: Int, n: Int, mkPattern: (Lcg, Array[String]) => String): Unit =
    assume(rgAvailable(), "rg not on PATH — differential suite skipped")
    val rng = Lcg(seed)
    val root = Node.fs.mkdtempSync(join(os.tmpdir().asInstanceOf[String], "auk-grep-diff-")).asInstanceOf[String]
    val corpus = genTree(root, rng).split("\n").filter(_.nonEmpty)
    precondition(root, seed)
    var i = 0
    while i < n do
      diff(root, mkPattern(rng, corpus), seed)
      i += 1
    Node.fs.rmSync(root, js.Dynamic.literal(recursive = true, force = true))

  // -- tests ------------------------------------------------------------------

  test("differential: corpus-derived literal-heavy patterns over a mixed tree"):
    run(0x51a7e01, 25, genCorpusPattern)

  test("differential: class and quantifier patterns"):
    run(0x2b9d114, 25, genPattern)

  test("differential: anchor and alternation patterns"):
    run(0x77c3f02, 25, genPattern)

  test("differential: corpus-derived mutated patterns"):
    run(0x1e6a5d3, 25, genCorpusPattern)

  test("differential: handpicked zero-width and anchor edge patterns"):
    assume(rgAvailable(), "rg not on PATH — differential suite skipped")
    val edge = List("x*", "(?:a|)", "^", "$", "^$", "a{0,2}", "\\bthe\\b", "[^a]", ".*", "\\d*")
    val seed = 0x9f0b28c
    val rng = Lcg(seed)
    val root = Node.fs.mkdtempSync(join(os.tmpdir().asInstanceOf[String], "auk-grep-diff-")).asInstanceOf[String]
    genTree(root, rng)
    precondition(root, seed)
    edge.foreach(p => diff(root, p, seed))
    Node.fs.rmSync(root, js.Dynamic.literal(recursive = true, force = true))
