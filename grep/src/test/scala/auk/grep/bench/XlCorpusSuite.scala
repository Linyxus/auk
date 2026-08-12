package auk.grep.bench

/** Two invariants of the XL corpus that are cheap to check and expensive to
  * discover the hard way. Neither writes a byte: the corpus itself is generated
  * only by `./mill grepBenchXL`, never by the test suite.
  */
class XlCorpusSuite extends munit.FunSuite:

  /** Every match in the XL corpus is planted, which is what lets the MANIFEST
    * record exact counts for the bench to check both engines against. That holds
    * only while no vocabulary word can match a benchmark pattern on its own —
    * one "returns" in the pool and every count in the table drifts, silently and
    * plausibly. (Words are joined by spaces, so no match can span two of them.)
    */
  test("no vocabulary word can match a benchmark pattern") {
    val patterns = List("return", "handler", "aukGrepBenchNeedle")
    for
      (pool, name) <- List(XlCorpus.Words -> "XlCorpus.Words", Bench.JunkWords -> "Bench.JunkWords")
      word <- pool
      pattern <- patterns
    do
      assert(!word.contains(pattern), s"$name contains '$word', which matches '$pattern'")
  }

  /** The corpus is a pure function of its tag, so its bytes must not move
    * without one. This pins the line pool — the LCG, the vocabulary and the line
    * shape — which is everything the file contents are built from. If it fails
    * because the generator changed on purpose, bump `XlCorpus.Tag` in the same
    * commit and update the fingerprint; a stale cached corpus under the old tag
    * would otherwise be silently mixed with new numbers.
    */
  test("the line pool is deterministic at the fixed seed") {
    assertEquals(XlCorpus.linePoolFingerprint(), "8192:380009:5edf678d")
  }
