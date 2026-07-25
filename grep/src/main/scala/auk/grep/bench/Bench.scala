package auk.grep.bench

import scala.scalajs.js

import auk.grep.{Grep, Walker}

/** `sbt grepBench`: race the auk-grep engine against ripgrep.
  *
  * Two corpora, each generated once into the OS tmpdir and cached (a version
  * tag in the directory name invalidates it when the layout here changes); both
  * live *outside* any repository on purpose, since ripgrep consults ancestor
  * `.gitignore` files.
  *
  *   - **clean** (`auk-grep-bench-v1`): no ignore files, no hidden files — both
  *     engines do the same logical work, so it measures raw engine speed. Match
  *     counts must agree, which the harness checks.
  *   - **dirty** (`auk-grep-bench-dirty-v1`): a small real tree buried under
  *     junk that a root `.gitignore` excludes (`node_modules/`, `target/`,
  *     `dist/`), with a `.git/` stub so ripgrep actually honors the ignore
  *     rules. Every junk file is written from a *disjoint* word pool that
  *     contains none of the three patterns, so the junk contributes zero matches
  *     and the two engines still report identical counts — while rg prunes the
  *     junk and our engine reads all of it. That timing gap is the measurement.
  *
  * Each section leads with a **walk row** (list files: `Walker.walk` vs
  * `rg --files`) — on the dirty corpus these counts *diverge* (ours sees
  * everything, rg prunes), which is the point, so no parity check there — then
  * the three pattern rows (rare literal / common word / regex) with `auk`, `rg`,
  * and `rg -j1`, keeping the parity WARNING for grep rows.
  *
  * **Timing**: per cell — one untimed warmup, then samples until 5 runs or a
  * 2.5 s budget, whichever first (min 2). The **median** is the headline number;
  * the min is the best case. Ours is timed in-process; rg's wall time includes
  * ~5-10 ms of process start, which is the honest cost of shelling out.
  */
object Bench:

  private def fs = js.Dynamic.global.require("node:fs")
  private def os = js.Dynamic.global.require("node:os")
  private def cp = js.Dynamic.global.require("node:child_process")
  private def pathMod = js.Dynamic.global.require("node:path")
  private def join(a: String, b: String): String = pathMod.join(a, b).asInstanceOf[String]
  private def now(): Double = js.Dynamic.global.performance.now().asInstanceOf[Double]

  // -- deterministic corpora --------------------------------------------------

  // The tag and the two generators below are public because the
  // interpreter-mode bench (`sbt grepInterpBench/run`) drives these very corpora
  // through the REPL worker and generates them the same way when absent, so
  // either command can be the one that runs first. Nothing else is shared.
  val CorpusTag = "v1"

  // clean corpus (unchanged from v1 — same layout/bytes, reuses the cache)
  private val TopDirs = 6
  private val SubDirs = 8
  private val FilesPerDir = 25 // 6 * 8 * 25 = 1200 text files
  private val Needle = "aukGrepBenchNeedle"

  // dirty corpus: a smaller real tree buried in gitignored junk
  private val DirtyTopDirs = 4
  private val DirtySubDirs = 6
  private val DirtyFilesPerDir = 15 // 4 * 6 * 15 = 360 real files
  private val NmPkgDirs = 200
  private val NmFilesPerPkg = 20 // 200 * 20 = 4000 node_modules files
  private val TargetFiles = 50
  private val BlobBytes = 32 * 1024 * 1024 // dist/blob.bin, NUL-marked binary
  private val MinJsBytes = 8 * 1024 * 1024 // node_modules/lib.min.js, one line

  private val CleanSeed = 0x2f6e2b1
  private val DirtySeed = 0x1a2b3c4d

  private var seed: Int = CleanSeed
  private def nextInt(bound: Int): Int =
    seed = seed * 1664525 + 1013904223
    (seed >>> 8) % bound

  // The real-tree word pool: contains "return" and "handler", and lines carry
  // planted handler_ tokens and the rare needle — so real files match all three
  // benchmark patterns.
  private val Words = Array(
    "val", "def", "return", "match", "case", "handler", "config", "buffer",
    "index", "stream", "worker", "result", "engine", "parse", "walk", "entry",
    "flush", "cache", "token", "scope", "yield", "await", "batch", "frame",
    "queue", "shard", "probe", "trace", "guard", "merge", "split", "close"
  )

  // The junk word pool: a DISJOINT lorem-ipsum vocabulary with no "return"
  // substring, no "handler_" substring, and no needle — so every junk file
  // contributes zero matches for all three patterns. That is what lets auk
  // (which searches the junk) and rg (which prunes it) report identical match
  // counts while doing very different work.
  private val JunkWords = Array(
    "lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing",
    "elit", "sed", "eiusmod", "tempor", "incididunt", "labore", "dolore",
    "magna", "aliqua", "enim", "minim", "veniam", "quis", "nostrud",
    "exercitation", "ullamco", "laboris", "nisi", "aliquip", "commodo",
    "consequat", "duis", "aute", "irure", "voluptate", "velit", "esse",
    "cillum", "fugiat", "nulla", "pariatur", "excepteur", "sint", "occaecat",
    "cupidatat", "proident", "culpa", "officia", "deserunt", "mollit", "anim"
  )

  private def line(globalLine: Int): String =
    val sb = new StringBuilder
    val n = 4 + nextInt(8)
    var i = 0
    while i < n do
      if i > 0 then sb.append(' ')
      sb.append(Words(nextInt(Words.length)))
      i += 1
    // A regex-only pattern with real matches: no fixed word, digits vary.
    if globalLine % 211 == 0 then sb.append(" handler_").append(globalLine % 97)
    sb.toString

  private def junkLine(): String =
    val sb = new StringBuilder
    val n = 4 + nextInt(8)
    var i = 0
    while i < n do
      if i > 0 then sb.append(' ')
      sb.append(JunkWords(nextInt(JunkWords.length)))
      i += 1
    sb.toString

  /** Generate (or reuse) the clean corpus; returns (root, files, bytes, cached). */
  def cleanCorpus(): (String, Int, Double, Boolean) =
    val root = join(os.tmpdir().asInstanceOf[String], s"auk-grep-bench-$CorpusTag")
    val manifest = join(root, "MANIFEST")
    if fs.existsSync(manifest).asInstanceOf[Boolean] then
      val parts = fs.readFileSync(manifest, "utf8").asInstanceOf[String].trim.split(' ')
      (root, parts(0).toInt, parts(1).toDouble, true)
    else
      seed = CleanSeed
      fs.rmSync(root, js.Dynamic.literal(recursive = true, force = true))
      var files = 0
      var bytes = 0.0
      var globalLine = 0
      var fileIdx = 0
      def writeFile(p: String, content: String): Unit =
        fs.writeFileSync(p, content, "utf8")
        files += 1
        bytes += content.length
      for t <- 0 until TopDirs; s <- 0 until SubDirs do
        val dir = join(join(root, s"mod$t"), s"pkg$s")
        fs.mkdirSync(dir, js.Dynamic.literal(recursive = true))
        for f <- 0 until FilesPerDir do
          val sb = new StringBuilder
          val lines = 200 + nextInt(300)
          var l = 0
          while l < lines do
            sb.append(line(globalLine)).append('\n')
            globalLine += 1
            l += 1
          // The rare literal: planted in exactly 12 of the 1200 files.
          if fileIdx % 100 == 50 then sb.append("// ").append(Needle).append(" marker\n")
          fileIdx += 1
          writeFile(join(dir, s"file$f.txt"), sb.toString)
      // A few NUL-marked binaries — both engines must skip them. The separator
      // IS the NUL (matching the cached v1 bytes): word NUL word NUL …
      for b <- 0 until 3 do
        val sb = new StringBuilder
        var i = 0
        while i < 65536 do
          sb.append(Words(nextInt(Words.length))).append('\u0000')
          i += 1
        writeFile(join(root, s"blob$b.bin"), sb.toString)
      // One big text file, so the single-large-file path is represented.
      val big = new StringBuilder
      var l = 0
      while big.length < 5 * 1024 * 1024 do
        big.append(line(globalLine)).append('\n')
        globalLine += 1
        l += 1
      writeFile(join(root, "big.log"), big.toString)
      fs.writeFileSync(manifest, s"$files $bytes", "utf8")
      (root, files, bytes, false)

  /** Generate (or reuse) the dirty corpus; returns (root, files, bytes, cached).
    * A small real tree ([[Words]] pool, planted needle and handler_ tokens)
    * buried under junk that a root .gitignore excludes: node_modules/, target/,
    * and dist/, all written from the disjoint [[JunkWords]] pool so they match
    * none of the benchmark patterns. A .git/ stub makes ripgrep honor the
    * .gitignore (it only applies gitignore rules inside a repository). */
  def dirtyCorpus(): (String, Int, Double, Boolean) =
    val root = join(os.tmpdir().asInstanceOf[String], s"auk-grep-bench-dirty-$CorpusTag")
    val manifest = join(root, "MANIFEST")
    if fs.existsSync(manifest).asInstanceOf[Boolean] then
      val parts = fs.readFileSync(manifest, "utf8").asInstanceOf[String].trim.split(' ')
      (root, parts(0).toInt, parts(1).toDouble, true)
    else
      seed = DirtySeed
      fs.rmSync(root, js.Dynamic.literal(recursive = true, force = true))
      var files = 0
      var bytes = 0.0
      def writeFile(p: String, content: String): Unit =
        fs.writeFileSync(p, content, "utf8")
        files += 1
        bytes += content.length
      def mkdir(p: String): Unit =
        fs.mkdirSync(p, js.Dynamic.literal(recursive = true))

      // -- real tree: matches live only here ---------------------------------
      var globalLine = 0
      var fileIdx = 0
      for t <- 0 until DirtyTopDirs; s <- 0 until DirtySubDirs do
        val dir = join(join(root, s"mod$t"), s"pkg$s")
        mkdir(dir)
        for f <- 0 until DirtyFilesPerDir do
          val sb = new StringBuilder
          val lines = 150 + nextInt(201) // 150..350
          var l = 0
          while l < lines do
            sb.append(line(globalLine)).append('\n')
            globalLine += 1
            l += 1
          // The rare literal: planted in every 60th file (6 of 360).
          if fileIdx % 60 == 30 then sb.append("// ").append(Needle).append(" marker\n")
          fileIdx += 1
          writeFile(join(dir, s"file$f.txt"), sb.toString)

      // -- ignore rules + .git stub (so rg honors them) ----------------------
      writeFile(join(root, ".gitignore"), "node_modules/\ntarget/\ndist/\n")
      mkdir(join(root, ".git"))
      writeFile(join(join(root, ".git"), "HEAD"), "ref: refs/heads/main\n")
      for g <- 0 until 5 do
        val sb = new StringBuilder
        val lines = 5 + nextInt(10)
        var l = 0
        while l < lines do
          sb.append(junkLine()).append('\n')
          l += 1
        writeFile(join(join(root, ".git"), s"junk$g"), sb.toString)

      // -- junk: node_modules/ (gitignored) ----------------------------------
      for pkg <- 0 until NmPkgDirs do
        val dir = join(join(root, "node_modules"), s"pkg$pkg")
        mkdir(dir)
        for f <- 0 until NmFilesPerPkg do
          val sb = new StringBuilder
          val lines = 20 + nextInt(41) // 20..60
          var l = 0
          while l < lines do
            sb.append(junkLine()).append('\n')
            l += 1
          writeFile(join(dir, s"mod$f.js"), sb.toString)
      // A single-line minified-style file — auk reads and scans all of it.
      val minjs = new StringBuilder
      while minjs.length < MinJsBytes do
        if minjs.length > 0 then minjs.append(';')
        minjs.append(JunkWords(nextInt(JunkWords.length)))
      writeFile(join(join(root, "node_modules"), "lib.min.js"), minjs.toString)

      // -- junk: target/ (gitignored) ----------------------------------------
      mkdir(join(root, "target"))
      for f <- 0 until TargetFiles do
        val sb = new StringBuilder
        val lines = 100 + nextInt(100) // 100..199
        var l = 0
        while l < lines do
          sb.append(junkLine()).append('\n')
          l += 1
        writeFile(join(join(root, "target"), s"build$f.log"), sb.toString)

      // -- junk: dist/blob.bin (gitignored, NUL-marked binary) ---------------
      // Built programmatically so NO raw NUL byte appears in this source: a
      // 1 KiB block with a NUL every 64 bytes (so NULs appear in the first KB
      // and throughout), repeated to the target size. Written latin1 (one byte
      // per char) so the size is exact and the NUL bytes survive.
      mkdir(join(root, "dist"))
      val block = new StringBuilder
      var j = 0
      while j < 1024 do
        val v = if j % 64 == 0 then 0 else j % 256
        block.append(v.toChar)
        j += 1
      val blockStr = block.toString
      val blob = new StringBuilder
      while blob.length < BlobBytes do
        blob.append(blockStr)
      val blobStr = blob.toString
      fs.writeFileSync(join(join(root, "dist"), "blob.bin"), blobStr, "latin1")
      files += 1
      bytes += blobStr.length

      fs.writeFileSync(manifest, s"$files $bytes", "utf8")
      (root, files, bytes, false)

  // -- measurement ------------------------------------------------------------

  private final case class Cell(medianMs: Double, minMs: Double, runs: Int, matches: Int)

  /** One warmup, then sample until 5 runs or the 2.5 s budget (min 2). */
  private def measure(run: () => Int): Cell =
    run()
    var samples = List.empty[Double]
    var matches = 0
    val budgetStart = now()
    while samples.length < 5 && (samples.length < 2 || now() - budgetStart < 2500.0) do
      val t0 = now()
      matches = run()
      samples ::= now() - t0
    val sorted = samples.sorted
    Cell(sorted(sorted.length / 2), sorted.head, sorted.length, matches)

  // -- ripgrep ----------------------------------------------------------------

  private def rgAvailable(): Boolean =
    val r = cp.spawnSync("rg", js.Array("--version"), js.Dynamic.literal(encoding = "utf8"))
    js.isUndefined(r.error) && r.status.asInstanceOf[Double] == 0.0

  /** Base flags per corpus. Clean has no ignore files, so `--no-ignore` is
    * simplest and honest. Dirty must honor ONLY the corpus's own ignore files:
    * disable the parent/global/exclude sources but keep the local .gitignore. */
  private def rgBaseFlags(dirty: Boolean): List[String] =
    if dirty then
      List("--no-config", "--no-ignore-parent", "--no-ignore-global", "--no-ignore-exclude")
    else
      List("--no-config", "--no-ignore")

  private def rgSpawn(args: js.Array[String]): String =
    val r = cp.spawnSync("rg", args, js.Dynamic.literal(encoding = "utf8", maxBuffer = 1 << 28))
    if !js.isUndefined(r.error) then sys.error(s"rg failed to run: ${r.error}")
    val status = r.status.asInstanceOf[Double]
    if status > 1 then sys.error(s"rg exited $status: ${r.stderr}")
    r.stdout.asInstanceOf[String]

  private def countLines(out: String): Int =
    var count = 0
    var i = out.indexOf('\n')
    while i >= 0 do
      count += 1
      i = out.indexOf('\n', i + 1)
    count

  private def rgGrep(root: String, pattern: String, extra: List[String], dirty: Boolean): Int =
    val args = js.Array((rgBaseFlags(dirty)
      ++ List("-n", "--no-heading", "--color", "never")
      ++ extra ++ List(pattern, root))*)
    countLines(rgSpawn(args))

  private def rgFiles(root: String, dirty: Boolean): Int =
    val args = js.Array((rgBaseFlags(dirty) ++ List("--files", root))*)
    countLines(rgSpawn(args))

  // -- main -------------------------------------------------------------------

  private def fmt(ms: Double): String = f"$ms%.0f ms"
  private def pad(s: String, w: Int): String = s + " " * math.max(0, w - s.length)

  private val patterns = List(
    ("rare literal", Needle),
    ("common word", "return"),
    ("regex", "handler_[0-9]+"),
  )

  private def printRow(label: String, engine: String, c: Cell): Unit =
    println(pad(label, 38) + pad(engine, 9) + pad(fmt(c.medianMs), 10) +
      pad(fmt(c.minMs), 10) + pad(c.runs.toString, 6) + c.matches.toString)

  /** One corpus section: a walk row (no rg -j1, no parity check — on the dirty
    * corpus the file counts MUST diverge), then the three pattern rows with the
    * parity WARNING kept. */
  private def section(title: String, root: String, dirty: Boolean, hasRg: Boolean): Unit =
    println()
    println(title)
    println(pad("pattern", 38) + pad("engine", 9) + pad("median", 10) + pad("min", 10) +
      pad("runs", 6) + "matches")

    // walk row: engine speed to list files; auk sees everything, rg prunes.
    printRow("walk (list files)", "auk", measure(() => Walker.walk(root).count(!_.dir)))
    if hasRg then
      printRow("", "rg", measure(() => rgFiles(root, dirty)))

    for (label, pattern) <- patterns do
      val ours = measure(() => Grep.search(root, pattern).length)
      printRow(s"$label  ($pattern)", "auk", ours)
      if hasRg then
        val rg = measure(() => rgGrep(root, pattern, Nil, dirty))
        val rg1 = measure(() => rgGrep(root, pattern, List("-j", "1"), dirty))
        printRow("", "rg", rg)
        printRow("", "rg -j1", rg1)
        if rg.matches != ours.matches then
          println(s"  WARNING: match counts differ (auk=${ours.matches}, rg=${rg.matches}) — " +
            "the engines are not doing the same work; timings above are not comparable")

  def main(args: Array[String]): Unit =
    def timed[A](f: => A): (A, Double) =
      val t0 = now()
      val a = f
      (a, now() - t0)
    val (cleanRes, cleanGenMs) = timed(cleanCorpus())
    val (dirtyRes, dirtyGenMs) = timed(dirtyCorpus())
    val (cleanRoot, cleanFiles, cleanBytes, cleanCached) = cleanRes
    val (dirtyRoot, dirtyFiles, dirtyBytes, dirtyCached) = dirtyRes
    def statLabel(cached: Boolean, ms: Double): String =
      if cached then "cached" else f"generated in ${ms / 1000}%.1f s"

    println("auk-grep benchmark")
    println(f"clean corpus: $cleanFiles files, ${cleanBytes / 1024 / 1024}%.1f MB " +
      f"(${statLabel(cleanCached, cleanGenMs)})")
    println(s"  root: $cleanRoot")
    println(f"dirty corpus: $dirtyFiles files, ${dirtyBytes / 1024 / 1024}%.1f MB " +
      f"(${statLabel(dirtyCached, dirtyGenMs)})")
    println(s"  root: $dirtyRoot")

    val hasRg = rgAvailable()
    if !hasRg then println("NOTE: rg not found on PATH — benchmarking the auk engine only")

    section("=== clean corpus — engine speed; no ignore files ===",
      cleanRoot, dirty = false, hasRg)
    section("=== dirty corpus — work avoidance; junk is gitignored, needles only in real files ===",
      dirtyRoot, dirty = true, hasRg)

    println()
    println("auk is timed in-process; rg timings include ~5-10 ms of process start (the honest cost of shelling out).")
