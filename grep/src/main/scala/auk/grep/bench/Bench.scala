package auk.grep.bench

import scala.scalajs.js

import auk.grep.{Grep, Match, Walker}

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
  *
  * There is a third, **opt-in** corpus: `sbt grepBenchXL` runs [[runXL]] on
  * [[XlCorpus]], a ~1.1 GB monorepo-scale tree that answers stage 6's
  * parallelism question (single-threaded auk against a parallel rg at a scale
  * where core count matters). It is generated on demand and cached like the
  * others, but `sbt grepBench` never touches it: neither the disk nor the
  * multi-second rows belong in the bench that runs before and after every stage.
  */
object Bench:

  // Node accessors and the junk vocabulary are `private[bench]` rather than
  // `private` so [[XlCorpus]] can build on exactly the same primitives; nothing
  // outside this package sees them, and the visibility is all that changed.
  private[bench] def fs = js.Dynamic.global.require("node:fs")
  private[bench] def os = js.Dynamic.global.require("node:os")
  private def cp = js.Dynamic.global.require("node:child_process")
  private[bench] def pathMod = js.Dynamic.global.require("node:path")
  private[bench] def join(a: String, b: String): String = pathMod.join(a, b).asInstanceOf[String]
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
  private[bench] val JunkWords = Array(
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

  /** `rg --count`: the same search reporting `path:n` per file instead of every
    * matching line. Only the XL tier uses it, as an output-cost control — at
    * ~170k matches rg's formatting and our decoding of its stdout are a real
    * part of the `rg` cell, and this row is that same search without them. The
    * summed counts are also an independent parity oracle. */
  private def rgCount(root: String, pattern: String, dirty: Boolean): Int =
    val args = js.Array((rgBaseFlags(dirty)
      ++ List("--count", "--no-heading", "--color", "never")
      ++ List(pattern, root))*)
    var total = 0
    for l <- rgSpawn(args).linesIterator do
      val i = l.lastIndexOf(':')
      if i >= 0 then total += l.substring(i + 1).toInt
    total

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

  private def timed[A](f: => A): (A, Double) =
    val t0 = now()
    val a = f
    (a, now() - t0)

  // -- the XL tier ------------------------------------------------------------

  /** Exactly 3 timed runs after one untimed warmup, no time budget.
    *
    * [[measure]]'s 2.5 s budget exists to keep a millisecond-scale bench short;
    * on the XL corpus every cell is seconds, so that budget would cut each one
    * to the 2-run minimum and the "median" would be an average of two. Three
    * runs is the smallest sample a median means anything for. */
  private def measureXL(run: () => Int): Cell =
    run()
    var samples = List.empty[Double]
    var matches = 0
    while samples.length < 3 do
      val t0 = now()
      matches = run()
      samples ::= now() - t0
    val sorted = samples.sorted
    Cell(sorted(1), sorted.head, sorted.length, matches)

  private def rssMb(): Double =
    js.Dynamic.global.process.memoryUsage().rss.asInstanceOf[Double] / 1024 / 1024

  private def heapMb(): Double =
    js.Dynamic.global.process.memoryUsage().heapUsed.asInstanceOf[Double] / 1024 / 1024

  /** Collect, if the runner exposed `gc` (`grepBenchXL` starts Node with
    * `--expose-gc`). Twice, because one pass leaves objects a finalizer or a
    * weak reference kept alive. */
  private def collected(): Boolean =
    if js.isUndefined(js.Dynamic.global.gc) then false
    else
      js.Dynamic.global.applyDynamic("gc")()
      js.Dynamic.global.applyDynamic("gc")()
      true

  /** The production marshalling shape: what `GrepEngineExports.grep` hands the
    * library — one JS object per matching line in a `js.Array`, which the
    * `GrepResult` then holds. `grepEngine` lives downstream of this project so
    * the three lines are repeated rather than called; keep them identical. */
  private def matchRows(ms: List[Match]): js.Array[js.Dynamic] =
    val out = js.Array[js.Dynamic]()
    ms.foreach(m => out.push(js.Dynamic.literal(path = m.path, line = m.line, text = m.text)))
    out

  /** What one XL-scale result set costs to *hold*, as opposed to how high the
    * heap floated while producing four of them. Both shapes are reported: the
    * engine's own `List[Match]`, and the `js.Array` of row objects the library
    * receives and a `GrepResult` keeps alive for as long as the agent holds it.
    * Read under a forced GC, so these are live-set numbers. */
  private def reportRetention(root: String, pattern: String): Unit =
    if !collected() then
      println("retained memory: unavailable (Node was started without --expose-gc)")
    else
      val base = heapMb()
      val ms = Grep.search(root, pattern)
      collected()
      val withList = heapMb()
      val rows = matchRows(ms)
      collected()
      val withRows = heapMb()
      println(f"retained heap for one $pattern result (${ms.length}%d matches): " +
        f"List[Match] ${withList - base}%+.0f MB, its js.Array of rows ${withRows - withList}%+.0f MB " +
        f"(live heap $base%.0f -> $withRows%.0f MB, ${rows.length}%d rows held)")

  /** The stage-6 decision gate: the standard rows on a monorepo-scale corpus.
    *
    * Never reached by `sbt grepBench` — `sbt grepBenchXL` is the only caller, so
    * the default bench neither generates the ~1.1 GB corpus nor pays its
    * multi-second rows. Every grep row is checked twice: against ripgrep, and
    * against the exact match counts [[XlCorpus]] planted, so a corpus that
    * generated wrong cannot quietly produce a plausible table. */
  def runXL(): Unit =
    val (stats, genMs) = timed(XlCorpus.generate())
    val gb = stats.bytes / 1024 / 1024 / 1024

    println()
    println("auk-grep XL benchmark — monorepo scale, single-threaded auk vs parallel rg")
    println(f"XL corpus: ${stats.files}%d files, $gb%.2f GB " +
      (if stats.cached then "(cached)" else f"(generated in ${genMs / 1000}%.0f s)"))
    println(s"  root: ${stats.root}")
    println(f"  ${stats.realFiles}%d searchable files across ${stats.dirs}%d dirs, " +
      f"max depth ${stats.maxDepth}%d; ${stats.junkFiles}%d gitignored junk files")
    println("  files by depth: " + stats.depthHist.zipWithIndex
      .collect { case (n, d) if n > 0 => s"$d:$n" }.mkString(" "))
    println(f"  planted matches: rare ${stats.rare}%d, common ${stats.common}%d, regex ${stats.regex}%d")
    println(f"  disk: delete ${stats.root} to reclaim $gb%.1f GB")

    val hasRg = rgAvailable()
    if !hasRg then println("NOTE: rg not found on PATH — benchmarking the auk engine only")

    println()
    println("=== XL corpus — the parallelism decision gate ===")
    println(pad("pattern", 38) + pad("engine", 9) + pad("median", 10) + pad("min", 10) +
      pad("runs", 6) + "matches")

    var peakRss = rssMb()
    def sampleRss(): Unit = peakRss = math.max(peakRss, rssMb())
    var wrong = List.empty[String]

    printRow("walk (list files)", "auk", measureXL(() => Walker.walk(stats.root).count(!_.dir)))
    sampleRss()
    if hasRg then printRow("", "rg", measureXL(() => rgFiles(stats.root, dirty = true)))

    val xlPatterns = List(
      ("rare literal", Needle, stats.rare),
      ("common word", "return", stats.common),
      ("regex", "handler_[0-9]+", stats.regex),
    )
    for (label, pattern, planted) <- xlPatterns do
      val ours = measureXL(() => Grep.search(stats.root, pattern).length)
      sampleRss()
      printRow(s"$label  ($pattern)", "auk", ours)
      if ours.matches != planted then
        wrong ::= s"auk $label: ${ours.matches} matches, corpus planted $planted"
      if hasRg then
        val rg = measureXL(() => rgGrep(stats.root, pattern, Nil, dirty = true))
        val rg1 = measureXL(() => rgGrep(stats.root, pattern, List("-j", "1"), dirty = true))
        val rgc = measureXL(() => rgCount(stats.root, pattern, dirty = true))
        printRow("", "rg", rg)
        printRow("", "rg -j1", rg1)
        printRow("", "rg -c", rgc)
        if rg.matches != ours.matches then
          wrong ::= s"$label: auk ${ours.matches} vs rg ${rg.matches}"
        if rgc.matches != ours.matches then
          wrong ::= s"$label: auk ${ours.matches} vs rg -c ${rgc.matches}"
        println(f"    auk / rg = ${ours.medianMs / rg.medianMs}%.1fx, " +
          f"auk / rg -j1 = ${ours.medianMs / rg1.medianMs}%.1fx, " +
          f"rg -j1 / rg = ${rg1.medianMs / rg.medianMs}%.1fx")

    // The result object's scale test: the same common-word search, but building
    // the js.Array of rows the library actually receives.
    val rows = measureXL(() => matchRows(Grep.search(stats.root, "return")).length)
    sampleRss()
    printRow("common word .rows  (js.Array)", "auk", rows)

    println()
    println(f"peak RSS across the auk rows: $peakRss%.0f MB (high-water with four runs' garbage in it)")
    reportRetention(stats.root, "return")
    if wrong.isEmpty then
      println("counts: exact — every row agrees with ripgrep and with the corpus's planted totals")
    else
      println("COUNT MISMATCH — the table above is not comparable:")
      wrong.reverse.foreach(m => println(s"  $m"))
      js.Dynamic.global.process.exitCode = 1
    println("auk is timed in-process; rg timings include ~5-10 ms of process start and, on the")
    println("high-volume rows, the cost of formatting and piping every matching line — the `rg -c`")
    println("row is the same search without that output, so the gap between them is output cost.")

  def main(args: Array[String]): Unit =
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
