package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js

import gears.async.Async
import gears.async.default.given

import auk.platform.js.ReplArtifacts
import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** `sbt grepBench` in production shape: drive the SAME corpora and patterns as
  * [[auk.grep.bench.Bench]] through the real REPL worker, so the numbers reflect
  * how grep actually runs for the agent — the packed `library.bin` executed by
  * the sjsir INTERPRETER — rather than the linked fastLinkJS engine grepBench
  * measures. The rows here are therefore comparable row-by-row with `sbt
  * grepBench`, and are expected to be markedly slower.
  *
  * Opt-in only (three `assume` gates below), so plain `sbt test` never runs it:
  * run with `env AUK_INTERP_BENCH=1 sbt "testOnly auk.runtime.GrepInterpBenchSuite"`.
  *
  * Modeled on [[FsLibrarySuite]]: one shared [[ScalaRepl]], `Async.fromSync`
  * around each eval, a `completed` helper, the same `ReplArtifacts.resolve()`
  * availability check, and `repl.close()` in [[afterAll]].
  */
class GrepInterpBenchSuite extends munit.FunSuite:

  // Interpreted grep over a 24-52 MB corpus can be brutally slow; give munit and
  // each eval room so a slow-but-live run is not mistaken for a hang.
  override def munitTimeout: Duration = 30.minutes
  private val CellTimeoutMs = 900_000

  // -- host-side corpus discovery (node fs/os/path) ---------------------------

  private def nodeFs = js.Dynamic.global.require("node:fs")
  private def nodeOs = js.Dynamic.global.require("node:os")
  private def nodePath = js.Dynamic.global.require("node:path")
  private def join(a: String, b: String): String = nodePath.join(a, b).asInstanceOf[String]

  // Keep CorpusTag in sync with `Bench.CorpusTag` in
  // grep/src/main/scala/auk/grep/bench/Bench.scala — bump both together.
  private val CorpusTag = "v1"
  private def tmpdir: String = nodeOs.tmpdir().asInstanceOf[String]
  private def cleanRoot: String = join(tmpdir, s"auk-grep-bench-$CorpusTag")
  private def dirtyRoot: String = join(tmpdir, s"auk-grep-bench-dirty-$CorpusTag")

  /** A corpus is present iff its `MANIFEST` (written last during generation) exists. */
  private def corpusPresent(root: String): Boolean =
    nodeFs.existsSync(join(root, "MANIFEST")).asInstanceOf[Boolean]

  /** `MANIFEST` holds `"files bytes"` (see `Bench.cleanCorpus` / `dirtyCorpus`). */
  private def manifest(root: String): (Int, Double) =
    val parts = nodeFs.readFileSync(join(root, "MANIFEST"), "utf8").asInstanceOf[String].trim.split(' ')
    (parts(0).toInt, parts(1).toDouble)

  private def benchEnabled: Boolean =
    !js.isUndefined(js.Dynamic.global.process.env.AUK_INTERP_BENCH)

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight
  private val repl = ScalaRepl()

  override def afterAll(): Unit =
    Async.fromSync(repl.close())

  private def completed(code: String, timeoutMs: Int)(using Async): ReplProtocol.Response =
    repl.eval(code, Some(timeoutMs)).status match
      case ScalaRepl.Status.Completed(r) => r
      case other                         => fail(s"unexpected REPL status: $other")

  // -- assume gates: env opt-in, then artifacts, then corpus (in this order) --

  private def gate(root: String, corpusLabel: String): Unit =
    assume(
      benchEnabled,
      "interp bench is opt-in; run with " +
        "`env AUK_INTERP_BENCH=1 sbt \"testOnly auk.runtime.GrepInterpBenchSuite\"`"
    )
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl packLibraryBin`")
    assume(corpusPresent(root), s"$corpusLabel corpus ($root) missing; run `sbt grepBench` once to generate the corpora")

  // -- measurement ------------------------------------------------------------

  private final case class Cell(medianMs: Long, minMs: Long, runs: Int, matches: Int)

  private val AukBenchRe = raw"AUKBENCH (\d+) (\d+) (\d+) (\d+)".r

  /** Run ONE self-contained bench cell in the worker and parse its marker.
    *
    * Timing lives INSIDE the snippet — host wall time would fold in the worker
    * compiling the snippet, which we must exclude. No session state is carried
    * between cells, so a timed-out cell (which kills the worker) cannot poison
    * later cells: the next eval boots a fresh worker. One untimed warmup, then
    * up to 3 samples within a 10 s in-snippet budget (min 1 — an interpreted run
    * may be very slow, so this deliberately relaxes grepBench's min-2). The
    * snippet renders a short marker string (the REPL truncates long rendered
    * values); the host recovers `AUKBENCH <median> <min> <runs> <matches>` from
    * `r.output`.
    */
  private def runCell(root: String, body: String)(using Async): Cell =
    val snippet =
      s"""{
         |  val d = lib.fs.accessDir(lib.path("$root"))
         |  def run(): Int = $body
         |  val _ = run()
         |  var samples = List.empty[Double]
         |  var matches = 0
         |  val t0 = System.nanoTime
         |  while samples.length < 3 && (samples.isEmpty || (System.nanoTime - t0) / 1e6 < 10000.0) do
         |    val s = System.nanoTime
         |    matches = run()
         |    samples ::= (System.nanoTime - s) / 1e6
         |  val sorted = samples.sorted
         |  "AUKBENCH " + sorted(sorted.length / 2).round + " " + sorted.head.round + " " + sorted.length + " " + matches
         |}""".stripMargin
    val r = completed(snippet, CellTimeoutMs)
    assert(r.ok, s"cell eval errored (body: $body):\n${r.error.getOrElse(r.output)}\n${r.stderr}")
    val out = ReplProtocol.stripAnsi(r.output)
    AukBenchRe.findFirstMatchIn(out) match
      case Some(m) => Cell(m.group(1).nn.toLong, m.group(2).nn.toLong, m.group(3).nn.toInt, m.group(4).nn.toInt)
      case None    => fail(s"no AUKBENCH marker in cell output (body: $body): $out")

  // -- table output (mirrors grepBench's columns; engine col widened for the
  //    longer `auk-interp` label so the table stays aligned) -----------------

  private val LabelW = 38
  private val EngineW = 11
  private val NumW = 10
  private val RunsW = 6

  private def pad(s: String, w: Int): String = s + " " * math.max(0, w - s.length)
  private def fmt(ms: Long): String = s"$ms ms"

  private def header(): Unit =
    println(pad("pattern", LabelW) + pad("engine", EngineW) + pad("median", NumW) +
      pad("min", NumW) + pad("runs", RunsW) + "matches")

  private def printRow(label: String, c: Cell): Unit =
    println(pad(label, LabelW) + pad("auk-interp", EngineW) + pad(fmt(c.medianMs), NumW) +
      pad(fmt(c.minMs), NumW) + pad(c.runs.toString, RunsW) + c.matches.toString)

  // The three grep patterns, same as grepBench, with their tag-v1 match counts.
  // These pins are the counts grepBench prints (rg-cross-checked there); update
  // them whenever CorpusTag bumps.
  private val cleanPins = List(
    ("rare literal", "aukGrepBenchNeedle", 12),
    ("common word", "return", 110186),
    ("regex", "handler_[0-9]+", 2566),
  )
  private val dirtyPins = List(
    ("rare literal", "aukGrepBenchNeedle", 6),
    ("common word", "return", 18207),
    ("regex", "handler_[0-9]+", 424),
  )

  private def section(title: String, root: String, corpusLabel: String, pins: List[(String, String, Int)])(using Async): Unit =
    val (files, bytes) = manifest(root)
    println()
    println(title)
    println(f"$corpusLabel corpus: $files files, ${bytes / 1024 / 1024}%.1f MB")
    println(s"  root: $root")
    header()

    // walk row: count files without extra syscalls. `walk` already maps engine
    // children to FsFile/FsDir handles by kind, so `isInstanceOf[FsFile]` is a
    // pure type test — `isFile`/`!_.isDir` would statSync per entry. No parity
    // assertion: walk semantics differ from `rg --files`; the linked grepBench
    // walk count is the comparison point (clean 1205, dirty 362).
    printRow("walk (list files)", runCell(root, "d.walk.count(_.isInstanceOf[FsFile])"))

    for (label, pattern, expected) <- pins do
      val c = runCell(root, s"""d.grep("$pattern").length""")
      printRow(s"$label  ($pattern)", c)
      assertEquals(c.matches, expected, s"$corpusLabel $label match count drift (tag $CorpusTag)")

  // -- tests (declaration order: boot, clean, dirty; one shared worker) --------

  test("worker boot"):
    gate(cleanRoot, "clean")
    Async.fromSync:
      // Host-side wall time of the very first eval on the fresh worker: it
      // includes worker spawn + preamble compile, a real one-time production
      // cost worth seeing (measured host-side on purpose, unlike the cells).
      val t0 = System.nanoTime
      val r = completed("1 + 1", CellTimeoutMs)
      val bootMs = (System.nanoTime - t0) / 1e6
      assert(r.ok, s"boot eval errored: ${r.error.getOrElse(r.output)}")
      println()
      println("auk-grep interpreter-mode benchmark (production REPL worker, sjsir interpreter)")
      println(f"worker boot + preamble (one-time cost): $bootMs%.0f ms")

  test("clean corpus"):
    gate(cleanRoot, "clean")
    Async.fromSync:
      section("=== clean corpus — engine speed; no ignore files ===", cleanRoot, "clean", cleanPins)

  test("dirty corpus"):
    gate(dirtyRoot, "dirty")
    Async.fromSync:
      section(
        "=== dirty corpus — work avoidance; junk is gitignored, needles only in real files ===",
        dirtyRoot, "dirty", dirtyPins
      )
      println()
      println("rows above are `auk-interp` (production sjsir-interpreter in the REPL worker); compare")
      println("row-by-row against `sbt grepBench` (linked fastLinkJS engine) on the same corpora.")
