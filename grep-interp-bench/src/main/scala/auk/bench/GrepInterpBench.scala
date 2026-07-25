package auk.bench

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.util.{Failure, Success}

import gears.async.Async
import gears.async.default.given

import auk.grep.bench.Bench
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** `sbt grepBench` in production shape: drive the SAME corpora and patterns as
  * [[auk.grep.bench.Bench]] through the real REPL worker, so the numbers reflect
  * how grep actually runs for the agent — the packed `library.bin` executed by
  * the sjsir INTERPRETER — rather than the linked fastLinkJS engine grepBench
  * measures. The rows here are therefore comparable row-by-row with `sbt
  * grepBench`, and are expected to be markedly slower.
  *
  * Run it with `sbt grepInterpBench` (or `sbt grepInterpBench/run`, what that
  * aliases).
  * The corpora are grepBench's own, cached in the OS tmpdir and generated here
  * through grepBench's generators when absent — so either command can be the
  * one that runs first. What this does NOT rebuild is `vendor/repl/library.bin`:
  * run `sbt packLibraryBin` after touching `library/` or `grep/`, or the rows
  * below measure the previously packed engine.
  *
  * Every failure is fatal — missing REPL artifacts, an errored eval, a match
  * count that drifted from its pin — so the command exits nonzero rather than
  * printing a table nobody can trust.
  */
object GrepInterpBench:

  // Interpreted grep over a 24-52 MB corpus can be brutally slow; give each eval
  // room so a slow-but-live run is not mistaken for a hang.
  private val CellTimeoutMs = 900_000

  private val repl = ScalaRepl()

  private def completed(code: String)(using Async): ReplProtocol.Response =
    repl.eval(code, Some(CellTimeoutMs)).status match
      case ScalaRepl.Status.Completed(r) => r
      case other                         => sys.error(s"unexpected REPL status: $other")

  // -- measurement ------------------------------------------------------------

  private final case class Corpus(root: String, files: Int, bytes: Double)

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
    val r = completed(snippet)
    if !r.ok then
      sys.error(s"cell eval errored (body: $body):\n${r.error.getOrElse(r.output)}\n${r.stderr}")
    val out = ReplProtocol.stripAnsi(r.output)
    AukBenchRe.findFirstMatchIn(out) match
      case Some(m) => Cell(m.group(1).nn.toLong, m.group(2).nn.toLong, m.group(3).nn.toInt, m.group(4).nn.toInt)
      case None    => sys.error(s"no AUKBENCH marker in cell output (body: $body): $out")

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
  // them whenever Bench.CorpusTag bumps. Rows count through `GrepResult.length`,
  // which reads the engine's JS array directly — the search, and nothing else.
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

  // Rows measured through `GrepResult.matches` instead: the `List[Match]`
  // materialization an agent pays only when it asks for the handles. Kept on the
  // clean common-word cell because that is where it dominates — 110186 Match
  // allocations, historically the whole reason that row missed its stage-4.5
  // target — so the cost stays tracked now that `.length` no longer pays it.
  private val cleanMaterializePins = List(
    ("common word .matches", "return", 110186),
  )

  private def section(title: String, corpus: Corpus, corpusLabel: String,
      pins: List[(String, String, Int)],
      materializePins: List[(String, String, Int)] = Nil)(using Async): Unit =
    println()
    println(title)
    println(f"$corpusLabel corpus: ${corpus.files} files, ${corpus.bytes / 1024 / 1024}%.1f MB")
    println(s"  root: ${corpus.root}")
    header()

    // walk row: count files without extra syscalls. `walk` already maps engine
    // children to FsFile/FsDir handles by kind, so `isInstanceOf[FsFile]` is a
    // pure type test — `isFile`/`!_.isDir` would statSync per entry. No parity
    // assertion: walk semantics differ from `rg --files`; the linked grepBench
    // walk count is the comparison point (clean 1205, dirty 362).
    printRow("walk (list files)", runCell(corpus.root, "d.walk.count(_.isInstanceOf[FsFile])"))

    def pinnedRow(label: String, pattern: String, body: String, expected: Int): Unit =
      val c = runCell(corpus.root, body)
      printRow(s"$label  ($pattern)", c)
      if c.matches != expected then
        sys.error(s"$corpusLabel $label match count drift (tag ${Bench.CorpusTag}): " +
          s"expected $expected, got ${c.matches}")

    for (label, pattern, expected) <- pins do
      pinnedRow(label, pattern, s"""d.grep("$pattern").length""", expected)
    for (label, pattern, expected) <- materializePins do
      pinnedRow(label, pattern, s"""d.grep("$pattern").matches.length""", expected)

  // -- setup ------------------------------------------------------------------

  /** Publish Node's `require` on `globalThis`.
    *
    * This project links as an ES module (root's Wasm config), where `require` is
    * not in scope — but [[auk.grep.bench.Bench]] reaches the Node built-ins it
    * generates the corpora with through `js.Dynamic.global.require`, exactly as
    * the REPL worker bootstrap arranges in production. `createRequire` only
    * needs a real directory to anchor relative resolution; ours resolves nothing
    * but `node:*` built-ins.
    */
  private def installGlobalRequire(): Unit =
    val cwd = js.Dynamic.global.process.cwd().asInstanceOf[String]
    js.Dynamic.global.globalThis.updateDynamic("require")(
      NodeModule.createRequire(cwd + "/grep-interp-bench.js")
    )

  /** Generate the corpus if it is not already cached, reporting the wait. */
  private def ensure(label: String, gen: () => (String, Int, Double, Boolean)): Corpus =
    val t0 = System.nanoTime
    val (root, files, bytes, cached) = gen()
    if !cached then
      println(f"generated the $label corpus in ${(System.nanoTime - t0) / 1e9}%.1f s: $root")
    Corpus(root, files, bytes)

  /** Report a fatal failure and mark the run failed.
    *
    * A throwable that reaches the Wasm module initializer surfaces as
    * `[Object: null prototype] {}` and nothing else, so every failure path here
    * diagnoses itself rather than propagating. Marking the process through
    * `exitCode` rather than calling `process.exit` lets the writes flush first.
    */
  private def fatal(message: String): Unit =
    println()
    println(s"grep interp bench FAILED: $message")
    js.Dynamic.global.process.exitCode = 1

  private def describe(e: Throwable): String =
    val m = e.getMessage
    if m == null then e.toString else m

  def main(args: Array[String]): Unit =
    installGlobalRequire()
    try
      ReplArtifacts.resolve() match
        case Left(err) =>
          fatal(s"$err\nRun `sbt vendorRepl packLibraryBin` before benchmarking.")
        case Right(_) => bench()
    catch case e: Throwable => fatal(describe(e))

  private def bench(): Unit =
    val clean = ensure("clean", () => Bench.cleanCorpus())
    val dirty = ensure("dirty", () => Bench.dirtyCorpus())

    // `Async.fromSync` hands the outcome back as a Future and returns long
    // before the body has finished, so a throwable that escapes it is simply
    // dropped: the process would exit 0 having printed half a table. Catch
    // inside, and report whatever the Future settles on (below).
    val outcome = Async.fromSync:
      try
        // Host-side wall time of the very first eval on the fresh worker: it
        // includes worker spawn + preamble compile, a real one-time production
        // cost worth seeing (measured host-side on purpose, unlike the cells).
        val t0 = System.nanoTime
        val r = completed("1 + 1")
        val bootMs = (System.nanoTime - t0) / 1e6
        if !r.ok then sys.error(s"boot eval errored: ${r.error.getOrElse(r.output)}")
        println()
        println("auk-grep interpreter-mode benchmark (production REPL worker, sjsir interpreter)")
        println(f"worker boot + preamble (one-time cost): $bootMs%.0f ms")

        section("=== clean corpus — engine speed; no ignore files ===", clean, "clean", cleanPins,
          cleanMaterializePins)
        section(
          "=== dirty corpus — work avoidance; junk is gitignored, needles only in real files ===",
          dirty, "dirty", dirtyPins
        )

        println()
        println("rows above are `auk-interp` (production sjsir-interpreter in the REPL worker); compare")
        println("row-by-row against `sbt grepBench` (linked fastLinkJS engine) on the same corpora.")
        println("grep rows count through `GrepResult.length` — the search alone. The `.matches` row is")
        println("the same search plus the List[Match] materialization an agent pays only when it asks")
        println("for the handles; the gap between the two is that allocation.")
        None
      catch case e: Throwable => Some(e)
      finally repl.close()

    outcome.onComplete: settled =>
      val failure = settled match
        case Success(f) => f
        case Failure(e) => Some(e)
      failure.foreach(e => fatal(describe(e)))

@js.native
@JSImport("node:module", JSImport.Namespace)
private object NodeModule extends js.Object:
  def createRequire(filename: String): js.Dynamic = js.native
