package auk.grep.bench

/** `sbt grepBenchXL`: the stage-6 decision gate, [[Bench.runXL]].
  *
  * A project of its own for one reason: Scala.js links exactly one main module
  * initializer per project, and sbt's Scala.js `run` takes no arguments at all
  * (`grep/run --xl` is rejected by the command parser, not ignored), so an
  * opt-in bench tier cannot be a flag on `grepBench` — it has to be a second
  * entry point. Everything it runs lives in `grep/`: this is the entry point and
  * nothing else, which is also what keeps `sbt grepBench` byte-identical.
  */
object BenchXL:
  def main(args: Array[String]): Unit = Bench.runXL()
