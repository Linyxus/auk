package auk.bench

/** `./mill grepInterpBenchXL`: [[GrepInterpBench]] on the stage-6 XL corpus.
  *
  * Its own project for the same reason `grepBenchXL` is — Scala.js links one
  * main module initializer per project and a Scala.js `run` takes no arguments — and
  * a separate command because the XL corpus is ~1.1 GB and its rows are seconds
  * each, which does not belong in the run you do after every change.
  */
object GrepInterpBenchXL:
  def main(args: Array[String]): Unit = GrepInterpBench.runXL()
