package auk.library

import scala.scalajs.js

/** Shared base for the `auk.library` suites.
  *
  * These run as plain Scala.js under Node (the default jsEnv), so they exercise
  * the real implementation against a real file system and real child processes —
  * the same Node `fs`/`path`/`child_process` modules the production REPL preloads,
  * reached directly here instead of through a worker. The build's `Test /
  * jsEnvInput` prelude publishes Node's `require` on `globalThis`, which is what
  * [[Node]] (and the `os`/`process` lookups below) rely on.
  *
  * Every test that touches disk gets a fresh, isolated temp directory via the
  * [[tmp]] fixture — created before the test and removed (recursively) after — so
  * cases never see each other's files and a crash leaks at most one tmp dir.
  */
abstract class LibSuite extends munit.FunSuite:

  /** The interface under test. A fresh instance per suite is fine: `AukImpl`
    * holds no mutable state, all I/O goes straight to Node. */
  protected val lib: AukInterface = new AukImpl

  private def osTmpdir: String =
    js.Dynamic.global.require("node:os").tmpdir().asInstanceOf[String]

  private var counter = 0
  private def freshName(): String =
    counter += 1
    s"auk-libtest-${System.nanoTime()}-$counter"

  /** A unique, already-created temp directory, removed after the test. */
  val tmp: FunFixture[FsDir] = FunFixture[FsDir](
    setup = _ =>
      val d = (lib.path(osTmpdir) / freshName()).openAsDir
      d.makedir()
      d,
    teardown = d =>
      try d.delete()
      catch case _: Throwable => ()
  )

  /** Capture everything printed to `Console.out` while running `body`. */
  protected def captured(body: => Unit): String =
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out)(body)
    out.toString

  /** Assert that `body` throws a `RuntimeException` whose message contains
    * `substr` (case-insensitive), and return the exception. */
  protected def interceptContains(substr: String)(body: => Any): RuntimeException =
    val ex = intercept[RuntimeException](body)
    val msg = Option(ex.getMessage).getOrElse("")
    assert(
      msg.toLowerCase.contains(substr.toLowerCase),
      s"expected an exception message containing '$substr', got: '$msg'"
    )
    ex

  // -- Node helpers for cases the public API can't set up directly -----------

  private def nodeFs: js.Dynamic = js.Dynamic.global.require("node:fs")

  /** Create a symlink at `link` pointing at `target` (verbatim, not resolved). */
  protected def symlink(target: Path, link: Path): Unit =
    nodeFs.symlinkSync(target.toString, link.toString)

  /** Write raw bytes to `p` (e.g. binary content with NUL bytes), bypassing the
    * library's UTF-8 text path. */
  protected def writeBytes(p: Path, bytes: Array[Byte]): Unit =
    val buf = js.Dynamic.global.Buffer.from(js.Array(bytes.map(b => (b & 0xff).toInt)*))
    nodeFs.writeFileSync(p.toString, buf)

  /** Set both access and modification times of `p` to `ms` epoch-milliseconds,
    * so `lastModified*` assertions are deterministic. */
  protected def setMtime(p: Path, ms: Double): Unit =
    val secs = ms / 1000.0
    nodeFs.utimesSync(p.toString, secs, secs)
