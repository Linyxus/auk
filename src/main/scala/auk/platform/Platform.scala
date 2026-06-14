package auk.platform

import auk.platform.js.{NodeFileSystem, NodeProcess, NodeEnv, NodeUuid, GlobalProcess, nodeTmpdir, nodeToday}

/** The live platform services, wired to the Node/Bun implementations.
  *
  * The single place the rest of the app reaches for runtime capabilities, so the
  * JS-interop layer is swappable from here.
  */
object Platform:
  val fs: FileSystem = NodeFileSystem
  val process: Process = NodeProcess
  val env: Env = NodeEnv
  val uuid: Uuid = NodeUuid

  /** Process working directory (replaces `Paths.get("").toAbsolutePath`). */
  def cwd(): String = GlobalProcess.cwd()

  /** System temp directory (for tests/fixtures). */
  def tmpdir(): String = nodeTmpdir()

  /** Today's date as `YYYY-MM-DD`, for dating a session in the system prompt. */
  def today(): String = nodeToday()

  /** Terminate the process (replaces `scala.sys.exit`, unsupported on Scala.js). */
  def exit(code: Int): Nothing =
    GlobalProcess.exit(code)
    throw new RuntimeException("unreachable")
