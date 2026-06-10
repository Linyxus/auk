package auk.platform.js

import scala.scalajs.js

/** Buffers stream chunks and yields complete `\n`-terminated lines (a trailing
  * `\r` is stripped); the unterminated tail is held until more input arrives.
  */
final class LineSplitter:
  private var buf = ""

  def feed(chunk: String): List[String] =
    buf += chunk
    val lines = List.newBuilder[String]
    var idx = buf.indexOf('\n')
    while idx >= 0 do
      lines += buf.substring(0, idx).nn.stripSuffix("\r")
      buf = buf.substring(idx + 1).nn
      idx = buf.indexOf('\n')
    lines.result()

  /** The buffered, not-yet-terminated remainder. */
  def pending: String = buf

/** A long-lived child process exchanging newline-framed text over stdio.
  *
  * The run-to-completion sibling is [[NodeProcess]]; this one stays up across
  * many exchanges (auk's REPL worker), so instead of one captured result it
  * surfaces stdout line by line and reports exit exactly once — for a spawn
  * failure too, in which case the diagnostic goes to `onStderr` first. All
  * callbacks fire on the JS event loop; bridge them into gears with
  * `Future.Promise`.
  */
object LineProcess:
  trait Handle:
    /** Queue `line` (a newline is appended) on the child's stdin. False means
      * the pipe was already closed and nothing was sent. */
    def writeLine(line: String): Boolean
    def kill(): Unit
    def alive: Boolean

  def spawn(
      argv: List[String],
      extraEnv: Map[String, String],
      onLine: String => Unit,
      onStderr: String => Unit,
      onExit: Int => Unit
  ): Handle =
    // The child sees the parent's environment plus the overrides.
    val env = js.Object.assign(
      js.Dynamic.literal().asInstanceOf[js.Object],
      GlobalProcess.env.asInstanceOf[js.Object],
      js.Dictionary(extraEnv.toSeq*).asInstanceOf[js.Object]
    )
    val opts = js.Dynamic.literal(
      stdio = js.Array[String]("pipe", "pipe", "pipe"),
      env = env
    )
    val child =
      NodeChildProcess.spawn(argv.head, js.Array(argv.tail*), opts.asInstanceOf[js.Object])

    val lines = new LineSplitter
    child.stdout.setEncoding("utf8")
    child.stderr.setEncoding("utf8")
    child.stdout.on("data", chunk => lines.feed(chunk.asInstanceOf[String]).foreach(onLine))
    child.stderr.on("data", chunk => onStderr(chunk.asInstanceOf[String]))

    var exited = false
    def exit(code: Int): Unit =
      if !exited then
        exited = true
        onExit(code)

    // `error` fires for spawn failures (possibly with no `close` to follow);
    // `close` for everything else. Either way the handle dies exactly once.
    child.on("error", (e, _) => { onStderr(s"failed to start worker: $e\n"); exit(-1) })
    child.on(
      "close",
      (code, _) => exit(if code != null then code.asInstanceOf[Int] else -1)
    )

    new Handle:
      def writeLine(line: String): Boolean =
        !exited && (
          try { child.stdin.write(line + "\n"); true }
          catch case _: Throwable => false
        )
      def kill(): Unit = if !exited then { child.kill("SIGKILL"); () }
      def alive: Boolean = !exited
