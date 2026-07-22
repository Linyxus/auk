package auk.platform.js

import scala.scalajs.js

/** Once the TUI owns the terminal, nothing else may write to it: a stray
  * `console.log` from a bundled dependency, an SDK retry warning, or a Node
  * runtime warning (they print to stderr asynchronously) would land between
  * renderer frames and smear the screen — the cell diff then patches a grid
  * the terminal no longer shows, and the smear persists until a full repaint.
  *
  * [[install]] claims the tty for the renderer alone: the whole `console` is
  * replaced with one writing to `logPath`, and the stderr stream's `write` is
  * redirected there too (Node's warning printer and anything else that writes
  * to `process.stderr` directly rides that). `process.stdout.write` is left
  * untouched — it is the renderer's own channel. Best-effort and idempotent;
  * only ever called for a real TTY session (headless/test runs keep their
  * consoles). */
object TtyGuard:
  private var installed = false

  def install(logPath: String): Unit =
    if installed then return
    installed = true
    try
      val fs = js.Dynamic.global.require("node:fs")
      try
        fs.mkdirSync(logPath.split('/').dropRight(1).mkString("/"), js.Dynamic.literal(recursive = true))
      catch case _: Throwable => ()
      // All console.* (log/info/warn/error/debug) → the log file, via a real
      // Console bound to an append stream — formatting semantics preserved.
      val stream = fs.createWriteStream(logPath, js.Dynamic.literal(flags = "a"))
      val consoleMod = js.Dynamic.global.require("node:console")
      js.Dynamic.global.globalThis.updateDynamic("console")(js.Dynamic.newInstance(consoleMod.Console)(stream, stream))
      // Direct stderr writers (incl. Node's own warning printer). The write
      // contract allows a trailing callback in either optional slot; honor it
      // so a caller awaiting the flush never hangs.
      val handler: js.Function3[js.Any, js.Any, js.Any, Boolean] = (chunk, a, b) =>
        try { fs.appendFileSync(logPath, chunk); () }
        catch case _: Throwable => ()
        if js.typeOf(a) == "function" then a.asInstanceOf[js.Function0[Unit]]()
        else if js.typeOf(b) == "function" then b.asInstanceOf[js.Function0[Unit]]()
        true
      js.Dynamic.global.process.stderr.updateDynamic("write")(handler)
      ()
    catch case _: Throwable => () // a guard must never take the session down
