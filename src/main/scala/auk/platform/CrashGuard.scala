package auk.platform

import scala.scalajs.js
import auk.platform.js.GlobalProcess

/** Captures otherwise-fatal runtime failures so an intermittent crash leaves a
  * forensic trail instead of a vanished process.
  *
  * Two purposes:
  *   - Mitigation: a stray async rejection — which Bun, unlike Node, can treat as
  *     fatal — no longer tears down an interactive session. Registering an
  *     `unhandledRejection` handler suppresses the runtime's default crash; we
  *     log it and carry on.
  *   - Diagnosis: whether these handlers fire at all is itself a signal. A
  *     JS-level error or rejection lands in [[LogPath]]. A NATIVE engine fault (a
  *     JavaScriptCore / Wasm crash under JSPI stack-switching) bypasses JS
  *     handlers entirely and leaves no entry — which points at the runtime, not
  *     our code, and is consistent with "fine on Node, crashes on Bun".
  */
object CrashGuard:
  val LogPath = ".auk/crash.log"

  def install(): Unit =
    // A rejection with no handler: record it, but do NOT exit — losing a turn is
    // far better than losing the session.
    GlobalProcess.on("unhandledRejection", (reason: js.Any) => append("unhandledRejection", reason))
    // A truly uncaught synchronous throw: state is unknown, so restore the
    // terminal and exit rather than soldier on with a corrupt UI.
    GlobalProcess.on(
      "uncaughtException",
      (err: js.Any) =>
        append("uncaughtException", err)
        restoreTerminal()
        System.err.nn.println(s"auk crashed (uncaughtException); details in $LogPath")
        GlobalProcess.exit(70) // EX_SOFTWARE
    )

  private def append(kind: String, value: js.Any): Unit =
    try
      val ts = new js.Date().toISOString()
      Platform.fs.createDirectories(".auk")
      Platform.fs.appendString(LogPath, s"$ts [$kind] ${describe(value)}\n")
    catch case _: Throwable => () // a crash handler must never crash

  /** The stack if the value carries one (an `Error`), else its JSON / string form. */
  private def describe(value: js.Any): String =
    val stack = value.asInstanceOf[js.Dynamic].selectDynamic("stack")
    if !js.isUndefined(stack) && stack != null then stack.toString
    else
      try js.JSON.stringify(value)
      catch case _: Throwable => String.valueOf(value)

  /** Best-effort: leave the terminal usable after a fatal error mid-render. Raw
    * escape strings (not `tui.render.Ansi`) so this crash path stays free of any
    * render-layer dependency. Order: mouse reporting off (SGR-1006 then ?1000),
    * kitty keyboard pop (`CSI <u`, harmless if never pushed), alt-screen exit,
    * show cursor, then a fresh line. */
  private def restoreTerminal(): Unit =
    try
      GlobalProcess.stdin.setRawMode(false)
      GlobalProcess.stdout.write("\u001b[?1006l\u001b[?1000l\u001b[<u\u001b[?1049l\u001b[?25h\r\n")
    catch case _: Throwable => ()
