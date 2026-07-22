package auk.platform.js

import scala.scalajs.js

/** Best-effort "open this URL in the default browser": a detached,
  * fire-and-forget spawn of the OS opener. Failures (no opener, headless box)
  * are swallowed — opening a dashboard is never worth crashing over.
  */
private[platform] object NodeBrowser:
  def open(url: String): Unit =
    val argv = GlobalProcess.platform match
      case "darwin" => List("open", url)
      // `start` is a cmd builtin; the empty "" is its window-title slot, so the
      // URL is not mistaken for one.
      case "win32" => List("cmd", "/c", "start", "", url)
      case _       => List("xdg-open", url)
    val opts = js.Dynamic.literal(detached = true, stdio = "ignore")
    val child = NodeChildProcess.spawn(argv.head, js.Array(argv.tail*), opts.asInstanceOf[js.Object])
    child.on("error", (_, _) => ())
    child.unref()
