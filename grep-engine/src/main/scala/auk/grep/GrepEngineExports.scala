package auk.grep

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** The engine's JS entry points: the `aukGrepEngine` native companion module.
 *
 *  This is the whole of the `grepEngine` project — the engine itself lives in
 *  `grep/`, unchanged. Linked as a single-file CommonJS bundle, this surface is
 *  packed into `library.bin` as `js-modules/aukGrepEngine.js`, and the REPL
 *  worker publishes it at `globalThis.__replJSModules.aukGrepEngine` before
 *  loading any IR. `auk.library.GrepEngine` is the typed facade that resolves
 *  there, so the library's `glob`/`grep` run as linked JS instead of paying
 *  the sjsir interpreter's per-match tax.
 *
 *  Everything crossing the boundary is plain JS: strings in, arrays of
 *  `{path, dir}` / `{path, line, text}` objects out. JS has no overloading, so
 *  the two `search` arities get distinct names.
 */
object GrepEngineExports:

  @JSExportTopLevel("glob")
  def glob(root: String, pattern: String): js.Array[js.Dynamic] =
    crossing(entryRows(Walker.glob(root, pattern)))

  @JSExportTopLevel("globAll")
  def globAll(root: String, pattern: String): js.Array[js.Dynamic] =
    crossing(entryRows(Walker.globAll(root, pattern)))

  @JSExportTopLevel("grep")
  def grep(root: String, pattern: String): js.Array[js.Dynamic] =
    crossing(matchRows(Grep.search(root, pattern)))

  @JSExportTopLevel("grepGlob")
  def grepGlob(root: String, pattern: String, filePattern: String): js.Array[js.Dynamic] =
    crossing(matchRows(Grep.search(root, pattern, filePattern)))

  @JSExportTopLevel("grepAll")
  def grepAll(root: String, pattern: String): js.Array[js.Dynamic] =
    crossing(matchRows(Grep.searchAll(root, pattern)))

  @JSExportTopLevel("grepAllGlob")
  def grepAllGlob(root: String, pattern: String, filePattern: String): js.Array[js.Dynamic] =
    crossing(matchRows(Grep.searchAll(root, pattern, filePattern)))

  @JSExportTopLevel("grepFile")
  def grepFile(path: String, pattern: String): js.Array[js.Dynamic] =
    crossing(matchRows(Grep.searchFile(path, pattern)))

  private def entryRows(es: List[Entry]): js.Array[js.Dynamic] =
    val out = js.Array[js.Dynamic]()
    es.foreach(e => out.push(js.Dynamic.literal(path = e.path, dir = e.dir)))
    out

  private def matchRows(ms: List[Match]): js.Array[js.Dynamic] =
    val out = js.Array[js.Dynamic]()
    ms.foreach(m => out.push(js.Dynamic.literal(path = m.path, line = m.line, text = m.text)))
    out

  /** Run one exported operation, making sure its failure crosses the boundary as
   *  a JS error carrying the engine's own message. The caller is a different
   *  Scala.js world (interpreted IR, or a separately linked test build), so a
   *  Scala `Throwable` from this bundle would arrive there as an opaque foreign
   *  object; the library's error contract — `grep: invalid regular expression
   *  '…': …`, pinned by GrepSuite and the library suites — is the message, so
   *  the message is what is preserved. A raw JS error (a Node `ENOENT`, say)
   *  already crosses cleanly and is rethrown untouched, keeping its `code`.
   */
  private def crossing[A](body: => A): A =
    try body
    catch
      case e: js.JavaScriptException => throw e
      case t: Throwable =>
        throw js.JavaScriptException(new js.Error(Option(t.getMessage).getOrElse(t.toString)))
