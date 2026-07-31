package auk.library

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The auk-grep engine as a native JS module — the search path `FsDir` and
 *  `FsFile` route through.
 *
 *  The engine's Scala source lives in the `grep` subproject and is compiled
 *  twice: once to IR (packed into `library.bin` like the rest of the library)
 *  and once, by the `grepEngine` project, to a linked single-file CommonJS
 *  bundle carried in the same archive as `js-modules/aukGrepEngine.js`. The REPL
 *  worker executes that bundle at boot and publishes its exports at
 *  `globalThis.__replJSModules.aukGrepEngine`; this facade's `globalFallback`
 *  resolves exactly there, so a search costs one JS-interop call plus the
 *  conversion of its results, instead of running the whole scan through the
 *  sjsir interpreter.
 *
 *  The module name, the archive key and the last segment of the global fallback
 *  must all stay `aukGrepEngine`. There is no in-IR fallback on purpose: if the
 *  bundle is missing, the registry raises its own named error rather than
 *  silently running a second copy of the engine at interpreted speed.
 */
@js.native
@JSImport("aukGrepEngine", JSImport.Namespace, globalFallback = "__replJSModules.aukGrepEngine")
private[library] object GrepEngine extends js.Object:
  /** Rows of `{path, dir}`, in traversal order. */
  def glob(root: String, pattern: String): js.Array[js.Dynamic] = js.native
  def globAll(root: String, pattern: String): js.Array[js.Dynamic] = js.native

  /** Rows of `{path, line, text}`, in traversal order, a file's matches
   *  adjacent. */
  def grep(root: String, pattern: String): js.Array[js.Dynamic] = js.native
  def grepGlob(root: String, pattern: String, filePattern: String): js.Array[js.Dynamic] = js.native
  def grepAll(root: String, pattern: String): js.Array[js.Dynamic] = js.native
  def grepAllGlob(root: String, pattern: String, filePattern: String): js.Array[js.Dynamic] = js.native
  def grepFile(path: String, pattern: String): js.Array[js.Dynamic] = js.native
