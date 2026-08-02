package auk.platform

/** One entry of a directory listing. */
final case class DirEntry(name: String, isFile: Boolean, mtimeMs: Long)

/** Synchronous filesystem seam, replacing `java.nio.file` (absent on Scala.js).
  *
  * Operations are synchronous: the single-threaded JS runtime can block on
  * Node's `*Sync` fs calls without JSPI, which matches the (synchronous) call
  * sites in the tools and session layer. Failures are surfaced as
  * `java.io.IOException` so existing `catch case e: IOException` clauses keep
  * working.
  */
trait FileSystem:
  def exists(path: String): Boolean
  def isDirectory(path: String): Boolean
  def isRegularFile(path: String): Boolean
  def size(path: String): Long
  def readString(path: String): String
  def writeString(path: String, content: String): Unit
  def appendString(path: String, content: String): Unit
  def createDirectories(path: String): Unit

  /** Set `path`'s permission bits (POSIX numeric, e.g. `384` = `0600`). */
  def chmod(path: String, mode: Int): Unit

  /** Directory entries, or empty if `path` is absent or not a directory. */
  def listDir(path: String): List[DirEntry]

  /** Remove `path` — a file or a directory tree — if it exists. */
  def removeAll(path: String): Unit
