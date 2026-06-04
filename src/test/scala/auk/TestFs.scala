package auk

import auk.platform.{Platform, PathOps}

/** Filesystem helpers for tests, replacing `java.nio.file` fixtures. Backed by
  * the Node/Bun [[Platform.fs]] against the system temp dir. */
object TestFs:
  private var counter = 0

  /** A fresh, unique temp directory (created), returned as a path string. */
  def tempDir(prefix: String): String =
    counter += 1
    val dir = PathOps.join(Platform.tmpdir(), s"$prefix-${System.currentTimeMillis()}-$counter")
    Platform.fs.createDirectories(dir)
    dir

  def write(path: String, content: String): Unit =
    PathOps.parent(path).foreach(Platform.fs.createDirectories)
    Platform.fs.writeString(path, content)

  def append(path: String, content: String): Unit =
    PathOps.parent(path).foreach(Platform.fs.createDirectories)
    Platform.fs.appendString(path, content)

  def read(path: String): String = Platform.fs.readString(path)
  def exists(path: String): Boolean = Platform.fs.exists(path)
  def mkdir(path: String): Unit = Platform.fs.createDirectories(path)
  def join(dir: String, name: String): String = PathOps.join(dir, name)
