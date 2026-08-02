package auk.platform.js

import java.io.IOException
import scala.scalajs.js
import scala.util.control.NonFatal

import auk.platform.{FileSystem, DirEntry}

/** [[FileSystem]] backed by Node/Bun synchronous `fs` calls. */
object NodeFileSystem extends FileSystem:
  private def io[T](what: String)(body: => T): T =
    try body
    catch case NonFatal(e) => throw IOException(s"$what: ${jsMessage(e)}")

  private def jsMessage(e: Throwable): String = e match
    case js.JavaScriptException(err) => err.toString
    case other                       => other.getMessage

  def exists(path: String): Boolean = NodeFs.existsSync(path)

  def isDirectory(path: String): Boolean =
    NodeFs.existsSync(path) && NodeFs.statSync(path).isDirectory()

  def isRegularFile(path: String): Boolean =
    NodeFs.existsSync(path) && NodeFs.statSync(path).isFile()

  def size(path: String): Long =
    io(s"stat $path")(NodeFs.statSync(path).size.toLong)

  def readString(path: String): String =
    io(s"read $path")(NodeFs.readFileSync(path, "utf8"))

  def writeString(path: String, content: String): Unit =
    io(s"write $path")(NodeFs.writeFileSync(path, content, "utf8"))

  def appendString(path: String, content: String): Unit =
    io(s"append $path")(NodeFs.appendFileSync(path, content, "utf8"))

  def createDirectories(path: String): Unit =
    io(s"mkdir $path")(NodeFs.mkdirSync(path, js.Dynamic.literal(recursive = true)))

  def chmod(path: String, mode: Int): Unit =
    io(s"chmod $path")(NodeFs.chmodSync(path, mode))

  def removeAll(path: String): Unit =
    io(s"remove $path")(NodeFs.rmSync(path, js.Dynamic.literal(recursive = true, force = true)))

  def listDir(path: String): List[DirEntry] =
    if !isDirectory(path) then Nil
    else
      io(s"readdir $path"):
        NodeFs
          .readdirSync(path)
          .toList
          .map: name =>
            val full = NodePath.join(path, name)
            val st = NodeFs.statSync(full)
            DirEntry(name, st.isFile(), st.mtimeMs.toLong)
