package auk.library

import scala.scalajs.js

/** [[FileSystem]] / `FileSystemImpl` — the thin facade over [[Path]]'s open
  * methods, reached as `lib.fs` — plus the top-level [[AukInterface]] wiring. */
class FileSystemSuite extends LibSuite:

  test("fs.cwd reports the process working directory as an absolute path"):
    val cwd = lib.fs.cwd
    assert(cwd.toString.startsWith("/"), s"not absolute: $cwd")
    val nodeCwd = js.Dynamic.global.process.cwd().asInstanceOf[String]
    assertEquals(cwd, lib.path(nodeCwd))

  tmp.test("access resolves an existing directory to a directory entry"): d =>
    val sub = d.dir("ad"); sub.makedir()
    assert(lib.fs.access(sub.path).isDir)

  tmp.test("access resolves an existing file to a file entry"): d =>
    val f = d.file("af.txt"); f.write("x")
    assert(lib.fs.access(f.path).isFile)

  tmp.test("accessFile opens a path as a file regardless of existence"): d =>
    val f = d.file("g.txt"); f.write("content")
    assertEquals(lib.fs.accessFile(f.path).rawContent, "content")
    assert(!lib.fs.accessFile(d.path / "ghost.txt").exists)

  tmp.test("accessDir opens a path as a directory"): d =>
    val sub = d.dir("dd"); sub.makedir()
    assert(lib.fs.accessDir(sub.path).isDir)

  // -- AukInterface wiring ---------------------------------------------------

  test("lib.path constructs a Path from a string"):
    assertEquals(lib.path("/a/b").toString, "/a/b")

  test("lib exposes a file system and a shell"):
    assert(lib.fs ne null)
    assert(lib.shell ne null)
