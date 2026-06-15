package auk.library

import scala.scalajs.js

/** The [[FsEntry]] behaviour shared by files and directories (mixed in via
  * `EntryOps`): identity (path/name/parent), existence/type predicates,
  * modification time, and the mutating moves — delete/moveTo/copyTo — plus
  * `Path.openAsEntry`'s file-vs-directory dispatch. */
class FsEntrySuite extends LibSuite:

  // -- identity --------------------------------------------------------------

  tmp.test("path round-trips the handle's location"): d =>
    val f = d.file("a.txt")
    assertEquals(f.path, d.path / "a.txt")

  tmp.test("name is the final path segment"): d =>
    assertEquals(d.file("n.txt").name, "n.txt")
    assertEquals(d.dir("sub").name, "sub")

  tmp.test("parent is the containing directory"): d =>
    val f = d.file("p.txt"); f.write("x")
    assertEquals(f.parent.path, d.path)

  // -- existence & type ------------------------------------------------------

  tmp.test("a written file exists and is a file, not a directory"): d =>
    val f = d.file("x.txt"); f.write("x")
    assert(f.exists)
    assert(f.isFile)
    assert(!f.isDir)

  tmp.test("a directory is a directory, not a file"): d =>
    assert(d.exists)
    assert(d.isDir)
    assert(!d.isFile)

  tmp.test("a non-existent path exists=false and is neither file nor dir"): d =>
    val f = d.file("nope.txt")
    assert(!f.exists)
    assert(!f.isFile)
    assert(!f.isDir)

  // -- modification time -----------------------------------------------------

  tmp.test("lastModifiedMs is a positive epoch milliseconds value"): d =>
    val f = d.file("lm.txt"); f.write("x")
    assert(f.lastModifiedMs > 0L)

  tmp.test("lastModified is a YYYY-MM-DD HH:MM:SS datetime string"): d =>
    val f = d.file("lmf.txt"); f.write("x")
    val s = f.lastModified
    assert(s.matches("""\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}"""), s"unexpected format: '$s'")

  tmp.test("lastModifiedMs on a non-existent entry fails with a clear message"): d =>
    interceptContains("cannot stat")(d.file("ghost.txt").lastModifiedMs)

  tmp.test("lastModified (string) on a non-existent entry fails with a clear message"): d =>
    interceptContains("cannot stat")(d.file("ghost2.txt").lastModified)

  tmp.test("lastModified renders a known mtime in local time with zero-padded fields"): d =>
    val f = d.file("mt.txt"); f.write("x")
    // 2026-03-04 05:06:07 *local* — single-digit month/day/h/m/s pin p2 padding,
    // and using a local js.Date keeps the assertion host-timezone-independent.
    val ms = new js.Date(2026, 2, 4, 5, 6, 7).getTime() // month is 0-based
    setMtime(f.path, ms)
    assertEquals(f.lastModified, "2026-03-04 05:06:07")
    assertEquals(f.lastModifiedMs, ms.toLong)

  // -- delete ----------------------------------------------------------------

  tmp.test("delete removes a file"): d =>
    val f = d.file("del.txt"); f.write("x")
    assert(f.exists)
    f.delete()
    assert(!f.exists)

  tmp.test("delete removes a directory and everything inside it"): d =>
    val sub = d.dir("deldir"); sub.makedir()
    sub.file("inner.txt").write("x")
    sub.dir("nested").makedir()
    assert(sub.exists)
    sub.delete()
    assert(!sub.exists)

  tmp.test("delete of a non-existent entry is a no-op, not an error"): d =>
    d.file("never.txt").delete() // must not throw

  // -- moveTo ----------------------------------------------------------------

  tmp.test("moveTo relocates a file and returns a handle to the new location"): d =>
    val f = d.file("mv1.txt"); f.write("data")
    val dest = d.path / "mv2.txt"
    val moved = f.moveTo(dest)
    assert(!f.exists)
    assertEquals(moved.path, dest)
    assertEquals(dest.openAsFile.rawContent, "data")

  tmp.test("moveTo relocates a directory with its contents"): d =>
    val src = d.dir("mvsrc"); src.makedir()
    src.file("k.txt").write("v")
    val dest = d.path / "mvdest"
    src.moveTo(dest)
    assert(!src.exists)
    assertEquals(dest.openAsDir.file("k.txt").rawContent, "v")

  tmp.test("moveTo of a directory returns a directory handle"): d =>
    val src = d.dir("mvd"); src.makedir()
    assert(src.moveTo(d.path / "mvd2").isInstanceOf[FsDir])

  tmp.test("moveTo into a missing parent directory throws"): d =>
    val f = d.file("mvm.txt"); f.write("x")
    intercept[Throwable](f.moveTo(d.path / "nope" / "x.txt"))

  // -- copyTo ----------------------------------------------------------------

  tmp.test("copyTo duplicates a file, leaving the original in place"): d =>
    val f = d.file("cp1.txt"); f.write("data")
    val dest = d.path / "cp2.txt"
    val copy = f.copyTo(dest)
    assert(f.exists)
    assertEquals(copy.path, dest)
    assertEquals(dest.openAsFile.rawContent, "data")

  tmp.test("copyTo duplicates a directory recursively"): d =>
    val src = d.dir("cpd"); src.makedir()
    src.file("k.txt").write("v")
    src.dir("inner").makedir()
    src.dir("inner").file("deep.txt").write("w")
    val dest = d.path / "cpd2"
    src.copyTo(dest)
    assert(src.exists)
    assertEquals(dest.openAsDir.file("k.txt").rawContent, "v")
    assertEquals(dest.openAsDir.dir("inner").file("deep.txt").rawContent, "w")

  tmp.test("copyTo creates missing destination parents (unlike moveTo, which throws)"): d =>
    // cpSync(recursive=true) makes intermediate dirs, so the doc's "parent must
    // exist" holds for moveTo (renameSync) but not for copyTo — pin the asymmetry.
    val f = d.file("cpm.txt"); f.write("payload")
    val copy = f.copyTo(d.path / "nope" / "x.txt")
    assertEquals(copy.path, d.path / "nope" / "x.txt")
    assertEquals((d.path / "nope" / "x.txt").openAsFile.rawContent, "payload")

  tmp.test("copyTo onto the same path is a no-op, leaving the file intact"): d =>
    val f = d.file("self.txt"); f.write("data")
    f.copyTo(f.path) // must not throw
    assertEquals(f.rawContent, "data")

  tmp.test("copyTo onto a normalized-equal spelling of the same path is a no-op"): d =>
    val f = d.file("self2.txt"); f.write("data")
    f.copyTo(d.path / "." / "self2.txt") // resolves to the same file
    assertEquals(f.rawContent, "data")

  tmp.test("copyTo of a directory into its own subtree raises a clear error"): d =>
    val src = d.dir("own"); src.makedir()
    src.file("k.txt").write("v")
    interceptContains("copyTo failed")(src.copyTo(src.path / "sub"))

  // -- Path.openAsEntry dispatch ---------------------------------------------

  tmp.test("openAsEntry yields a directory handle for a real directory"): d =>
    val sub = d.dir("realdir"); sub.makedir()
    assert(sub.path.openAsEntry.isInstanceOf[FsDir])

  tmp.test("openAsEntry yields a file handle for a real file"): d =>
    val f = d.file("real.txt"); f.write("x")
    assert(f.path.openAsEntry.isInstanceOf[FsFile])

  tmp.test("openAsEntry of a non-existent path falls back to a (non-existent) file"): d =>
    val e = (d.path / "missing").openAsEntry
    assert(e.isInstanceOf[FsFile])
    assert(!e.exists)

  // -- asFile / asDir narrowing ----------------------------------------------

  tmp.test("asFile yields a usable file view of a file entry; asDir on it throws"): d =>
    val e: FsEntry = { val f = d.file("nf.txt"); f.write("hi"); f }
    assert(e.asFile.isInstanceOf[FsFile])
    assertEquals(e.asFile.rawContent, "hi")
    interceptContains("not a directory")(e.asDir)

  tmp.test("asDir yields a usable directory view of a directory entry; asFile on it throws"): d =>
    val e: FsEntry = { val sub = d.dir("nd"); sub.makedir(); sub.file("k.txt").write("v"); sub }
    assert(e.asDir.isInstanceOf[FsDir])
    assertEquals(e.asDir.file("k.txt").rawContent, "v")
    interceptContains("not a file")(e.asFile)

  tmp.test("an entry opened from a real directory narrows with asDir, not asFile"): d =>
    val sub = d.dir("od"); sub.makedir()
    val e = lib.fs.access(sub.path) // openAsEntry → a directory handle
    assert(e.isDir)
    assertEquals(e.asDir.path, sub.path)
    interceptContains("not a file")(e.asFile)

  tmp.test("an entry opened from a real file narrows with asFile, not asDir"): d =>
    val f = d.file("of.txt"); f.write("x")
    val e = lib.fs.access(f.path) // openAsEntry → a file handle
    assert(e.isFile)
    assertEquals(e.asFile.path, f.path)
    interceptContains("not a directory")(e.asDir)

  tmp.test("asFile/asDir return the very same handle (a pure view, no re-open)"): d =>
    val f = d.file("id.txt"); f.write("x")
    assert(f.asFile eq f)
    assert(d.asDir eq d)

  tmp.test("asFile narrows by the handle's kind, so it works on a not-yet-created file"): d =>
    // Consistent with the rest of the library, where a file handle to a missing
    // path is first-class — you create the file through it. asFile must not stat.
    val f = d.file("ghost.txt")
    assert(!f.exists)
    f.asFile.write("now real")
    assertEquals(f.rawContent, "now real")

  tmp.test("asDir on a missing path (opened as an entry → file fallback) throws"): d =>
    val e = (d.path / "missing").openAsEntry // file fallback for a non-existent path
    assert(e.isInstanceOf[FsFile])
    interceptContains("not a directory")(e.asDir)

  tmp.test("directory entries narrow to the matching kind"): d =>
    d.file("child.txt").write("x")
    d.dir("childdir").makedir()
    val byName = d.entries.map(e => e.name -> e).toMap
    assert(byName("child.txt").isFile)
    assertEquals(byName("child.txt").asFile.rawContent, "x")
    assert(byName("childdir").isDir)
    assert(byName("childdir").asDir.entries.isEmpty)

  tmp.test("the narrowing error explains the mismatch and names the offending path"): d =>
    val sub = d.dir("named"); sub.makedir()
    interceptContains("is a directory")(lib.fs.access(sub.path).asFile)
    interceptContains(sub.path.toString)(lib.fs.access(sub.path).asFile)
