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
