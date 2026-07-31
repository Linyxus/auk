package auk.library

/** Symlink handling, which no existing suite covered. Directory symlinks are
  * *followed*: listing classifies them as directories and the recursive
  * traversal (`glob("**")`) descends them (cutting only true cycles), while
  * `Path.openAsEntry` surfaces a clear error on a cyclic link rather than
  * masking it as a file. Links are created directly via Node, since the public
  * API has no way to make one. */
class SymlinkSuite extends LibSuite:

  private def names(es: List[? <: FsEntry]): List[String] = es.map(_.name).sorted

  // -- directory symlinks: followed by listing & traversal -------------------

  tmp.test("a directory symlink is classified as a directory and descended by glob"): d =>
    val real = d.dir("real"); real.makedir()
    real.file("f.txt").write("hi")
    symlink(real.path, d.path / "link")
    // entries/dirs see `link` as a directory...
    assert(names(d.dirs).contains("link"))
    assertEquals(d.entries.find(_.name == "link").map(_.isInstanceOf[FsDir]), Some(true))
    // ...and the traversal follows it through to the target's contents.
    assert(d.glob("**").entries.exists(e => e.name == "f.txt" && e.path.toString.contains("/link/")))

  tmp.test("fs.access follows a directory symlink (isDir is true)"): d =>
    val real = d.dir("realb"); real.makedir()
    symlink(real.path, d.path / "linkb")
    assert(lib.fs.access(d.path / "linkb").isDir)

  // -- file symlinks ---------------------------------------------------------

  tmp.test("a file symlink is a file, and access reads through it"): d =>
    val target = d.file("t.txt"); target.write("payload")
    symlink(target.path, d.path / "flink")
    assert(lib.fs.access(d.path / "flink").isFile)
    assertEquals(lib.fs.accessFile(d.path / "flink").rawContent, "payload")

  tmp.test("a broken symlink is a non-existent file handle"): d =>
    symlink(d.path / "no-such-target", d.path / "broken")
    val e = lib.fs.access(d.path / "broken")
    assert(e.isInstanceOf[FsFile])
    assert(!e.exists)
    assert(!e.isDir)

  // -- cycle safety ----------------------------------------------------------

  tmp.test("glob(\"**\") terminates on a directory-symlink cycle"): d =>
    val a = d.dir("a"); a.makedir()
    a.file("real.txt").write("x")
    symlink(a.path, a.path / "loop") // a/loop -> a, a cycle
    val all = d.glob("**").entries // must not hang or stack-overflow
    assert(all.exists(_.name == "loop"))
    assert(all.exists(_.name == "real.txt"))

  tmp.test("openAsEntry surfaces a clear error on a self-referential symlink"): d =>
    // self -> self makes statSync fail with ELOOP, which must not be masked as a file.
    symlink(d.path / "self", d.path / "self")
    interceptContains("cannot open")((d.path / "self").openAsEntry)

  // -- delete operates on the link, not the target ---------------------------

  tmp.test("delete on a symlink removes the link, not its target"): d =>
    val target = d.file("keep.txt"); target.write("v")
    symlink(target.path, d.path / "dlink")
    lib.fs.access(d.path / "dlink").delete()
    assert(!(d.path / "dlink").openAsFile.exists)
    assert(target.exists) // the target survives
