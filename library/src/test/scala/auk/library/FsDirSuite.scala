package auk.library

/** [[FsDir]] / `FsDirImpl`: directory creation, listing immediate children
  * (entries/files/dirs), recursive `walk`, recursive content `grep` (with and
  * without a file glob), and the `file`/`dir` child-handle constructors. (Glob
  * matching has its own suite, [[GlobSuite]].) */
class FsDirSuite extends LibSuite:

  private def names(es: List[? <: FsEntry]): List[String] = es.map(_.name).sorted

  // -- makedir ---------------------------------------------------------------

  tmp.test("makedir creates missing parent directories"): d =>
    val deep = d.dir("x1").dir("x2").dir("x3")
    deep.makedir()
    assert(deep.exists)
    assert(deep.isDir)

  tmp.test("makedir is idempotent"): d =>
    val sub = d.dir("once")
    sub.makedir(); sub.makedir()
    assert(sub.exists)

  tmp.test("makedir throws when a path component is an existing file"): d =>
    d.file("blocker").write("x")
    intercept[Throwable](d.dir("blocker").dir("sub").makedir())

  // -- error paths on a missing / non-directory path -------------------------

  tmp.test("entries / files / dirs / walk of a non-existent directory throw"): d =>
    val missing = d.dir("missing")
    intercept[Throwable](missing.entries)
    intercept[Throwable](missing.files)
    intercept[Throwable](missing.dirs)
    intercept[Throwable](missing.walk)

  tmp.test("listing a path that is actually a file throws"): d =>
    d.file("plain.txt").write("x")
    intercept[Throwable](d.file("plain.txt").path.openAsDir.entries)

  // -- entries / files / dirs ------------------------------------------------

  tmp.test("entries lists immediate children, both files and dirs"): d =>
    d.file("a.txt").write("a")
    d.file("b.txt").write("b")
    d.dir("sub").makedir()
    assertEquals(names(d.entries), List("a.txt", "b.txt", "sub"))

  tmp.test("files lists only immediate child files"): d =>
    d.file("a.txt").write("a")
    d.file("b.txt").write("b")
    d.dir("sub").makedir()
    d.dir("sub").file("nested.txt").write("n")
    assertEquals(names(d.files), List("a.txt", "b.txt"))

  tmp.test("dirs lists only immediate child directories"): d =>
    d.file("a.txt").write("a")
    d.dir("one").makedir()
    d.dir("two").makedir()
    assertEquals(names(d.dirs), List("one", "two"))

  tmp.test("listings of an empty directory are empty"): d =>
    assertEquals(d.entries, Nil)
    assertEquals(d.files, Nil)
    assertEquals(d.dirs, Nil)

  // -- walk ------------------------------------------------------------------

  tmp.test("walk returns the entire subtree, including directories"): d =>
    d.file("top.txt").write("t")
    val s = d.dir("s"); s.makedir()
    s.file("deep.txt").write("x")
    s.dir("ss").makedir()
    s.dir("ss").file("deepest.txt").write("y")
    assertEquals(names(d.walk), List("deep.txt", "deepest.txt", "s", "ss", "top.txt"))

  tmp.test("walk of an empty directory is empty"): d =>
    assertEquals(d.walk, Nil)

  tmp.test("walk lists empty subdirectories too"): d =>
    d.dir("empty").makedir()
    assertEquals(names(d.walk), List("empty"))

  tmp.test("walk does not include the directory itself"): d =>
    d.file("only.txt").write("x")
    assert(!d.walk.exists(_.path == d.path))

  // -- file / dir child handles ----------------------------------------------

  tmp.test("file returns a child file handle rooted at this directory"): d =>
    assertEquals(d.file("f.txt").path, d.path / "f.txt")

  tmp.test("dir returns a child directory handle rooted at this directory"): d =>
    assertEquals(d.dir("sub").path, d.path / "sub")

  tmp.test("child handles do not require the child to exist yet"): d =>
    assert(!d.file("ghost.txt").exists)
    assert(!d.dir("ghostdir").exists)

  // -- grep (recursive) ------------------------------------------------------

  tmp.test("grep searches file contents recursively"): d =>
    d.file("a.txt").write("TODO one\nok")
    d.dir("s").makedir()
    d.dir("s").file("b.txt").write("TODO two")
    assertEquals(d.grep("TODO").matches.map(_.line).sorted, List("TODO one", "TODO two"))

  tmp.test("recursive grep reports the matching file in each Match"): d =>
    d.dir("s").makedir()
    val b = d.dir("s").file("b.txt"); b.write("needle")
    val ms = d.grep("needle").matches
    assertEquals(ms.map(_.file), List(b))

  tmp.test("recursive grep that finds nothing is empty"): d =>
    d.file("a.txt").write("nothing here")
    val r = d.grep("absent")
    assert(r.isEmpty)
    assertEquals(r.matches, Nil)

  tmp.test("recursive grep of a malformed pattern raises a clear error, not an empty list"): d =>
    d.file("a.txt").write("x")
    interceptContains("invalid regular expression")(d.grep("(unclosed"))

  tmp.test("recursive grep skips binary files but still searches text siblings"): d =>
    d.file("text.txt").write("needle here")
    // n e e d l e NUL x  — the NUL byte marks it binary, so it is skipped.
    writeBytes(d.path / "bin.dat", Array[Byte](110, 101, 101, 100, 108, 101, 0, 120))
    assertEquals(d.grep("needle").matches.map(_.file), List(d.file("text.txt")))

  tmp.test("grep with a file glob restricts which files are searched"): d =>
    d.file("a.scala").write("TODO s")
    d.file("b.txt").write("TODO t")
    assertEquals(d.grep("TODO", "*.scala").matches.map(_.line), List("TODO s"))

  tmp.test("grep with a recursive file glob reaches into subdirectories"): d =>
    d.file("a.scala").write("TODO top")
    d.dir("p").makedir()
    d.dir("p").file("c.scala").write("TODO deep")
    d.dir("p").file("d.txt").write("TODO ignored")
    assertEquals(d.grep("TODO", "**/*.scala").matches.map(_.line).sorted, List("TODO deep", "TODO top"))

  // -- ignore-aware pruning (grep/walk prune; grepAll/walkAll do not) ---------

  tmp.test("grep skips .git and gitignored files, while grepAll finds them"): d =>
    d.file(".gitignore").write("*.log")
    d.file("keep.txt").write("needle")
    d.file("noise.log").write("needle")
    d.dir(".git").makedir()
    d.dir(".git").file("config").write("needle")
    assertEquals(d.grep("needle").matches.map(_.file.name), List("keep.txt"))
    assertEquals(d.grepAll("needle").matches.map(_.file.name).sorted, List("config", "keep.txt", "noise.log"))

  tmp.test("walk skips .git but walkAll lists it"): d =>
    d.file("a.txt").write("x")
    d.dir(".git").makedir()
    d.dir(".git").file("HEAD").write("ref")
    assert(!names(d.walk).contains(".git"))
    assert(names(d.walkAll).contains(".git"))
