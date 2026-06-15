package auk.library

/** Pure path algebra ([[Path]] / `PathImpl`): no construction here touches the
  * file system, so these need no fixture. */
class PathSuite extends munit.FunSuite:
  private val lib: AukInterface = new AukImpl

  test("`/` joins segments"):
    assertEquals((lib.path("/foo") / "bar" / "baz").toString, "/foo/bar/baz")

  test("`/` normalizes `.`"):
    assertEquals((lib.path("/foo") / "." / "bar").toString, "/foo/bar")

  test("`/` normalizes `..`"):
    assertEquals((lib.path("/foo/bar") / ".." / "qux").toString, "/foo/qux")

  test("`/` collapses redundant separators"):
    assertEquals((lib.path("/foo/") / "/bar").toString, "/foo/bar")

  test("`/` works on relative roots"):
    assertEquals((lib.path("foo") / "bar").toString, "foo/bar")

  test("baseName is the final segment"):
    assertEquals(lib.path("/a/b/c.txt").baseName, "c.txt")

  test("baseName of a trailing-slash path drops the slash"):
    assertEquals(lib.path("/a/b/").baseName, "b")

  test("parent drops the final segment"):
    assertEquals(lib.path("/a/b/c.txt").parent.toString, "/a/b")

  test("parent of a top-level path is the root"):
    assertEquals(lib.path("/a").parent.toString, "/")

  test("toString is the raw path verbatim"):
    assertEquals(lib.path("/x/y").toString, "/x/y")

  test("equality is by path string"):
    assertEquals(lib.path("/a/b"), lib.path("/a/b"))
    assertNotEquals(lib.path("/a"), lib.path("/b"))

  test("the constructor stores the string verbatim — only `/` normalizes"):
    // `lib.path` does not touch the string, so an un-normalized spelling stays
    // distinct; joining with `/` runs it through path.join and collapses `..`.
    assertNotEquals(lib.path("/a/x/../b"), lib.path("/a/b"))
    assertEquals((lib.path("/a/x") / ".." / "b"), lib.path("/a/b"))

  test("hashCode is consistent with equals"):
    assertEquals((lib.path("/a/b")).hashCode, (lib.path("/a") / "b").hashCode)

  test("a Path is not equal to a non-Path value"):
    assertNotEquals[Any, Any](lib.path("/a"), "/a")

  test("openAsFile / openAsDir are pure handles that do not require existence"):
    // A path to nothing still opens; the handle just reports it absent.
    val f = lib.path("/no/such/file.txt").openAsFile
    val d = lib.path("/no/such/dir").openAsDir
    assert(!f.exists)
    assert(!d.exists)
    assertEquals(f.path, lib.path("/no/such/file.txt"))
    assertEquals(d.path, lib.path("/no/such/dir"))
