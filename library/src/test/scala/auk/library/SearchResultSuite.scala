package auk.library

import scala.scalajs.js

/** [[GrepResult]] / [[GlobResult]]: the objects `grep` and `glob` hand back.
  *
  * Most cases drive the implementations directly over fabricated engine rows —
  * the windowing, the markers and the summaries are pure rendering, and rows made
  * here can be exactly as many, and exactly as awkward, as a case needs. The
  * suite ends with a few cases over a real directory so the rendering is pinned
  * against rows the engine itself produced.
  *
  * The laziness claim is tested by construction: [[poisonedRows]] rows throw
  * when any field is read, so a `length` / `isEmpty` / `toString` that survives
  * them provably never touched a row, and the `matches` that fails provably did.
  */
class SearchResultSuite extends LibSuite:

  /** `{path, line, text}` rows, as the engine's grep entry points return them. */
  private def matchRows(rows: (String, Int, String)*): js.Array[js.Dynamic] =
    js.Array(rows.map((p, l, t) => js.Dynamic.literal(path = p, line = l, text = t))*)

  /** `{path, dir}` rows, as the engine's glob entry points return them. */
  private def entryRows(rows: (String, Boolean)*): js.Array[js.Dynamic] =
    js.Array(rows.map((p, d) => js.Dynamic.literal(path = p, dir = d))*)

  /** `n` rows that throw as soon as any of their fields is read. */
  private def poisonedRows(n: Int): js.Array[js.Dynamic] =
    val arr = js.Array[js.Dynamic]()
    for _ <- 0 until n do
      val row = js.Dynamic.literal()
      for field <- List("path", "line", "text", "dir") do
        val read: js.Function0[Any] = () => throw new RuntimeException(s"row field '$field' was read")
        js.Dynamic.global.Object.defineProperty(row, field, js.Dynamic.literal(get = read))
      arr.push(row)
    arr

  /** `n` plain grep rows, `a.txt:1@ line 1` … — enough of them to window. */
  private def manyRows(n: Int): js.Array[js.Dynamic] =
    matchRows((1 to n).map(i => ("a.txt", i, s"line $i"))*)

  /** What `body` printed, without the trailing newline `println` adds. */
  private def printed(body: => Unit): String = captured(body).stripSuffix("\n")

  // -- GrepResult: display windowing -----------------------------------------

  test("display prints every match as path:linenum@ line, in row order"):
    val r = GrepResultImpl(matchRows(("/a.txt", 1, "foo"), ("/a.txt", 3, "foobar"), ("/b.txt", 2, "food")))
    assertEquals(printed(r.display()), "/a.txt:1@ foo\n/a.txt:3@ foobar\n/b.txt:2@ food")

  test("display defaults to the first 200 matches and reports the rest"):
    val r = GrepResultImpl(manyRows(250))
    val out = printed(r.display()).split("\n").toList
    assertEquals(out.length, 201)
    assertEquals(out.head, "a.txt:1@ line 1")
    assertEquals(out(199), "a.txt:200@ line 200")
    assertEquals(out.last, "(... 50 more matches ...)")

  test("display with an offset says how many it skipped"):
    val r = GrepResultImpl(manyRows(5))
    assertEquals(
      printed(r.display(offset = 2)),
      """(... 2 matches skipped ...)
        |a.txt:3@ line 3
        |a.txt:4@ line 4
        |a.txt:5@ line 5""".stripMargin
    )

  test("display with a limit says how many it held back"):
    val r = GrepResultImpl(manyRows(5))
    assertEquals(
      printed(r.display(limit = 2)),
      """a.txt:1@ line 1
        |a.txt:2@ line 2
        |(... 3 more matches ...)""".stripMargin
    )

  test("display of a middle window carries both markers"):
    val r = GrepResultImpl(manyRows(5))
    assertEquals(
      printed(r.display(offset = 2, limit = 2)),
      """(... 2 matches skipped ...)
        |a.txt:3@ line 3
        |a.txt:4@ line 4
        |(... 1 more match ...)""".stripMargin
    )

  test("the markers read singular for one match"):
    val r = GrepResultImpl(manyRows(3))
    assertEquals(
      printed(r.display(offset = 1, limit = 1)),
      """(... 1 match skipped ...)
        |a.txt:2@ line 2
        |(... 1 more match ...)""".stripMargin
    )

  test("a negative limit prints to the end"):
    val r = GrepResultImpl(manyRows(3))
    assertEquals(
      printed(r.display(offset = 1, limit = -1)),
      """(... 1 match skipped ...)
        |a.txt:2@ line 2
        |a.txt:3@ line 3""".stripMargin
    )

  test("a negative offset starts at the beginning, with no skipped marker"):
    val r = GrepResultImpl(manyRows(2))
    assertEquals(printed(r.display(offset = -5)), "a.txt:1@ line 1\na.txt:2@ line 2")

  test("an offset past the end skips only what was there and says the window is empty"):
    val r = GrepResultImpl(manyRows(3))
    assertEquals(
      printed(r.display(offset = 99)),
      """(... 3 matches skipped ...)
        |(no matches in this window)""".stripMargin
    )

  test("a zero limit prints no matches but still reports them all"):
    val r = GrepResultImpl(manyRows(3))
    assertEquals(
      printed(r.display(limit = 0)),
      """(no matches in this window)
        |(... 3 more matches ...)""".stripMargin
    )

  test("display of an empty result says so"):
    assertEquals(printed(GrepResultImpl(matchRows()).display()), "(no matches)")
    assertEquals(printed(GrepResultImpl(matchRows()).display(offset = 10, limit = 2)), "(no matches)")

  // -- GrepResult: length, matches, rendering --------------------------------

  test("length / isEmpty / nonEmpty count the rows"):
    val r = GrepResultImpl(manyRows(3))
    assertEquals(r.length, 3)
    assert(r.nonEmpty)
    assert(!r.isEmpty)
    val empty = GrepResultImpl(matchRows())
    assertEquals(empty.length, 0)
    assert(empty.isEmpty)
    assert(!empty.nonEmpty)

  test("length, emptiness and rendering never read a row"):
    val r = GrepResultImpl(poisonedRows(4))
    assertEquals(r.length, 4)
    assert(r.nonEmpty)
    assert(!r.isEmpty)
    assertEquals(r.toString, "GrepResult(4 matches — use .display() or .matches)")
    // ... and a window that selects no row renders without reading one either.
    assertEquals(printed(r.display(limit = 0)), "(no matches in this window)\n(... 4 more matches ...)")
    // Only asking for the matches materializes them — here, into the poison.
    interceptContains("row field")(r.matches)

  test("matches rebuilds the rows as Matches, in row order"):
    val r = GrepResultImpl(matchRows(("/a.txt", 1, "foo"), ("/b.txt", 7, "bar")))
    val ms = r.matches
    assertEquals(ms.map(_.file.path.toString), List("/a.txt", "/b.txt"))
    assertEquals(ms.map(_.lineNumber), List(1, 7))
    assertEquals(ms.map(_.line), List("foo", "bar"))

  test("matches is materialized once and cached"):
    val r = GrepResultImpl(manyRows(3))
    val first = r.matches
    assert(first eq r.matches)

  test("a result renders as a one-line summary, never as its matches"):
    assertEquals(GrepResultImpl(matchRows()).toString, "GrepResult(no matches)")
    assertEquals(
      GrepResultImpl(manyRows(1)).toString,
      "GrepResult(1 match — use .display() or .matches)"
    )
    assertEquals(
      GrepResultImpl(manyRows(110186)).toString,
      "GrepResult(110186 matches — use .display() or .matches)"
    )

  // -- GlobResult ------------------------------------------------------------

  test("glob display prints one path per line, directories with a trailing slash"):
    val r = GlobResultImpl(entryRows(("/p/a.scala", false), ("/p/sub", true), ("/p/sub/b.scala", false)))
    assertEquals(printed(r.display()), "/p/a.scala\n/p/sub/\n/p/sub/b.scala")

  test("glob display windows and marks like grep, in entries"):
    val r = GlobResultImpl(entryRows((1 to 5).map(i => (s"/p/f$i.txt", false))*))
    assertEquals(
      printed(r.display(offset = 1, limit = 2)),
      """(... 1 entry skipped ...)
        |/p/f2.txt
        |/p/f3.txt
        |(... 2 more entries ...)""".stripMargin
    )

  test("glob display of an empty result says so"):
    assertEquals(printed(GlobResultImpl(entryRows()).display()), "(no entries)")

  test("glob length never reads a row, entries does"):
    val r = GlobResultImpl(poisonedRows(3))
    assertEquals(r.length, 3)
    assertEquals(r.toString, "GlobResult(3 entries — use .display() or .entries)")
    interceptContains("row field")(r.entries)

  test("glob entries rebuilds handles by kind, and is cached"):
    val r = GlobResultImpl(entryRows(("/p/a.scala", false), ("/p/sub", true)))
    val es = r.entries
    assertEquals(es.map(_.path.toString), List("/p/a.scala", "/p/sub"))
    assert(es.head.isInstanceOf[FsFile])
    assert(es(1).isInstanceOf[FsDir])
    assert(es eq r.entries)

  test("a glob result renders as a one-line summary"):
    assertEquals(GlobResultImpl(entryRows()).toString, "GlobResult(no entries)")
    assertEquals(
      GlobResultImpl(entryRows(("/p/a.scala", false))).toString,
      "GlobResult(1 entry — use .display() or .entries)"
    )

  // -- over the real engine --------------------------------------------------

  test("a Match renders exactly as display's lines do"):
    val r = GrepResultImpl(matchRows(("/a.txt", 2, "hit")))
    assertEquals(printed(r.display()), r.matches.head.toString)

  tmp.test("grep over a real directory displays engine rows"): d =>
    d.file("a.txt").write("needle one\nquiet\nneedle two")
    val r = d.grep("needle")
    assertEquals(r.length, 2)
    val a = d.file("a.txt").path
    assertEquals(printed(r.display()), s"$a:1@ needle one\n$a:3@ needle two")
    assertEquals(printed(r.display(offset = 1)), s"(... 1 match skipped ...)\n$a:3@ needle two")
    assertEquals(r.toString, "GrepResult(2 matches — use .display() or .matches)")

  tmp.test("glob over a real directory displays engine rows"): d =>
    d.file("a.scala").write("")
    d.dir("p").makedir()
    d.dir("p").file("c.scala").write("")
    val r = d.glob("**")
    val root = d.path.toString
    assertEquals(r.length, 3)
    assertEquals(
      printed(r.display()).split("\n").toList.sorted,
      List(s"$root/a.scala", s"$root/p/", s"$root/p/c.scala").sorted
    )
    assertEquals(r.entries.map(_.name).sorted, List("a.scala", "c.scala", "p"))

  tmp.test("a grep that finds nothing displays as no matches"): d =>
    d.file("a.txt").write("quiet")
    val r = d.grep("needle")
    assert(r.isEmpty)
    assertEquals(printed(r.display()), "(no matches)")
    assertEquals(r.toString, "GrepResult(no matches)")
