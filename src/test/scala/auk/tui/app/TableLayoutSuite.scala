package auk.tui.app

class TableLayoutSuite extends munit.FunSuite:

  test("columns keep their natural width when they fit"):
    assertEquals(TableLayout.columnWidths(Vector(3, 4), Vector(1, 1), 20), Vector(3, 4))

  test("surplus is shared in proportion to slack, summing to avail"):
    // naturals 10+10=20 > 12; mins 2+2=4; surplus 8 split evenly.
    assertEquals(TableLayout.columnWidths(Vector(10, 10), Vector(2, 2), 12), Vector(6, 6))

  test("a column with no slack is not shrunk; the slack column absorbs it"):
    // col0 natural==min (no slack), col1 has all the slack.
    val w = TableLayout.columnWidths(Vector(10, 2), Vector(2, 2), 8)
    assertEquals(w, Vector(6, 2))
    assertEquals(w.sum, 8)

  test("shrink widths always sum exactly to avail (largest remainder)"):
    val w = TableLayout.columnWidths(Vector(10, 10, 10), Vector(1, 1, 1), 14)
    assertEquals(w.sum, 14)
    assert(w.forall(_ >= 1), w.toString)

  test("when even the minimums overflow, widths scale down but stay >= 1"):
    val w = TableLayout.columnWidths(Vector(10, 10), Vector(8, 8), 10)
    assertEquals(w.sum, 10)
    assert(w.forall(_ >= 1), w.toString)

  test("empty input yields no columns"):
    assertEquals(TableLayout.columnWidths(Vector.empty, Vector.empty, 40), Vector.empty)
