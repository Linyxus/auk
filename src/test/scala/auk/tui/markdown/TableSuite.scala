package auk.tui.markdown

class TableSuite extends munit.FunSuite:
  import Inline.*

  private def parse(s: String): Vector[Block] = BlockParser.parse(s)
  private def cell(s: String): Vector[Inline] = InlineParser.parse(s)

  test("a minimal two-column table"):
    assertEquals(
      parse("| a | b |\n|---|---|\n| 1 | 2 |"),
      Vector(Block.Table(
        align = Vector(Align.None, Align.None),
        header = Vector(cell("a"), cell("b")),
        rows = Vector(Vector(cell("1"), cell("2")))
      ))
    )

  test("column alignment from the delimiter row"):
    val t = parse("| l | c | r |\n|:--|:-:|--:|\n| 1 | 2 | 3 |").head.asInstanceOf[Block.Table]
    assertEquals(t.align, Vector(Align.Left, Align.Center, Align.Right))

  test("a table without outer pipes"):
    val t = parse("a | b\n--- | ---\n1 | 2").head.asInstanceOf[Block.Table]
    assertEquals(t.header, Vector(cell("a"), cell("b")))
    assertEquals(t.rows, Vector(Vector(cell("1"), cell("2"))))

  test("ragged rows are padded and over-long rows truncated"):
    val t = parse("| a | b |\n|---|---|\n| 1 |\n| 1 | 2 | 3 |").head.asInstanceOf[Block.Table]
    assertEquals(t.rows(0), Vector(cell("1"), Vector.empty))
    assertEquals(t.rows(1), Vector(cell("1"), cell("2")))

  test("an escaped pipe stays inside the cell"):
    val t = parse("| a | b |\n|---|---|\n| x \\| y | z |").head.asInstanceOf[Block.Table]
    assertEquals(t.rows(0)(0), cell("x \\| y"))

  test("a pipe inside a code span does not split the cell"):
    val t = parse("| a | b |\n|---|---|\n| `x|y` | z |").head.asInstanceOf[Block.Table]
    assertEquals(t.rows(0), Vector(Vector(Code("x|y")), cell("z")))

  test("cells carry inline formatting"):
    val t = parse("| a | b |\n|---|---|\n| **x** | _y_ |").head.asInstanceOf[Block.Table]
    assertEquals(t.rows(0)(0), Vector(Strong(Vector(Text("x")))))

  test("a header line with no valid delimiter row is just a paragraph"):
    assertEquals(parse("a | b\nnot a delimiter"), Vector(Block.Paragraph(Vector(Text("a | b"), SoftBreak, Text("not a delimiter")))))
