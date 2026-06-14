package auk.tui.markdown

class ListSuite extends munit.FunSuite:
  import Inline.*

  private def parse(s: String): Vector[Block] = BlockParser.parse(s)
  private def para(s: String): Block = Block.Paragraph(InlineParser.parse(s))
  private def item(blocks: Block*): ListItem = ListItem(blocks.toVector, None)

  test("bullet list, tight"):
    assertEquals(
      parse("- a\n- b"),
      Vector(Block.ListBlock(ordered = false, start = 1, tight = true, Vector(item(para("a")), item(para("b")))))
    )

  test("bullet markers + and * each form their own list"):
    assertEquals(parse("- a\n+ b").length, 2)
    assertEquals(parse("- a\n* b").length, 2)

  test("ordered list keeps its start ordinal"):
    assertEquals(
      parse("3. a\n4. b"),
      Vector(Block.ListBlock(ordered = true, start = 3, tight = true, Vector(item(para("a")), item(para("b")))))
    )

  test("ordered with ) delimiter"):
    assertEquals(parse("1) a").head.asInstanceOf[Block.ListBlock].ordered, true)

  test("nested list under an item"):
    val parsed = parse("- a\n  - b")
    val outer = parsed.head.asInstanceOf[Block.ListBlock]
    assertEquals(outer.items.length, 1)
    assertEquals(outer.items.head.blocks.head, para("a"))
    assertEquals(
      outer.items.head.blocks(1),
      Block.ListBlock(ordered = false, start = 1, tight = true, Vector(item(para("b"))))
    )

  test("a blank line between items makes the list loose"):
    val lb = parse("- a\n\n- b").head.asInstanceOf[Block.ListBlock]
    assertEquals(lb.tight, false)
    assertEquals(lb.items.length, 2)

  test("a blank line inside an item makes the list loose"):
    val lb = parse("- a\n\n  b").head.asInstanceOf[Block.ListBlock]
    assertEquals(lb.tight, false)
    assertEquals(lb.items.head.blocks, Vector(para("a"), para("b")))

  test("task list items"):
    assertEquals(parse("- [ ] todo").head.asInstanceOf[Block.ListBlock].items.head,
      ListItem(Vector(para("todo")), Some(false)))
    assertEquals(parse("- [x] done").head.asInstanceOf[Block.ListBlock].items.head,
      ListItem(Vector(para("done")), Some(true)))
    assertEquals(parse("- [X] done").head.asInstanceOf[Block.ListBlock].items.head.task, Some(true))

  test("[] without a space is not a task"):
    assertEquals(parse("- [] x").head.asInstanceOf[Block.ListBlock].items.head.task, None)

  test("a bullet interrupts a paragraph; ordered does so only when it starts at 1"):
    assertEquals(parse("text\n- item").length, 2)
    assertEquals(parse("text\n1. item").length, 2)
    // An ordered marker that doesn't start at 1 is just paragraph text.
    assertEquals(parse("text\n2. item"), Vector(Block.Paragraph(Vector(Text("text"), SoftBreak, Text("2. item")))))
