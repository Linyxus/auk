package auk.tui.markdown

class BlockParserSuite extends munit.FunSuite:
  import Inline.*

  private def parse(s: String): Vector[Block] = BlockParser.parse(s)
  private def para(s: String): Block = Block.Paragraph(InlineParser.parse(s))

  test("ATX headings of each level"):
    assertEquals(parse("# Hi"), Vector(Block.Heading(1, Vector(Text("Hi")))))
    assertEquals(parse("### a b"), Vector(Block.Heading(3, Vector(Text("a b")))))

  test("a closing run of #'s is stripped"):
    assertEquals(parse("# Hi #"), Vector(Block.Heading(1, Vector(Text("Hi")))))

  test("not a heading: no space, or seven hashes"):
    assertEquals(parse("#no"), Vector(para("#no")))
    assertEquals(parse("####### x"), Vector(para("####### x")))

  test("thematic breaks"):
    assertEquals(parse("---"), Vector(Block.ThematicBreak))
    assertEquals(parse("***"), Vector(Block.ThematicBreak))
    assertEquals(parse("- - -"), Vector(Block.ThematicBreak))

  test("paragraph joins lines with a soft break"):
    assertEquals(parse("a\nb"), Vector(Block.Paragraph(Vector(Text("a"), SoftBreak, Text("b")))))

  test("blank line separates paragraphs"):
    assertEquals(parse("a\n\nb"), Vector(para("a"), para("b")))

  test("setext headings"):
    assertEquals(parse("Title\n==="), Vector(Block.Heading(1, Vector(Text("Title")))))
    assertEquals(parse("Title\n---"), Vector(Block.Heading(2, Vector(Text("Title")))))

  test("a dashed line with no preceding paragraph is a thematic break"):
    assertEquals(parse("\n---"), Vector(Block.ThematicBreak))

  test("fenced code keeps its info string's first word as the language"):
    assertEquals(parse("```scala\nval x = 1\n```"), Vector(Block.Code(Some("scala"), Some("scala"), "val x = 1", fenced = true)))

  test("tilde fence"):
    assertEquals(parse("~~~\nx\n~~~"), Vector(Block.Code(None, None, "x", fenced = true)))

  test("fenced code unterminated at EOF still closes"):
    assertEquals(parse("```\nx"), Vector(Block.Code(None, None, "x", fenced = true)))

  test("blank lines inside a fence are preserved"):
    assertEquals(parse("```\na\n\nb\n```"), Vector(Block.Code(None, None, "a\n\nb", fenced = true)))

  test("indented code block"):
    assertEquals(parse("    code"), Vector(Block.Code(None, None, "code", fenced = false)))

  test("blockquote with a lazy/multi-line paragraph"):
    assertEquals(parse("> a\n> b"), Vector(Block.Quote(Vector(Block.Paragraph(Vector(Text("a"), SoftBreak, Text("b")))))))

  test("nested blockquote"):
    assertEquals(parse("> > deep"), Vector(Block.Quote(Vector(Block.Quote(Vector(para("deep")))))))

  test("blockquote containing a fenced code block"):
    assertEquals(
      parse("> ```\n> x\n> ```"),
      Vector(Block.Quote(Vector(Block.Code(None, None, "x", fenced = true))))
    )

  test("thematic break between two paragraphs"):
    assertEquals(parse("a\n\n---\n\nb"), Vector(para("a"), Block.ThematicBreak, para("b")))
