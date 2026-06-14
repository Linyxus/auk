package auk.tui.markdown

class MarkdownDocumentSuite extends munit.FunSuite:
  import Inline.*

  private def para(s: String): Block = Block.Paragraph(InlineParser.parse(s))

  /** Feed a string one character at a time, as the typewriter reveal would. */
  private def feedCharByChar(s: String): MarkdownDocument =
    (1 to s.length).foldLeft(MarkdownDocument.empty)((d, i) => d.feedTo(s.substring(0, i)))

  test("an open document renders the same blocks as a one-shot parse"):
    for src <- Seq(
        "# H\n\nbody text",
        "a\n\nb\n\nc",
        "- one\n- two\n\nafter",
        "```scala\nval x = 1\n```\n\ndone",
        "> quote\n> more\n\ntail",
        "| a | b |\n|---|---|\n| 1 | 2 |\n\nnext"
      )
    do assertEquals(MarkdownDocument.empty.feedTo(src).blocks, BlockParser.parse(src), src)

  test("feeding character by character converges to the one-shot parse"):
    val src = "# Title\n\nA paragraph with **bold** and `code`.\n\n- a\n- b\n\n```\nx = 1\n```\n\nEnd."
    assertEquals(feedCharByChar(src).close().blocks, BlockParser.parse(src))

  test("closed and open renderings agree for finished text"):
    val src = "para one\n\npara two"
    assertEquals(MarkdownDocument.empty.feedTo(src).close().blocks, BlockParser.parse(src))

  test("earlier blocks finalise and are not disturbed by later feeds"):
    val a = MarkdownDocument.empty.feedTo("alpha\n\nbeta\n\n")
    assert(a.finalizedCount >= 1, s"expected a finalised block, got ${a.finalizedCount}")
    val b = a.feedTo("alpha\n\nbeta\n\ngamma")
    // The finalised prefix is preserved verbatim across the later feed.
    assertEquals(b.finalized.take(a.finalizedCount), a.finalized)
    assertEquals(b.blocks, Vector(para("alpha"), para("beta"), para("gamma")))

  test("an unclosed emphasis is literal until its closer is revealed (snap)"):
    val d1 = MarkdownDocument.empty.feedTo("**wor")
    assertEquals(d1.blocks, Vector(Block.Paragraph(Vector(Text("**wor")))))
    val d2 = d1.feedTo("**world**")
    assertEquals(d2.blocks, Vector(Block.Paragraph(Vector(Strong(Vector(Text("world")))))))

  test("a paragraph snaps to a setext heading when the underline arrives"):
    val d1 = MarkdownDocument.empty.feedTo("Title\n")
    assertEquals(d1.blocks, Vector(para("Title")))
    val d2 = d1.feedTo("Title\n===")
    assertEquals(d2.blocks, Vector(Block.Heading(1, Vector(Text("Title")))))

  test("a paragraph snaps to a table when the delimiter row arrives"):
    val d1 = MarkdownDocument.empty.feedTo("a | b\n")
    assertEquals(d1.blocks, Vector(para("a | b")))
    val d2 = d1.feedTo("a | b\n--- | ---\n")
    assertEquals(d2.blocks.head.getClass.getSimpleName, "Table")

  test("a fence stays open across feeds and closes only on its closing fence"):
    val d1 = MarkdownDocument.empty.feedTo("```\nco")
    assertEquals(d1.blocks, Vector(Block.Code(None, None, "co", fenced = true)))
    val d2 = d1.feedTo("```\ncode\n```")
    assertEquals(d2.blocks, Vector(Block.Code(None, None, "code", fenced = true)))

  test("the partial last line is never finalised before its newline"):
    val d = MarkdownDocument.empty.feedTo("done\n\nopen line still typing")
    // Only the first paragraph can be final; the trailing line stays open.
    assert(d.finalizedCount <= 1)
    assertEquals(d.blocks.last, para("open line still typing"))

  test("incremental feeding scales over many blocks"):
    val src = (1 to 400).map(i => s"paragraph number $i").mkString("\n\n")
    val lines = src.split("\n", -1)
    var d = MarkdownDocument.empty
    val sb = new StringBuilder
    lines.foreach: l =>
      sb.append(l).append('\n')
      d = d.feedTo(sb.toString)
    d = d.close()
    assertEquals(d.blocks.length, 400)
    assertEquals(d.blocks, BlockParser.parse(src))
