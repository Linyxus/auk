package auk.tui.markdown

import auk.tui.app.{Element, Layout}
import auk.tui.markdown.render.{AnswerRenderCache, MarkdownRender}

/** The cached `answerBlock` must be invisible: byte-identical laid output to
  * the uncached render at every feed step, while reusing the finalised blocks'
  * Elements (and their layout memos) across calls. */
class AnswerRenderCacheSuite extends munit.FunSuite:

  private val Source =
    "# Title\n\nfirst paragraph with some text\n\n- item one\n- item two\n\n" +
      "```scala\nval x = 1\n```\n\nsecond paragraph, still going\n\nthird paragraph tail"

  /** Feed `text` into a document in `step`-sized chunks, calling `f` at each step. */
  private def feedSteps(text: String, step: Int)(f: MarkdownDocument => Unit): Unit =
    var d = MarkdownDocument.empty
    var i = 0
    while i < text.length do
      i = math.min(text.length, i + step)
      d = d.feedTo(text.take(i))
      f(d)

  /** Laid ANSI lines — the ground truth the renderer consumes. */
  private def laid(e: Element, w: Int = 40): Vector[String] = Layout.lay(e, w).map(_.render)

  private def children(e: Element): Vector[Element] = e match
    case Element.Stack(cs) => cs
    case other             => Vector(other)

  test("cached plain render matches the uncached one at every feed step"):
    val cache = AnswerRenderCache()
    feedSteps(Source, 7): d =>
      assertEquals(
        laid(MarkdownRender.answerBlock(d, None, cache)),
        laid(MarkdownRender.answerBlock(d, glow = None))
      )

  test("cached glow render matches the uncached one at every feed step"):
    val cache = AnswerRenderCache()
    feedSteps(Source, 13): d =>
      val glow = Some((4, 2))
      assertEquals(
        laid(MarkdownRender.answerBlock(d, glow, cache)),
        laid(MarkdownRender.answerBlock(d, glow))
      )

  test("finalised blocks' Elements are rendered once and reused by reference"):
    val cache = AnswerRenderCache()
    val d1 = MarkdownDocument.empty.feedTo("first para\n\nsecond para\n\nthird")
    assert(d1.finalizedCount >= 1, s"expected a finalised block, got ${d1.finalizedCount}")
    val e1 = MarkdownRender.answerBlock(d1, None, cache)
    val d2 = d1.feedTo(d1.source + " grows\n\nnext para\n\nlast")
    val e2 = MarkdownRender.answerBlock(d2, None, cache)
    // joinSpaced puts the first finalised block's Element first in the stack.
    assert(children(e1).head eq children(e2).head, "first finalised Element should be the same instance")

  test("a memoized block lays once per width and recomputes on resize"):
    val cache = AnswerRenderCache()
    val d = MarkdownDocument.parse("alpha beta gamma delta epsilon\n\ntail")
    val memoNode = children(MarkdownRender.answerBlock(d, None, cache)).head
    val l1 = Layout.lay(memoNode, 12)
    assert(Layout.lay(memoNode, 12) eq l1, "same width should replay the memoized lines")
    val l2 = Layout.lay(memoNode, 20)
    assert(l1 ne l2)
    // The recomputed lines match a fresh uncached render at the new width.
    val uncachedFirst = children(MarkdownRender.render(d.blocks)).head
    assertEquals(l2.map(_.render), Layout.lay(uncachedFirst, 20).map(_.render))

  test("two documents sharing one cache stay correct"):
    val cache = AnswerRenderCache()
    val a = MarkdownDocument.parse("para a1\n\npara a2\n\npara a3")
    val b = MarkdownDocument.parse("para b1\n\npara b2")
    for _ <- 1 to 3 do
      assertEquals(laid(MarkdownRender.answerBlock(a, None, cache)), laid(MarkdownRender.answerBlock(a, glow = None)))
      assertEquals(laid(MarkdownRender.answerBlock(b, None, cache)), laid(MarkdownRender.answerBlock(b, glow = None)))

  test("glow on a fully-finalised document glows the last block, cached prefix intact"):
    val cache = AnswerRenderCache()
    val d = MarkdownDocument.parse("first para\n\nsecond para")
    val glow = Some((3, 1))
    assertEquals(laid(MarkdownRender.answerBlock(d, glow, cache)), laid(MarkdownRender.answerBlock(d, glow)))

  test("an empty document with glow renders just the cursor"):
    val glow = Some((3, 1))
    assertEquals(
      laid(MarkdownRender.answerBlock(MarkdownDocument.empty, glow, AnswerRenderCache())),
      laid(MarkdownRender.answerBlock(MarkdownDocument.empty, glow))
    )
