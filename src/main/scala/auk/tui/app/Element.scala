package auk.tui.app

import auk.tui.render.{Color, Span, Style}

/** How [[wrapText]] / a table cell reflows text that exceeds the width. */
enum Wrap:
  /** Break between words; a word longer than the line is split as a last resort. */
  case Word
  /** Break at any code point (verbatim text such as code blocks). */
  case Char

/** Horizontal alignment of a table column's cells. */
enum ColumnAlign:
  case Left, Center, Right

/** The view DSL. A small tree of text, colour, styling, vertical stacking,
  * line breaks, rules, and spinners — laid out by [[Layout]] into styled lines.
  *
  * Mirrors the concepts the app was written against (`Text`, `layout`, `br`,
  * `Empty`, `spinner`, `Color.X(text)`, `.style`, `.render`) so it ports
  * mechanically, but it is our own design over our renderer's primitives.
  *
  * INVARIANT — Elements are width-agnostic by design. A node must never bake the
  * terminal width into its value; all width-dependent reflow (rule expansion,
  * table sizing, text wrapping) is deferred to [[Layout.lay]], which is given the
  * width at render time. Two things rely on this: the resize repaint relays the
  * existing committed Element tree at the new width (it never rebuilds it), and
  * `ChatApp` memoizes committed Elements keyed on `(transcriptEpoch, length)`
  * with NO width in the key. Introduce a width-baking node (e.g. pre-wrapping
  * prose, or embedding a `.render`ed-at-fixed-width string for committed content)
  * and both break silently on resize. Keep width out of the AST.
  */
sealed trait Element

object Element:
  /** Raw text in a style. `value` may contain newlines (split at layout) and may
    * embed our own SGR sequences (re-tokenized into spans at layout). */
  final case class TextNode(value: String, style: Style) extends Element
  /** Vertical stack: children laid out in order, concatenated. */
  final case class Stack(children: Vector[Element]) extends Element
  case object LineBreak extends Element
  case object Blank extends Element
  /** A horizontal rule expanded to the layout width at render time. */
  final case class RuleNode(ch: Char, style: Style) extends Element
  /** An animated spinner: `frame` selects the glyph, `label` follows it. */
  final case class SpinnerNode(label: String, frame: Int, style: Style) extends Element
  /** A style applied over an inner element (base for the inner's own styles). */
  final case class StyledNode(inner: Element, style: Style) extends Element
  /** Text soft-wrapped at layout width, with a distinct first-line prefix. */
  final case class WrappedTextNode(firstPrefix: String, nextPrefix: String, value: String, style: Style, mode: Wrap) extends Element
  /** A table laid out at the layout width: columns size to content when they fit,
    * else share the width and cells wrap. Cells are styled spans; `border` styles
    * the column separators and the header rule. */
  final case class TableNode(
      firstPrefix: String,
      nextPrefix: String,
      align: Vector[ColumnAlign],
      header: Vector[Vector[Span]],
      rows: Vector[Vector[Vector[Span]]],
      border: Style
  ) extends Element

/* ---- Top-level DSL (brought in by `import auk.tui.app.*`) ---- */

def Text(s: String): Element = Element.TextNode(s, Style.Default)
def layout(children: Element*): Element = Element.Stack(children.toVector)
val br: Element = Element.LineBreak
val Empty: Element = Element.Blank
def spinner(label: String, frame: Int): Element = Element.SpinnerNode(label, frame, Style.Default)
def hr(ch: Char = '─', color: Color = Color.Default): Element =
  Element.RuleNode(ch, if color == Color.Default then Style.Default else Style.fg(color))
def wrapText(firstPrefix: String, nextPrefix: String, value: String, mode: Wrap = Wrap.Word): Element =
  Element.WrappedTextNode(firstPrefix, nextPrefix, value, Style.Default, mode)

def table(
    firstPrefix: String,
    nextPrefix: String,
    align: Vector[ColumnAlign],
    header: Vector[Vector[Span]],
    rows: Vector[Vector[Vector[Span]]],
    border: Style
): Element =
  Element.TableNode(firstPrefix, nextPrefix, align, header, rows, border)

extension (c: Color)
  /** Apply this colour to text — mirrors layoutz's `Color.Cyan("text")`. */
  def apply(s: String): Element = Element.TextNode(s, Style.fg(c))

extension (e: Element)
  /** Add a style to this element. Folds into text/styled nodes directly. */
  def style(s: Style): Element = e match
    case Element.TextNode(v, st)       => Element.TextNode(v, st ++ s)
    case Element.StyledNode(inner, st) => Element.StyledNode(inner, st ++ s)
    case other                         => Element.StyledNode(other, s)

  /** Render to a single ANSI string (newline-joined). Used for inline
    * composition where the app builds a string from sub-elements. */
  def render: String = Layout.renderInline(e)
