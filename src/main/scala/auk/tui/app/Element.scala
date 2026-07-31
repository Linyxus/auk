package auk.tui.app

import auk.tui.render.{Color, Span, Style, StyledLine}

/** How [[wrapText]] / a table cell reflows text that exceeds the width. */
enum Wrap:
  /** Break between words; a word longer than the line is split as a last resort. */
  case Word
  /** Break at any code point (verbatim text such as code blocks). */
  case Char

/** Horizontal alignment of a table column's cells. */
enum ColumnAlign:
  case Left, Center, Right

/** Which rows an [[Element.ClipNode]] keeps when its inner element lays taller
  * than the cap. */
enum ClipFocus:
  /** Keep the first rows; one trailing `… N more lines` marker stands for the
    * hidden tail. */
  case Head
  /** Keep a window around the cursor row — the first laid row holding an
    * underline-styled span, the input box's cursor cell — centered when
    * possible, pinned at the edges, with a marker on each clipped side. */
  case Cursor

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
  /** A full-width rule with a centered label — `──── label ────` — expanded at
    * render time. Width-agnostic like [[RuleNode]]: the dash counts are computed
    * from the width [[Layout.lay]] is given, never baked in. */
  final case class LabelledRuleNode(label: String, style: Style) extends Element
  /** A rounded-corner box expanded to the layout width at render time: `inner`
    * is laid at `width - 4` and framed as `│ inner │` rows between `╭─…─╮` and
    * `╰─…─╯` lines; `style` colours the border. Width-agnostic like [[RuleNode]]:
    * the width only enters at [[Layout.lay]]. */
  final case class BoxNode(inner: Element, style: Style) extends Element
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
  /** A layout memo over a subtree whose value no longer changes (e.g. a
    * finalised Markdown block): [[Layout.lay]] lays `inner` once and replays the
    * cached lines while the width is unchanged. This does not bend the
    * width-agnostic invariant above — no width is baked into the node's value;
    * the memo is keyed by the width `lay` was given, and a resize simply
    * recomputes. The cache only pays off when the same node instance is laid
    * across frames, so holders must reuse the node, not rebuild it. */
  final case class MemoNode(inner: Element, memo: LayMemo) extends Element

  /** A vertical cap over an inner element: when the inner lays taller than
    * `maxRows` rows, [[ClipFocus]] picks which rows survive and dim
    * `… N more lines` marker rows stand in for what was clipped. The markers
    * count against `maxRows`, so the laid height never exceeds it (floored at 3
    * rows — a marker sandwich needs at least one content row). `hint`, when
    * non-empty, is appended to the marker in parentheses (e.g. where to read the
    * full text). Width-agnostic like [[BoxNode]]: rows are counted as laid at
    * the width [[Layout.lay]] is given, so a resize re-clips from scratch. */
  final case class ClipNode(inner: Element, maxRows: Int, focus: ClipFocus, hint: String) extends Element

  /** A block of already-laid styled lines, returned verbatim by [[Layout.lay]]
    * regardless of the render width. This is the ONE deliberate exception to the
    * width-agnostic invariant above: it exists only for fullscreen frames, which
    * are rebuilt every frame from pre-laid, viewport-sliced lines and are never
    * cached across widths (a resize re-slices from scratch). Do not use it for
    * committed or inline content, whose whole point is to reflow on resize. */
  final case class RawLines(lines: Vector[StyledLine]) extends Element

/** The single-slot cache carried by an [[Element.MemoNode]]: the lines its
  * subtree laid to, at one width. Identity-ful by design — create one per
  * memoized subtree, alongside the node. Only the runtime's render step lays
  * elements (single-threaded), so plain vars. */
final class LayMemo:
  private[app] var laidWidth: Int = Int.MinValue
  private[app] var laid: Vector[StyledLine] = Vector.empty

/* ---- Top-level DSL (brought in by `import auk.tui.app.*`) ---- */

def Text(s: String): Element = Element.TextNode(s, Style.Default)
def layout(children: Element*): Element = Element.Stack(children.toVector)
val br: Element = Element.LineBreak
val Empty: Element = Element.Blank
def spinner(label: String, frame: Int): Element = Element.SpinnerNode(label, frame, Style.Default)
def hr(ch: Char = '─', color: Color = Color.Default): Element =
  Element.RuleNode(ch, if color == Color.Default then Style.Default else Style.fg(color))
/** A full-width rule with `label` centered in it, e.g. `──── Context compacted ────`. */
def labelledHr(label: String, style: Style = Style.Default): Element =
  Element.LabelledRuleNode(label, style)
def roundBox(inner: Element, color: Color = Color.Default): Element =
  Element.BoxNode(inner, if color == Color.Default then Style.Default else Style.fg(color))
/** Cap `inner` at `maxRows` laid rows, eliding per `focus` (markers included in
  * the count). See [[Element.ClipNode]]. */
def clipRows(inner: Element, maxRows: Int, focus: ClipFocus, hint: String = ""): Element =
  Element.ClipNode(inner, maxRows, focus, hint)
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
