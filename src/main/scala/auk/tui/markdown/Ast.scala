package auk.tui.markdown

/** The GitHub-Flavored-Markdown abstract syntax tree.
  *
  * Two layers — [[Block]] (structure) and [[Inline]] (runs of text and their
  * styling) — plus [[Align]] for table columns. Pure data with no dependency on
  * the view layer, so the parser can live below the TUI and the model can hold a
  * parsed document without pulling in rendering.
  *
  * The grammar is a deliberate GFM subset (see [[BlockParser]] / [[InlineParser]]
  * for what is and isn't recognised); unrecognised constructs degrade to literal
  * text rather than failing.
  */

/** Column alignment in a GFM table, from the delimiter row's colons. */
enum Align:
  case None, Left, Center, Right

/** An inline node: a run of text or a styled span within one block. */
enum Inline:
  /** Literal text (escapes already resolved, entities left as written). */
  case Text(value: String)
  case Emph(content: Vector[Inline])
  case Strong(content: Vector[Inline])
  case Strikethrough(content: Vector[Inline])
  /** A code span — verbatim, never further parsed. */
  case Code(value: String)
  /** `[content](dest "title")`. */
  case Link(content: Vector[Inline], dest: String, title: Option[String])
  /** `![alt](dest "title")`. `alt` is plain text (images have no inline target). */
  case Image(alt: String, dest: String, title: Option[String])
  /** `<uri>` / `<email>` and GFM bare autolinks. */
  case Autolink(uri: String, isEmail: Boolean)
  /** A line break forced by two trailing spaces or a trailing backslash. */
  case HardBreak
  /** A newline within a paragraph (rendered as a space / soft wrap). */
  case SoftBreak

/** A block-level node: a structural unit of the document. */
enum Block:
  /** ATX (`# h`) or setext (`h\n===`) heading; `level` in 1..6. */
  case Heading(level: Int, content: Vector[Inline])
  case Paragraph(content: Vector[Inline])
  /** A fenced or indented code block. `lang` is the first word of the info
    * string; `info` is the whole info string; `code` keeps original newlines. */
  case Code(info: Option[String], lang: Option[String], code: String, fenced: Boolean)
  case Quote(blocks: Vector[Block])
  /** An ordered or bullet list. `start` is the first ordinal (ordered only).
    * `tight` controls whether items render with paragraph spacing. */
  case ListBlock(ordered: Boolean, start: Int, tight: Boolean, items: Vector[ListItem])
  case Table(align: Vector[Align], header: Vector[Vector[Inline]], rows: Vector[Vector[Vector[Inline]]])
  case ThematicBreak

/** One item of a [[Block.ListBlock]]. `task` is `Some(checked)` for a GFM task
  * list item (`- [ ]` / `- [x]`), `None` otherwise. */
final case class ListItem(blocks: Vector[Block], task: Option[Boolean])
