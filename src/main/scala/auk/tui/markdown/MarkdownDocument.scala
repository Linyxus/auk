package auk.tui.markdown

/** An incrementally-parsed Markdown document, fed an append-only growing prefix
  * (the [[auk.tui.Typewriter]]'s revealed text).
  *
  * The invariant that makes it cheap: a top-level block is only *final* once a
  * later block has begun, so [[feedTo]] finalises every block but the trailing
  * one and advances [[consumedChars]] past them. Finalised blocks are immutable
  * and shared across feeds — never re-parsed. Only the still-open tail (bounded
  * by the current block) is re-scanned, so total work is O(N) for the common
  * case of bounded blocks.
  *
  * (A single unbounded trailing container — e.g. a very long list with nothing
  * after it — re-parses per feed; this is the one documented exception, fine for
  * model output where such a container is followed by more text or the turn ends.)
  *
  * @param finalized     blocks proven final, in order.
  * @param consumedChars prefix length already turned into [[finalized]] (always a
  *                      line boundary).
  * @param source        the last prefix fed, kept so [[blocks]] can render the
  *                      open tail (including the partial last line).
  */
final case class MarkdownDocument(
    finalized: Vector[Block],
    consumedChars: Int,
    source: String
):
  /** Feed more arrived text. */
  def feed(delta: String): MarkdownDocument = feedTo(source + delta)

  /** Feed the full revealed prefix so far (must start with the previous one). */
  def feedTo(prefix: String): MarkdownDocument =
    // Append-only: equal length ⇒ unchanged content (cheap per-frame guard).
    if prefix.length == source.length then this
    else finalizeOpen(prefix)

  private def finalizeOpen(prefix: String): MarkdownDocument =
    val lastNl = prefix.lastIndexOf('\n')
    if lastNl < consumedChars then copy(source = prefix) // no new complete line
    else
      val region = prefix.substring(consumedChars, lastNl)
      val spans = BlockParser.parseSpans(MarkdownDocument.splitRegion(region))
      if spans.length < 2 then copy(source = prefix) // only the open block so far
      else
        val finalizeUpTo = spans(spans.length - 2).endLine
        val newFinal = spans.take(spans.length - 1).map(_.block)
        val advance = MarkdownDocument.offsetAfterLines(region, finalizeUpTo)
        MarkdownDocument(finalized ++ newFinal, consumedChars + advance, prefix)

  /** End of stream: finalise everything still open. */
  def close(): MarkdownDocument =
    if consumedChars >= source.length then this
    else
      val open = BlockParser.parseBlocks(MarkdownDocument.splitRegion(source.substring(consumedChars)))
      MarkdownDocument(finalized ++ open, source.length, source)

  /** A provisional parse of the still-open tail (which includes the partial last
    * line being typed); empty once everything is finalised. Unlike [[finalized]]
    * these blocks may change on the next feed — re-parsed per call. */
  def openBlocks: Vector[Block] =
    if consumedChars >= source.length then Vector.empty
    else BlockParser.parseBlocks(MarkdownDocument.splitRegion(source.substring(consumedChars)))

  /** All blocks for rendering: the finalised ones plus [[openBlocks]]. */
  def blocks: Vector[Block] =
    val open = openBlocks
    if open.isEmpty then finalized else finalized ++ open

  /** How many of [[blocks]] are finalised — everything before this index is
    * cooled and renders plain; only blocks at/after it can still be streaming. */
  def finalizedCount: Int = finalized.length

  /** Two documents are equal when they render the same blocks, regardless of how
    * far parsing has been incrementally finalised. This keeps a document fed
    * delta-by-delta equal to one parsed in a single shot — important for value
    * comparisons of the blocks that carry them. */
  override def equals(other: Any): Boolean = other match
    case that: MarkdownDocument => this.blocks == that.blocks
    case _                      => false

  override def hashCode: Int = blocks.hashCode

object MarkdownDocument:
  val empty: MarkdownDocument = MarkdownDocument(Vector.empty, 0, "")

  /** Parse a complete string in one shot (non-incremental convenience). */
  def parse(text: String): MarkdownDocument =
    MarkdownDocument(BlockParser.parse(text), text.length, text)

  /** Split a region that has no trailing newline into its lines, dropping a
    * stray carriage return so CRLF input behaves like LF. */
  private def splitRegion(region: String): Vector[String] =
    if region.isEmpty then Vector.empty
    else region.split("\n", -1).toVector.map(l => if l.endsWith("\r") then l.dropRight(1) else l)

  /** Character offset in `region` just past its first `lines` newline-terminated
    * lines (i.e. after the `lines`-th `\n`). */
  private def offsetAfterLines(region: String, lines: Int): Int =
    var seen = 0
    var k = 0
    while k < region.length && seen < lines do
      if region.charAt(k) == '\n' then seen += 1
      k += 1
    k
