package auk.config

/** An error produced while parsing or decoding a config document.
  *
  * `path` records the location of the failure relative to the decoded root, as a
  * list of segments from outermost to innermost (record fields as `"name"`, list
  * elements as `"[i]"`) — the same convention as `auk.llm.tools.DecodeError`.
  * `line` is the 1-based source line of the offending value, when known.
  */
final case class ConfigError(
    message: String,
    path: List[String] = Nil,
    line: Option[Int] = None
):
  /** Prepend a path segment as a record decoder unwinds out of a nested value. */
  def prefix(segment: String): ConfigError = copy(path = segment :: path)

  /** Attach a source line. First write wins, so the line stamped closest to the
    * offending token (at the leaf) survives as records prepend their path.
    */
  def at(lineNo: Int): ConfigError =
    if line.isDefined then this else copy(line = Some(lineNo))

  /** A flat, readable rendering such as `line 7: model.coord.lat: expected number`. */
  def render: String =
    val loc = path.foldLeft("") { (acc, seg) =>
      if seg.startsWith("[") then acc + seg
      else if acc.isEmpty then seg
      else acc + "." + seg
    }
    val ln = line.fold("")(l => s"line $l: ")
    if loc.isEmpty then s"$ln$message" else s"$ln$loc: $message"

  override def toString: String = render
