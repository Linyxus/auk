package auk.llm.tools

/** An error produced while decoding [[Json]] into a typed value.
  *
  * `path` records the location of the failure relative to the decoded root, as
  * a list of segments from outermost to innermost (object fields as `"name"`,
  * array elements as `"[i]"`).
  */
final case class DecodeError(message: String, path: List[String] = Nil):
  /** Prepend a path segment as the decoder unwinds out of a nested value. */
  def prefix(segment: String): DecodeError = copy(path = segment :: path)

  /** A flat, readable rendering such as `address.[0].zip: expected string`. */
  def render: String =
    if path.isEmpty then message
    else
      val loc = path.foldLeft("") { (acc, seg) =>
        if seg.startsWith("[") then acc + seg
        else if acc.isEmpty then seg
        else acc + "." + seg
      }
      s"$loc: $message"

  override def toString: String = render
