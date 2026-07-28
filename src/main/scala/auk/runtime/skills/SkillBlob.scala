package auk.runtime.skills

/** The skill set as ONE compilation unit: every skill's source concatenated
  * into a single REPL entry, so the compiler resolves inter-skill references
  * (in any order, mutual recursion included) with no load-ordering logic here.
  *
  * `build` records each skill's 1-based line span in the blob so a compile
  * diagnostic — which names blob line numbers — can be attributed back to the
  * skill(s) it came from.
  */
object SkillBlob:

  /** `id` occupies blob lines `startLine..endLine`, 1-based inclusive. */
  final case class Span(id: String, startLine: Int, endLine: Int)

  def build(skills: List[Skill]): (String, List[Span]) =
    val sb = new StringBuilder
    val spans = List.newBuilder[Span]
    var line = 1
    for skill <- skills do
      val code = skill.code.stripTrailing.nn
      val n = code.linesIterator.length
      spans += Span(skill.id, line, line + n - 1)
      if sb.nonEmpty then
        sb.append("\n\n")
      sb.append(code)
      line += n + 1 // the code's lines plus the blank separator line
    (sb.result(), spans.result())

  // No `(?m)` here: Scala.js' Pattern lacks MULTILINE, so gutter lines are
  // matched per line instead of with a multiline anchor.
  private val GutterLine = """\s*(\d+)\s*\|.*""".r
  private val PosRef = """:(\d+):\d+""".r

  /** The ids of the skills a compile diagnostic points into — best-effort, from
    * the `N |` gutter lines and `:line:col` positions dotty diagnostics carry.
    * Empty when nothing in the diagnostic maps into a span. */
  def culprits(diagnostic: String, spans: List[Span]): List[String] =
    val gutterRefs = diagnostic.linesIterator.toList.collect:
      case GutterLine(n) => n.nn.toIntOption
    val posRefs = PosRef.findAllMatchIn(diagnostic).map(m => m.group(1).nn.toIntOption).toList
    val lines = (gutterRefs ++ posRefs).flatten
    lines
      .flatMap(l => spans.find(s => l >= s.startLine && l <= s.endLine))
      .map(_.id)
      .distinct
