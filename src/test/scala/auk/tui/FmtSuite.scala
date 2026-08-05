package auk.tui

/** Guards the hand-rolled one-decimal duration/token formatting against the
  * `java.util.Formatter` `%.1f` it replaced. The two agree on the Scala.js
  * target (its `%f` rounds half-up consistently, unlike the JVM's inexact-double
  * behaviour), which is the entire correctness risk of dropping the Formatter.
  * If this suite ever fails on a new target, the manual formula is no longer
  * byte-identical and the formatting change must be revisited. */
class FmtSuite extends munit.FunSuite:

  // The old expressions, replicated verbatim.
  private def oldDuration(ms: Long): String = f"${ms / 1000.0}%.1fs"
  private def oldTokens(n: Long): String = if n >= 1000 then f"${n / 1000.0}%.1fk" else n.toString
  private def oldThought(ms: Long): String = f"✻ Thought for ${ms / 1000.0}%.1fs"

  // The new manual formulas (mirroring ChatApp's private helpers).
  private def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"
  private def newDuration(ms: Long): String = s"${oneDecimal((ms + 50) / 100)}s"
  private def newTokens(n: Long): String = if n >= 1000 then s"${oneDecimal((n + 50) / 100)}k" else n.toString
  private def newThought(ms: Long): String = s"✻ Thought for ${oneDecimal((ms + 50) / 100)}s"

  test("duration formatting matches %.1f across 0..3,000,000 ms"):
    var ms = 0L
    while ms <= 3_000_000L do
      assertEquals(newDuration(ms), oldDuration(ms), s"ms=$ms")
      assertEquals(newThought(ms), oldThought(ms), s"ms=$ms")
      ms += 1L

  test("token formatting matches %.1f across 0..5,000,000"):
    var n = 0L
    while n <= 5_000_000L do
      assertEquals(newTokens(n), oldTokens(n), s"n=$n")
      n += 1L

  test("documented rounding boundaries"):
    assertEquals(newDuration(1050), "1.1s")
    assertEquals(newDuration(950), "1.0s")
    assertEquals(newTokens(1499), "1.5k")
    assertEquals(newTokens(999), "999")
    assertEquals(newTokens(1000), "1.0k")

  /** The TUI now formats durations through `EvalDisplay`, so that one call reads
    * the same here, in a sub-agent's transcript, and on the dashboard. The sweep
    * above is what makes the formula safe; this is what keeps it pointed at the
    * definition actually in use. */
  test("the shared definition is the formula this suite guards"):
    var ms = 0L
    while ms <= 200_000L do
      assertEquals(auk.workflow.EvalDisplay.duration(ms), newDuration(ms), s"ms=$ms")
      ms += 1L
    assertEquals(auk.workflow.EvalDisplay.oneDecimal(15L), oneDecimal(15L))
    assertEquals(auk.workflow.EvalDisplay.duration(3_000_000L), newDuration(3_000_000L))
