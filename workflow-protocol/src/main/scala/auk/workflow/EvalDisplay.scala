package auk.workflow

/** How a running `eval_scala` call reads on a row, wherever the row is drawn:
  * the lead's own transcript in the TUI, a sub-agent's transcript beside it, and
  * the browser dashboard. One definition because the three are describing the
  * same call and any drift between them would be a difference the reader has no
  * way to explain.
  *
  * The vocabulary is deliberately small. A stage this build has no word for is
  * shown as no stage at all: the tokens are the REPL worker's free choice, a
  * newer worker may name stages this build never heard of, and passing one
  * through unread would put a word on the row that nothing here can vouch for.
  * Stages that look the same from outside read the same, too — `rendering` is
  * executing code just as `running` is, and a row that flipped between them
  * would be reporting the worker's internal structure rather than what it is
  * doing.
  *
  * Nothing here calls a worker stuck. The worker's event loop does not yield
  * during a compile or a run, so a healthy long computation and a wedged one
  * look identical from outside, and the honest rendering of both is a clock that
  * does not stop.
  */
object EvalDisplay:

  /** The tool whose rows this object describes. */
  val ToolName = "eval_scala"

  // -- the metadata a progress report carries ---------------------------------
  // These key strings are the wire contract with the REPL worker: `ScalaRepl`
  // publishes them and every renderer reads them, so they are duplicated
  // nowhere else — the names below are its own, so that side reads as a
  // re-export rather than a translation.

  /** The worker's liveness state. */
  val MetaState = "replState"

  /** How long the worker has been silent. */
  val MetaSilentMs = "replSilentMs"

  /** How long the whole request has been out. */
  val MetaElapsedMs = "replElapsedMs"

  /** The worker process's pid, when it has one. */
  val MetaWorkerPid = "workerPid"

  /** The liveness picture as a sentence. */
  val MetaLiveness = "liveness"

  /** The pipeline stage the worker last named, when it named one. */
  val MetaPhase = "replPhase"

  /** How long that stage has been the current one — a clock of its own, started
    * when the stage was entered rather than when the request was sent. */
  val MetaPhaseMs = "replPhaseMs"

  // -- reading a report -------------------------------------------------------

  /** The worker's current stage in the reader's vocabulary, or `None` when it
    * named none or named one this build cannot vouch for. */
  def stageName(progress: Map[String, String]): Option[String] =
    progress.get(MetaPhase).collect:
      case "compiling"             => "Compiling"
      case "running" | "rendering" => "Running"

  /** How long the current stage has been running, extrapolated to `now` from the
    * report that stated it.
    *
    * The extrapolation is the point. The worker is sampled about once a second
    * while a row redraws ten times as often, so the reported figure alone would
    * sit still for ten frames and then jump — and with a tenth of a second on
    * show, the frozen digit would be the sampling offset rather than any fact
    * about the stage. Counting the time since the report arrived on top of it
    * makes the clock advance at the rate it appears to advance.
    *
    * A report can only be extrapolated forward: with no anchor, or with a clock
    * that somehow reads behind the report, the reported figure is the honest
    * one. `None` when the report named no stage clock at all. */
  def stageMs(progress: Map[String, String], progressAtMs: Option[Long], now: Long): Option[Long] =
    progress.get(MetaPhaseMs).flatMap(_.toLongOption).map(extrapolate(_, progressAtMs, now))

  /** How long the whole call has been out, extrapolated to `now` by exactly the
    * arithmetic [[stageMs]] uses on the stage clock.
    *
    * This is the total for a viewer that never saw the call begin. The lead's own
    * transcript stamps a call when it starts and counts from that stamp; a
    * transcript arriving over a wire has no such stamp, so the only total on offer
    * is the one the worker reported about itself. `None` until the first report
    * lands — a row then says nothing about a total rather than inventing one, and
    * reads exactly as it did before this existed for the second or so that takes. */
  def elapsedMs(progress: Map[String, String], progressAtMs: Option[Long], now: Long): Option[Long] =
    progress.get(MetaElapsedMs).flatMap(_.toLongOption).map(extrapolate(_, progressAtMs, now))

  /** A counter the worker sampled, read at `now`: the reported figure plus however
    * long ago it was reported. Both clocks on a row extrapolate this way, so they
    * advance together and neither can drift against the other. */
  private def extrapolate(reported: Long, progressAtMs: Option[Long], now: Long): Long =
    reported + progressAtMs.fold(0L)(at => math.max(0L, now - at))

  /** The stage and its clock, e.g. `"Compiling (3.1s)"` — the stage alone when
    * the report carried no clock for it. */
  def stage(progress: Map[String, String], progressAtMs: Option[Long], now: Long): Option[String] =
    stageName(progress).map: name =>
      stageMs(progress, progressAtMs, now).fold(name)(ms => s"$name (${duration(ms)})")

  /** [[stage]] as the segment a renderer appends after a row's other text, e.g.
    * `" · Compiling (3.1s)"`. `None` — rather than an empty string — so a caller
    * concatenating segments never has to ask whether one is really there. */
  def stageSuffix(progress: Map[String, String], progressAtMs: Option[Long], now: Long): Option[String] =
    stage(progress, progressAtMs, now).map(s => s" · $s")

  /** The whole annotation a live eval row carries after its label, e.g.
    * `" (12.4s) · Compiling (3.1s)"`: how long the call has been going, then how
    * long it has been doing the one thing it is doing now.
    *
    * Two clocks rather than one because they answer different questions — a
    * minute of compiling and a minute of running are the same total and very
    * different situations — and a stage whose number keeps climbing is all the
    * report a stuck eval needs. */
  def liveSuffix(elapsedMs: Long, progress: Map[String, String], progressAtMs: Option[Long], now: Long): String =
    s" (${duration(elapsedMs)})" + stageSuffix(progress, progressAtMs, now).getOrElse("")

  /** [[liveSuffix]] for a viewer whose only source is the report itself — the row
    * in a transcript that arrived over a wire, where nothing local timed the call.
    * Both clocks then come from the same report and are extrapolated together.
    *
    * `""` — the whole annotation, not just a segment — while the report says
    * neither, so a row that has been told nothing reads exactly as it would if
    * this were never called. A report that named a stage but no total shows the
    * stage alone: half the picture is not a reason to withhold the half there is. */
  def reportedSuffix(progress: Map[String, String], progressAtMs: Option[Long], now: Long): String =
    elapsedMs(progress, progressAtMs, now) match
      case Some(ms) => liveSuffix(ms, progress, progressAtMs, now)
      case None     => stageSuffix(progress, progressAtMs, now).getOrElse("")

  // -- formatting -------------------------------------------------------------

  /** A duration as one decimal of a second, e.g. `"3.1s"`. */
  def duration(ms: Long): String = s"${oneDecimal((ms + 50) / 100)}s"

  /** Round-half-up to one decimal, hand-rolled: `java.util.Formatter` is slow
    * under Scala.js and this is on a redraw path. `scaled` is the value in
    * tenths; for the non-negative quantities shown here `(x + 50) / 100` is
    * round-half-up to tenths, byte-identical to `%.1f` on the Scala.js target
    * (which the TUI's `FmtSuite` verifies over the full range). */
  def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"
