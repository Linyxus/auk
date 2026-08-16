package auk.runtime.repl

import scala.scalajs.js

/** The wire format of the scala3-js JSONL REPL worker.
  *
  * One JSON object per line in each direction, answered strictly in request
  * order. Requests carry an `op` (`eval` | `reset` | `shutdown`); every
  * response echoes the op plus `ok`, the REPL's rendering of the line
  * (`output`, e.g. `val xs: List[Int] = …`), the user code's captured I/O
  * (`stdout`, `stderr`), the session `stateVersion`, and `error` on failure.
  * A response with op `protocol` reports a malformed request — or, sent
  * unsolicited at startup, a worker that could not boot.
  *
  * A v2 worker additionally emits unsolicited liveness ops — `hello` at boot,
  * `received` when it dequeues a request, `tick` while its event loop is free
  * to send one, `progress` as the eval moves through its pipeline. None of them
  * settles the in-flight request, so incoming lines must be classified with
  * [[classify]] rather than taken as responses.
  */
object ReplProtocol:
  final case class Response(
      op: String,
      ok: Boolean,
      output: String,
      stdout: String,
      stderr: String,
      error: Option[String],
      stateVersion: Int
  )

  /** One classified line from the worker's stdout. */
  enum Line:
    /** A response that settles the in-flight request (ops `eval` | `reset` |
      * `shutdown` | `protocol`). */
    case Reply(response: Response)

    /** Boot announcement: the worker speaks liveness protocol `protocol` and
      * ticks every `tickMs` while busy. Fields are optional on the wire so a
      * newer worker can drop them; the defaults describe protocol 2. */
    case Hello(protocol: Int, tickMs: Int, pid: Option[Int])

    /** The worker dequeued the in-flight request and is about to evaluate it. */
    case Received

    /** Periodic heartbeat while a request is outstanding. A tick can only be
      * sent by an event loop that is free to run the timer, so a steady stream
      * of them says the worker is WAITING on something rather than computing.
      * `seq` is log-only. */
    case Tick(seq: Int)

    /** The worker reached a new stage of the eval pipeline. `phase` is opaque
      * display text the worker chooses (`compiling`, `running`, `rendering`, …),
      * never matched on here, so a worker may name stages this parent has never
      * heard of. Like a tick it is also proof the event loop turned. */
    case Progress(phase: String)

    /** A parseable JSON object whose op is none of the above. Dropped rather
      * than rejected, so a newer worker's ops do not break an older parent. */
    case Unknown(op: String)

  def evalRequest(code: String): String =
    js.JSON.stringify(js.Dynamic.literal(op = "eval", code = code))

  val shutdownRequest: String =
    js.JSON.stringify(js.Dynamic.literal(op = "shutdown"))

  def parse(line: String): Either[String, Response] =
    try
      val d = js.JSON.parse(line)
      str(d.op) match
        case None     => Left(s"REPL response without an op: $line")
        case Some(op) => Right(responseOf(op, d))
    catch case _: Throwable => Left(s"unparseable REPL response: $line")

  /** Classify one line of worker stdout. Only [[Line.Reply]] may settle the
    * in-flight request; the liveness ops and unrecognised ops carry no result.
    * Left means the line is not a JSON object with a string `op` — worker noise
    * (a JVM crash dump, a stray print) rather than a protocol message. */
  def classify(line: String): Either[String, Line] =
    try
      val d = js.JSON.parse(line)
      str(d.op) match
        case None => Left(s"REPL response without an op: $line")
        case Some(op) =>
          Right(op match
            case "eval" | "reset" | "shutdown" | "protocol" => Line.Reply(responseOf(op, d))
            case "hello" =>
              Line.Hello(
                protocol = num(d.protocol).map(_.toInt).getOrElse(2),
                tickMs = num(d.tickMs).map(_.toInt).getOrElse(1000),
                pid = num(d.pid).map(_.toInt)
              )
            case "received" => Line.Received
            case "tick"     => Line.Tick(num(d.seq).map(_.toInt).getOrElse(0))
            // A progress line is only worth anything for what it names, so one
            // that names nothing is an op we cannot read rather than an error:
            // same treatment as a newer worker's unrecognised op.
            case "progress" => str(d.phase).fold(Line.Unknown("progress"))(Line.Progress.apply)
            case other      => Line.Unknown(other)
          )
    catch case _: Throwable => Left(s"unparseable REPL response: $line")

  /** Compiler diagnostics arrive colourised; strip the SGR sequences before
    * handing text to the model. */
  def stripAnsi(s: String): String =
    s.replaceAll("\u001b\\[[0-9;]*m", "").nn

  /** How much of one response field this parent will hold.
    *
    * The eval tool truncates to 100 KB before the model sees anything, so past
    * that these characters exist only to be copied: `mkString`, `stripAnsi` and
    * `stripMarkers` each rebuild the whole text, and every copy is live at once.
    * On 2026-08-15 a worker sent a 40 MB line — a grep that had matched its own
    * logs under `.auk` — and this parent ran out of heap carrying it. A megabyte
    * leaves a wide margin over what any reader wants and a narrow one over what
    * the process can afford.
    */
  val MaxFieldChars: Int = 1_048_576

  /** How much of an over-long field's END is kept. The tail is where a stack
    * trace lands — and where the loop's `auk:loop:check:` verdict marker is
    * written, which a head-only cut would silently swallow, turning a checker
    * that worked into "the checker printed no result marker".
    */
  val TailChars: Int = 8_192

  /** `s` bounded to [[MaxFieldChars]], keeping its head and its last
    * [[TailChars]] characters and saying on its own line how much went missing.
    * Loud by construction: a reader that cannot tell 10 KB of loss from 15 MB
    * cannot know to ask a narrower question.
    */
  def clipField(s: String): String =
    if s.length <= MaxFieldChars then s
    else
      val note = s"\n…[$MaxFieldChars of ${s.length} characters kept; middle dropped]\n"
      // The tail and the note come out of the same budget as the head, so the
      // result is exactly MaxFieldChars long — and `head` cannot go negative
      // however the constants are retuned.
      val head = math.max(0, MaxFieldChars - TailChars - note.length)
      val tail = math.min(TailChars, s.length)
      s"${s.substring(0, head)}$note${s.substring(s.length - tail)}"

  /** Field extraction shared by [[parse]] and [[classify]], so the two can
    * never disagree about a response. The text fields are bounded here — the one
    * place both paths pass through, and before any consumer can copy them. */
  private def responseOf(op: String, d: js.Dynamic): Response =
    Response(
      op = op,
      ok = bool(d.ok),
      output = str(d.output).map(clipField).getOrElse(""),
      stdout = str(d.stdout).map(clipField).getOrElse(""),
      stderr = str(d.stderr).map(clipField).getOrElse(""),
      error = str(d.error).map(clipField),
      stateVersion = num(d.stateVersion).map(_.toInt).getOrElse(0)
    )

  private def str(v: js.Dynamic): Option[String] =
    if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None
  private def bool(v: js.Dynamic): Boolean =
    js.typeOf(v) == "boolean" && v.asInstanceOf[Boolean]
  private def num(v: js.Dynamic): Option[Double] =
    if js.typeOf(v) == "number" then Some(v.asInstanceOf[Double]) else None
