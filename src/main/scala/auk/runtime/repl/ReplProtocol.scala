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

  /** Field extraction shared by [[parse]] and [[classify]], so the two can
    * never disagree about a response. */
  private def responseOf(op: String, d: js.Dynamic): Response =
    Response(
      op = op,
      ok = bool(d.ok),
      output = str(d.output).getOrElse(""),
      stdout = str(d.stdout).getOrElse(""),
      stderr = str(d.stderr).getOrElse(""),
      error = str(d.error),
      stateVersion = num(d.stateVersion).map(_.toInt).getOrElse(0)
    )

  private def str(v: js.Dynamic): Option[String] =
    if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None
  private def bool(v: js.Dynamic): Boolean =
    js.typeOf(v) == "boolean" && v.asInstanceOf[Boolean]
  private def num(v: js.Dynamic): Option[Double] =
    if js.typeOf(v) == "number" then Some(v.asInstanceOf[Double]) else None
