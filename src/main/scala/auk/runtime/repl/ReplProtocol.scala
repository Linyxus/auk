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

  def evalRequest(code: String): String =
    js.JSON.stringify(js.Dynamic.literal(op = "eval", code = code))

  val shutdownRequest: String =
    js.JSON.stringify(js.Dynamic.literal(op = "shutdown"))

  def parse(line: String): Either[String, Response] =
    try
      val d = js.JSON.parse(line)
      str(d.op) match
        case None => Left(s"REPL response without an op: $line")
        case Some(op) =>
          Right(
            Response(
              op = op,
              ok = bool(d.ok),
              output = str(d.output).getOrElse(""),
              stdout = str(d.stdout).getOrElse(""),
              stderr = str(d.stderr).getOrElse(""),
              error = str(d.error),
              stateVersion = num(d.stateVersion).map(_.toInt).getOrElse(0)
            )
          )
    catch case _: Throwable => Left(s"unparseable REPL response: $line")

  /** Compiler diagnostics arrive colourised; strip the SGR sequences before
    * handing text to the model. */
  def stripAnsi(s: String): String =
    s.replaceAll("\u001b\\[[0-9;]*m", "").nn

  private def str(v: js.Dynamic): Option[String] =
    if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None
  private def bool(v: js.Dynamic): Boolean =
    js.typeOf(v) == "boolean" && v.asInstanceOf[Boolean]
  private def num(v: js.Dynamic): Option[Double] =
    if js.typeOf(v) == "number" then Some(v.asInstanceOf[Double]) else None
