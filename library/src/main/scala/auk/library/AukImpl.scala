package auk.library

import scala.scalajs.js

/** The [[AukInterface]] implementation preloaded into REPL sessions.
  *
  * A class, not an object: the session preamble (see
  * `auk.runtime.repl.ReplPreamble`) creates the instance, so construction can
  * later carry session-specific state — working directory, policy, handles —
  * without changing evaluated code.
  */
final class AukImpl extends AukInterface:
  def hello(name: String): String = s"Hello, $name!"

  def add(x: Int, y: Int): Int = x + y

  def cwd(): String =
    js.Dynamic.global.process.cwd().asInstanceOf[String]
