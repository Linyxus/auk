package auk.library

import scala.scalajs.js

/** The [[AukInterface]] implementation preloaded into REPL sessions. */
object AukImpl extends AukInterface:
  def hello(name: String): String = s"Hello, $name!"

  def add(x: Int, y: Int): Int = x + y

  def cwd(): String =
    js.Dynamic.global.process.cwd().asInstanceOf[String]
