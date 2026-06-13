package auk.library

import scala.scalajs.js

/** A file-system path, backed by Node's `path` module.
  *
  * Pure value: constructing one never touches the file system, so a `PathImpl`
  * may name something that does not exist. `/` joins (and normalizes via
  * `path.join`, collapsing `.`/`..`); equality and `toString` are by the path
  * string, so paths render readably in the REPL and compare by value.
  */
final class PathImpl(val raw: String) extends Path:
  def / (sub: String): Path = PathImpl(PathImpl.node.join(raw, sub).asInstanceOf[String])

  override def toString: String = raw

  override def equals(other: Any): Boolean = other match
    case that: PathImpl => that.raw == raw
    case _              => false

  override def hashCode: Int = raw.hashCode

private object PathImpl:
  def node: js.Dynamic = js.Dynamic.global.require("node:path")

/** The [[AukInterface]] implementation preloaded into REPL sessions.
  *
  * A class, not an object: the session preamble (see
  * `auk.runtime.repl.ReplPreamble`) creates the instance, so construction can
  * later carry session-specific state — working directory, policy, handles —
  * without changing evaluated code.
  */
final class AukImpl extends AukInterface:
  def cwd: Path = PathImpl(js.Dynamic.global.process.cwd().asInstanceOf[String])

  def Path(p: String): Path = PathImpl(p)
