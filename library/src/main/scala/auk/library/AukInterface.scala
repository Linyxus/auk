package auk.library

/** A path in the file system. */
trait Path:
  def / (sub: String): Path

/** The runtime interface for Auk agents. */
trait AukInterface:
  def cwd: Path
  def Path(p: String): Path
