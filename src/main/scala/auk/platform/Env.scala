package auk.platform

/** Environment-variable seam, replacing `sys.env`. */
trait Env:
  def get(name: String): Option[String]
