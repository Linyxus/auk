package auk.platform

/** UUID seam, replacing `java.util.UUID.randomUUID`. */
trait Uuid:
  def random(): String
