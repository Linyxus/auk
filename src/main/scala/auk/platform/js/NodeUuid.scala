package auk.platform.js

import auk.platform.Uuid

/** [[Uuid]] backed by `node:crypto` `randomUUID`. */
object NodeUuid extends Uuid:
  def random(): String = nodeRandomUUID()
