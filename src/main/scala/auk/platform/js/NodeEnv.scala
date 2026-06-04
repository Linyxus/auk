package auk.platform.js

import auk.platform.Env

/** [[Env]] backed by the global `process.env`. */
object NodeEnv extends Env:
  def get(name: String): Option[String] =
    GlobalProcess.env.get(name).filter(_ != null)
