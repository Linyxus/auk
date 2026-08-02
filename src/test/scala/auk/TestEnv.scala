package auk

import scala.scalajs.js

/** Process-env manipulation for tests. `Platform.env` reads `process.env`
  * live, so forcing entries here is the seam for exercising key-presence and
  * HOME-relative paths without touching the developer's real environment.
  */
object TestEnv:
  /** Run `body` with the given entries forced (`Some` sets, `None` deletes),
    * restoring the originals after — whatever `body` does. */
  def withEnv[A](pairs: (String, Option[String])*)(body: => A): A =
    val env = js.Dynamic.global.process.env
    val saved = pairs.map((name, _) => name -> env.selectDynamic(name))
    def put(name: String, value: Option[String]): Unit = value match
      case Some(v) => env.updateDynamic(name)(v)
      case None    => js.special.delete(env, name)
    pairs.foreach((name, value) => put(name, value))
    try body
    finally
      saved.foreach((name, value) =>
        if js.isUndefined(value) then js.special.delete(env, name)
        else env.updateDynamic(name)(value)
      )
