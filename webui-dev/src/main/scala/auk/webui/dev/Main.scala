package auk.webui.dev

import scala.scalajs.js

/** Entry point: `node main.js <rootDir> <port>`. Serves the static site from
  * `rootDir` and the mock SSE stream, for `./mill startTestWebUI`. */
@main def main(): Unit =
  val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]]
  val args = argv.toList.drop(2) // [node, script, <rootDir>, <port>]
  val rootDir = args.headOption.getOrElse(".")
  val port = args.drop(1).headOption.flatMap(_.toIntOption).getOrElse(8080)
  MockServer.start(
    rootDir,
    port,
    p => println(s"[webui-dev] serving $rootDir on http://localhost:$p  (scenarios: ${Scenarios.names.mkString(", ")})")
  )
  ()
