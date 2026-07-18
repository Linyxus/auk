package auk.runtime.mcp

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.llm.tools.Json
import auk.platform.js.{LineProcess, McpHelper}
import auk.runtime.McpBridge

/** End-to-end check of the DEV `--mcp-call` helper: a driver process `spawnSync`s
  * the REAL helper .mjs, which connects the REAL [[McpBridge]] over a UDS, backed
  * by a scripted [[McpHub]]. The driver is async-spawned so the harness event loop
  * stays free to service the bridge (a single-process spawnSync would deadlock —
  * the helper's request can't be answered while the caller blocks). This is the
  * faithful worker→helper→host→worker path, including the >64KB flush-before-exit
  * lesson.
  *
  * Gated behind `AUK_MCP_E2E` (it spawns node children), so the default
  * `sbt test` stays child-process-free; run with `AUK_MCP_E2E=1 sbt "testOnly
  * auk.runtime.mcp.McpHelperE2ESuite"`. */
class McpHelperE2ESuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  private def e2eEnabled: Boolean =
    val v = js.Dynamic.global.process.env.AUK_MCP_E2E
    !js.isUndefined(v) && v != null && v.asInstanceOf[String].nonEmpty

  private val initResult: Json = Json.Obj(
    List("protocolVersion" -> Json.Str("2025-06-18"), "capabilities" -> Json.Obj(Nil), "serverInfo" -> Json.Obj(Nil))
  )

  private def makeHub(callText: String): McpHub =
    val script = Map(
      "initialize" -> Reply.Result(initResult),
      "tools/call" -> Reply.Result(
        Json.Obj(
          List(
            "content" -> Json.Arr(List(Json.Obj(List("type" -> Json.Str("text"), "text" -> Json.Str(callText))))),
            "isError" -> Json.Bool(false)
          )
        )
      )
    )
    McpHub(
      List(McpServerConfig("stub", "unused", Nil, Map.empty)),
      transportFactory = _ => ((onLine, onExit) => new ScriptedTransport(script, _ => (), onLine, onExit))
    )

  private def tmp(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-mcp-e2e-${js.Dynamic.global.process.pid}-$name").asInstanceOf[String]

  private def writeFile(path: String, content: String): Unit =
    val fs = js.Dynamic.global.require("node:fs")
    fs.writeFileSync(path, content, "utf8")
    ()

  // The driver spawnSyncs the helper (env-driven), the closest analog to the
  // worker's Phase-3 McpImpl. It inherits AUK_MCP_SOCK to the helper via env.
  private val DriverSource =
    """import { spawnSync } from "node:child_process";
      |const r = spawnSync(process.execPath, [process.env.MCP_HELPER], {
      |  input: process.env.MCP_REQUEST + "\n",
      |  env: process.env,
      |  encoding: "utf8",
      |  maxBuffer: 67108864
      |});
      |process.stdout.write("EXIT:" + r.status + "\n");
      |if (r.stdout) process.stdout.write(r.stdout);
      |""".stripMargin

  private def start(bridge: McpBridge)(using Async.Spawn): Unit =
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await

  /** Async-spawn the driver and collect its full stdout (joined by newlines). */
  private def runDriver(driverPath: String, helperPath: String, request: String, sock: String)(using Async): String =
    val execPath = js.Dynamic.global.process.execPath.asInstanceOf[String]
    val lines = scala.collection.mutable.ListBuffer.empty[String]
    val exited = Future.Promise[Int]()
    LineProcess.spawn(
      argv = List(execPath, driverPath),
      extraEnv = Map("MCP_HELPER" -> helperPath, "MCP_REQUEST" -> request, "AUK_MCP_SOCK" -> sock),
      onLine = l => { lines += l; () },
      onStderr = _ => (),
      onExit = code => { exited.complete(Success(code)); () }
    )
    exited.asFuture.await
    lines.mkString("\n")

  private def callRequest: String =
    Json
      .Obj(
        List(
          "id" -> Json.num(1),
          "op" -> Json.Str("callTool"),
          "server" -> Json.Str("stub"),
          "tool" -> Json.Str("echo"),
          "args" -> Json.Obj(Nil)
        )
      )
      .render

  test("small round-trip: driver spawnSyncs helper -> bridge -> scripted hub"):
    assume(e2eEnabled, "set AUK_MCP_E2E=1 to run the MCP dev-helper end-to-end harness")
    Async.fromSync:
      val bridge = McpBridge(tmp("small.sock"), makeHub("pong"))
      start(bridge)
      val helperPath = McpHelper.writeDevHelper()
      val driverPath = tmp("small-driver.mjs")
      writeFile(driverPath, DriverSource)
      try
        val out = runDriver(driverPath, helperPath, callRequest, bridge.socketPath)
        assert(out.contains("EXIT:0"), out)
        assert(out.contains("\"ok\":true"), out)
        assert(out.contains("\"text\":\"pong\""), out)
      finally Async.fromSync(bridge.close())

  test("large payload survives: flush-before-exit is not truncated past 64KB"):
    assume(e2eEnabled, "set AUK_MCP_E2E=1 to run the MCP dev-helper end-to-end harness")
    Async.fromSync:
      val big = "x" * 200000 + "END"
      val bridge = McpBridge(tmp("big.sock"), makeHub(big))
      start(bridge)
      val helperPath = McpHelper.writeDevHelper()
      val driverPath = tmp("big-driver.mjs")
      writeFile(driverPath, DriverSource)
      try
        val out = runDriver(driverPath, helperPath, callRequest, bridge.socketPath)
        assert(out.contains("EXIT:0"), out.take(200))
        // The tail marker only survives if the whole >200KB line was flushed.
        assert(out.contains("xEND"), s"payload truncated; length=${out.length}")
      finally Async.fromSync(bridge.close())
