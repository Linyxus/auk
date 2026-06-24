package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future}
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl

/** Phase-1 worker-DSL test against a FAKE host (no real bridge, no LLM): an
  * in-test UDS server that echoes each `call` with `done:<prompt>`, records the
  * worker's protocol messages, and captures the final `done`. Verifies the worker
  * DSL (group/inGroup/agent/Agent.all/flatMap) computes and reports the right
  * result over the real side channel. The eval returns immediately (a pending
  * Future); the host-side await is exercised by WorkflowBridgeSuite instead.
  */
class WorkflowDslSuite extends munit.FunSuite:

  // Real REPL worker per test; under the full parallel `sbt test` run, worker
  // startup contends with the other worker-backed suites. Match their generous
  // budget rather than a tight 90s.
  override def munitTimeout: Duration = 240.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-wf-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  /** Fake host: echoes calls, records every message, and completes `done` with
    * the workflow's reported result value. */
  private def startFakeHost(sockPath: String)(using
      Async
  ): (js.Dynamic, scala.collection.mutable.ListBuffer[String], Future.Promise[String]) =
    val net = js.Dynamic.global.require("node:net")
    val fs = js.Dynamic.global.require("node:fs")
    try fs.unlinkSync(sockPath) catch case _: Throwable => ()
    val recorded = scala.collection.mutable.ListBuffer.empty[String]
    val donePromise = Future.Promise[String]()
    val server = net.createServer(((conn: js.Dynamic) =>
      conn.setEncoding("utf8")
      var buf = ""
      conn.on("data", ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then
            recorded += line
            val msg = js.JSON.parse(line)
            msg.t.asInstanceOf[String] match
              case "call" =>
                val reply = js.Dynamic.literal(
                  t = "result", id = msg.id, ok = true, value = "done:" + msg.prompt.asInstanceOf[String])
                conn.write(js.JSON.stringify(reply) + "\n")
              case "done" =>
                try donePromise.complete(Success(msg.value.asInstanceOf[String])) catch case _: Throwable => ()
              case _ => ()
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit])
      ()
    ): js.Function1[js.Dynamic, Unit]).asInstanceOf[js.Dynamic]
    val listening = Future.Promise[Unit]()
    server.listen(sockPath, (() => { listening.complete(Success(())); () }): js.Function0[Unit])
    listening.asFuture.await
    (server, recorded, donePromise)

  private def replWith(sockPath: String): ScalaRepl =
    ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> sockPath))))

  /** Fake host that pauses the run: on the first `call`, it signals `paused` (the
    * host→worker pause message) and drops the connection instead of replying —
    * mirroring what `WorkflowBridge.handlePause` does. Completes `pausedSent` once
    * the signal is out, so the test can then read the handle's status. */
  private def startPausingHost(sockPath: String)(using
      Async
  ): (js.Dynamic, Future.Promise[Unit]) =
    val net = js.Dynamic.global.require("node:net")
    val fs = js.Dynamic.global.require("node:fs")
    try fs.unlinkSync(sockPath) catch case _: Throwable => ()
    val pausedSent = Future.Promise[Unit]()
    val server = net.createServer(((conn: js.Dynamic) =>
      conn.setEncoding("utf8")
      var buf = ""
      conn.on("data", ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then
            val msg = js.JSON.parse(line)
            if msg.t.asInstanceOf[String] == "call" then
              // Signal paused, then drop the connection (in-order: the line arrives
              // before the FIN, so the worker marks paused before the close).
              conn.write(js.JSON.stringify(js.Dynamic.literal(t = "paused", run = msg.run)) + "\n")
              conn.end()
              try pausedSent.complete(Success(())) catch case _: Throwable => ()
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit])
      ()
    ): js.Function1[js.Dynamic, Unit]).asInstanceOf[js.Dynamic]
    val listening = Future.Promise[Unit]()
    server.listen(sockPath, (() => { listening.complete(Success(())); () }): js.Function0[Unit])
    listening.asFuture.await
    (server, pausedSent)

  test("a grouped workflow drives the side channel and reports the joined result"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val sockPath = tmpSock("dsl")
      val (server, recorded, done) = startFakeHost(sockPath)
      val repl = replWith(sockPath)
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        val tool = EvalScala(repl)
        val code =
          """wf.start[String]:
            |  val g = group("hunt", "find things")
            |  val a = inGroup(g) { agent[String]("task A", id = "a") }
            |  val b = inGroup(g) { agent[String]("task B", id = "b") }
            |  Agent.all(List(a, b)).flatMap(rs => agent[String]("summary of " + rs.mkString(","), id = "sum"))""".stripMargin
        tool.execute(EvalScalaParams(code, Some(30_000)))
        val doneValue = done.asFuture.await
        println(s"[WF] done=$doneValue\n--- recorded ---\n${recorded.mkString("\n")}")
        // The worker joined the agent results and reported the composed summary.
        assert(doneValue.contains("summary of done:task A,done:task B"), doneValue)
        // Structure crossed the wire: a group, three calls, and `sum`'s deps.
        assert(recorded.exists(_.contains("\"t\":\"group\"")), recorded.mkString("\n"))
        assert(recorded.count(_.contains("\"t\":\"call\"")) == 3, recorded.mkString("\n"))
        assert(
          recorded.exists(l => l.contains("\"t\":\"node\"") && l.contains("\"id\":\"sum\"") && l.contains("\"deps\":[")),
          recorded.mkString("\n")
        )
      finally
        Async.fromSync(repl.close())
        try server.close() catch case _: Throwable => ()

  test("a duplicate agent id fails the workflow build hard, not silently"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val sockPath = tmpSock("dup")
      val (server, _, _) = startFakeHost(sockPath)
      val repl = replWith(sockPath)
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        val tool = EvalScala(repl)
        // Both agents claim id "dup". The second `agent(...)` must throw during
        // the synchronous build, before the start marker, so eval_scala renders
        // the error rather than awaiting a `done` that never comes.
        val code =
          """wf.start[String]:
            |  val a = agent[String]("task A", id = "dup")
            |  val b = agent[String]("task B", id = "dup")
            |  Agent.all(List(a, b)).map(_.mkString(","))""".stripMargin
        val r = tool.execute(EvalScalaParams(code, Some(30_000)))
        assert(r.isError, s"expected an error, got: ${r.output}")
        assert(r.output.contains("duplicate agent id 'dup'"), r.output)
      finally
        Async.fromSync(repl.close())
        try server.close() catch case _: Throwable => ()

  test("Agent.pure lifts a value into the graph with no sub-agent call"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val sockPath = tmpSock("pure-after")
      val (server, recorded, done) = startFakeHost(sockPath)
      val repl = replWith(sockPath)
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        val tool = EvalScala(repl)
        val code =
          """wf.start[String]:
            |  agent[String]("task A", id = "a").flatMap(r => Agent.pure(r + "!"))""".stripMargin
        tool.execute(EvalScalaParams(code, Some(30_000)))
        val doneValue = done.asFuture.await
        assert(doneValue.contains("done:task A!"), doneValue)
        // Only the real agent "a" produced a call and a node; pure adds neither.
        assertEquals(recorded.count(_.contains("\"t\":\"call\"")), 1, recorded.mkString("\n"))
        assertEquals(recorded.count(_.contains("\"t\":\"node\"")), 1, recorded.mkString("\n"))
      finally
        Async.fromSync(repl.close())
        try server.close() catch case _: Throwable => ()

  test("a workflow can return a pure value with no sub-agents at all"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val sockPath = tmpSock("pure-only")
      val (server, recorded, done) = startFakeHost(sockPath)
      val repl = replWith(sockPath)
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        val tool = EvalScala(repl)
        val code = """wf.start[String](Agent.pure("just this"))"""
        tool.execute(EvalScalaParams(code, Some(30_000)))
        val doneValue = done.asFuture.await
        assertEquals(doneValue, "just this", doneValue)
        assertEquals(recorded.count(_.contains("\"t\":\"call\"")), 0, recorded.mkString("\n"))
      finally
        Async.fromSync(repl.close())
        try server.close() catch case _: Throwable => ()

  test("a paused run's handle reports Paused, not a failure"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val sockPath = tmpSock("paused")
      val (server, pausedSent) = startPausingHost(sockPath)
      val repl = replWith(sockPath)
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        val tool = EvalScala(repl)
        // eval #1: start a run and keep the handle in a val for a later eval.
        tool.execute(EvalScalaParams(
          """val run = wf.start[String](agent[String]("task A", id = "a"))""", Some(30_000)))
        // The host received the call, signalled paused, and dropped the connection.
        pausedSent.asFuture.await
        // eval #2+: the side channel advances between evals, so the worker has
        // processed the paused signal. Poll the handle's status (a few tries guards
        // against the rare case the worker hasn't drained the socket yet).
        var out = ""
        var tries = 0
        while !out.contains("Paused") && tries < 5 do
          out = tool.execute(EvalScalaParams("println(run.status)", Some(30_000))).output
          tries += 1
        // It reports Paused, and crucially NOT the 'socket closed' failure the
        // dropped connection would otherwise leave on the handle.
        assert(out.contains("Paused"), out)
        assert(!out.toLowerCase.contains("socket"), out)
      finally
        Async.fromSync(repl.close())
        try server.close() catch case _: Throwable => ()
