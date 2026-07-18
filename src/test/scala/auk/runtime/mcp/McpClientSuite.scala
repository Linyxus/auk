package auk.runtime.mcp

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Success

import gears.async.{Async, Future}
import gears.async.default.given

import auk.llm.tools.Json

/** [[McpClient]] JSON-RPC / handshake / timeout logic, driven entirely by a
  * scripted in-memory [[McpTransport]] — no child process. The fake answers
  * requests asynchronously (via `setTimeout(0)`, mirroring a real process
  * delivering on the event loop), so the client's suspend-and-resume path is
  * exercised for real. */
class McpClientSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 60.seconds

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  // -- scripted transport (shared double in ScriptedTransport.scala) -----------

  /** The standard successful `initialize` reply used by most tests. */
  private val initResult: Json = Json.Obj(
    List(
      "protocolVersion" -> Json.Str("2025-06-18"),
      "capabilities" -> Json.Obj(List("tools" -> Json.Obj(Nil))),
      "serverInfo" -> Json.Obj(List("name" -> Json.Str("stub-server"), "version" -> Json.Str("1.2.3")))
    )
  )

  /** Build a client wired to a scripted transport; returns the client and a thunk
    * to the (lazily created) transport for asserting what was written. */
  private def clientWith(
      script: Map[String, Reply],
      timeoutMs: Double = 5000,
      onWrite: Json => Unit = _ => ()
  ): (McpClient, () => ScriptedTransport) =
    var captured: ScriptedTransport | Null = null
    val factory: (String => Unit, Int => Unit) => McpTransport = (onLine, onExit) =>
      val t = new ScriptedTransport(script, onWrite, onLine, onExit)
      captured = t
      t
    (McpClient("test", factory, timeoutMs), () => captured.nn)

  private def methodOf(j: Json): Option[String] = JsonView.str(j, "method")
  private def paramsOf(j: Json): Json.Obj = JsonView.obj(j, "params").getOrElse(Json.Obj(Nil))

  // -- tests -------------------------------------------------------------------

  asyncTest("connect runs a well-formed initialize/initialized handshake and records serverInfo"):
    val (client, transport) = clientWith(Map("initialize" -> Reply.Result(initResult)))
    assert(client.connect() == Right(()))
    val w = transport().written.toList
    assertEquals(w.size, 2)
    val init = w.head
    assertEquals(JsonView.str(init, "jsonrpc"), Some("2.0"))
    assertEquals(methodOf(init), Some("initialize"))
    assert(JsonView.field(init, "id").exists { case Json.Num(_) => true; case _ => false }, "initialize must carry a numeric id")
    val params = paramsOf(init)
    assertEquals(JsonView.str(params, "protocolVersion"), Some("2025-06-18"))
    assertEquals(JsonView.str(JsonView.obj(params, "clientInfo").getOrElse(Json.Obj(Nil)), "name"), Some("auk"))
    // The second line is the initialized notification: a method, no id.
    val initialized = w(1)
    assertEquals(methodOf(initialized), Some("notifications/initialized"))
    assertEquals(JsonView.field(initialized, "id"), None)
    // Server identity was recorded.
    assertEquals(client.negotiatedProtocolVersion, Some("2025-06-18"))
    assertEquals(client.serverInfo.flatMap(si => JsonView.str(si, "name")), Some("stub-server"))

  asyncTest("connect is idempotent: a second call does not repeat the handshake"):
    val (client, transport) = clientWith(Map("initialize" -> Reply.Result(initResult)))
    assert(client.connect().isRight)
    assert(client.connect().isRight)
    assertEquals(transport().written.count(w => methodOf(w).contains("initialize")), 1)

  asyncTest("listTools connects lazily and decodes the tool list, defaulting absent fields"):
    val tools = Json.Obj(
      List(
        "tools" -> Json.Arr(
          List(
            Json.Obj(
              List(
                "name" -> Json.Str("add"),
                "description" -> Json.Str("adds two numbers"),
                "inputSchema" -> Json.Obj(List("type" -> Json.Str("object")))
              )
            ),
            Json.Obj(List("name" -> Json.Str("noschema")))
          )
        )
      )
    )
    val (client, _) = clientWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.Result(tools)))
    client.listTools() match
      case Right(list) =>
        assertEquals(list.map(_.name), List("add", "noschema"))
        assertEquals(list.head.description, "adds two numbers")
        assertEquals(list(1).description, "")
        assertEquals(list(1).inputSchema, Json.Obj(Nil))
      case Left(e) => fail(e)

  asyncTest("callTool forwards arguments and decodes the tool-level isError flag"):
    val callResult = Json.Obj(
      List(
        "content" -> Json.Arr(List(Json.Obj(List("type" -> Json.Str("text"), "text" -> Json.Str("boom"))))),
        "isError" -> Json.Bool(true)
      )
    )
    val (client, transport) = clientWith(Map("initialize" -> Reply.Result(initResult), "tools/call" -> Reply.Result(callResult)))
    client.callTool("explode", Json.Obj(List("x" -> Json.num(1)))) match
      case Right(r) =>
        assert(r.isError)
        assertEquals(r.content.map(_.kind), List("text"))
        assertEquals(r.content.map(_.text), List(Some("boom")))
        val call = transport().written.find(w => methodOf(w).contains("tools/call")).getOrElse(fail("no tools/call sent"))
        assertEquals(JsonView.str(paramsOf(call), "name"), Some("explode"))
      case Left(e) => fail(e)

  asyncTest("listResources decodes optional description/mimeType"):
    val res = Json.Obj(
      List(
        "resources" -> Json.Arr(
          List(
            Json.Obj(List("uri" -> Json.Str("file:///a"), "name" -> Json.Str("A"), "mimeType" -> Json.Str("text/plain"))),
            Json.Obj(List("uri" -> Json.Str("file:///b"), "name" -> Json.Str("B")))
          )
        )
      )
    )
    val (client, _) = clientWith(Map("initialize" -> Reply.Result(initResult), "resources/list" -> Reply.Result(res)))
    client.listResources() match
      case Right(list) =>
        assertEquals(list.map(_.uri), List("file:///a", "file:///b"))
        assertEquals(list.head.mimeType, Some("text/plain"))
        assertEquals(list(1).mimeType, None)
        assertEquals(list(1).description, None)
      case Left(e) => fail(e)

  asyncTest("readResource decodes text contents and forwards the uri"):
    val read = Json.Obj(
      List(
        "contents" -> Json.Arr(
          List(Json.Obj(List("uri" -> Json.Str("file:///a"), "mimeType" -> Json.Str("text/plain"), "text" -> Json.Str("hello"))))
        )
      )
    )
    val (client, transport) = clientWith(Map("initialize" -> Reply.Result(initResult), "resources/read" -> Reply.Result(read)))
    client.readResource("file:///a") match
      case Right(r) =>
        assertEquals(r.contents.map(_.text), List(Some("hello")))
        assertEquals(r.contents.head.kind, "text")
        assertEquals(r.contents.head.uri, Some("file:///a"))
        val req = transport().written.find(w => methodOf(w).contains("resources/read")).getOrElse(fail("no read sent"))
        assertEquals(JsonView.str(paramsOf(req), "uri"), Some("file:///a"))
      case Left(e) => fail(e)

  asyncTest("a JSON-RPC error response maps to Left carrying the server's message and code"):
    val (client, _) = clientWith(Map("initialize" -> Reply.Result(initResult), "tools/call" -> Reply.Error(-32000, "no such tool")))
    client.callTool("ghost", Json.Obj(Nil)) match
      case Left(msg) =>
        assert(msg.contains("no such tool"), msg)
        assert(msg.contains("-32000"), msg)
      case Right(_) => fail("expected an error Left")

  asyncTest("a request that is never answered fails with a timeout"):
    val (client, _) = clientWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.NoReply), timeoutMs = 80)
    client.listTools() match
      case Left(msg) => assert(msg.contains("timed out"), msg)
      case Right(_)  => fail("expected a timeout Left")

  asyncTest("onExit fails all pending requests"):
    val requested = Future.Promise[Unit]()
    val onWrite: Json => Unit = req =>
      if methodOf(req).contains("tools/list") then
        try requested.complete(Success(()))
        catch case _: Throwable => ()
    // A generous per-request timeout so the exit, not the timer, resolves the call.
    val (client, transport) =
      clientWith(Map("initialize" -> Reply.Result(initResult), "tools/list" -> Reply.NoReply), timeoutMs = 30000, onWrite = onWrite)
    val call = Future(client.listTools())
    requested.asFuture.await // tools/list has been written; the caller is now suspended
    transport().exit(1)
    call.await match
      case Left(msg) => assert(msg.contains("exited"), msg)
      case Right(_)  => fail("expected a Left after the server exited")

  asyncTest("after the server exits, the next call respawns a fresh transport and re-handshakes"):
    val script = Map[String, Reply](
      "initialize" -> Reply.Result(initResult),
      "tools/list" -> Reply.Result(Json.Obj(List("tools" -> Json.Arr(Nil))))
    )
    // A factory that records every transport it builds, so the test can prove the
    // second call built a NEW one rather than reusing the dead handle.
    val transports = mutable.ListBuffer.empty[ScriptedTransport]
    val factory: (String => Unit, Int => Unit) => McpTransport = (onLine, onExit) =>
      val t = new ScriptedTransport(script, _ => (), onLine, onExit)
      transports += t
      t
    val client = McpClient("test", factory, 30000)
    // First use: spawns transport #1 and handshakes.
    assert(client.listTools().isRight)
    assertEquals(transports.size, 1)
    // The server dies mid-life.
    transports.head.exit(1)
    // The next call must spawn a fresh transport, re-handshake, and succeed — not
    // fail forever against the dead handle.
    client.listTools() match
      case Right(_) => ()
      case Left(e)  => fail(s"expected the respawn to succeed, got: $e")
    assertEquals(transports.size, 2)
    assert(!(transports(0) eq transports(1)), "a new transport instance must handle the retry")
    assert(transports(1).written.exists(w => methodOf(w).contains("initialize")), "the fresh transport must be re-handshaked")

  asyncTest("a transport factory that throws fails connect cleanly and never leaks the gate"):
    // makeTransport throwing (a spawn blowing up) must become a Left, with the
    // connect gate resolved and cleared — otherwise a SECOND call would await the
    // dangling gate forever (which would surface here as a munitTimeout).
    var calls = 0
    val factory: (String => Unit, Int => Unit) => McpTransport = (_, _) =>
      calls += 1
      throw new RuntimeException("spawn boom")
    val client = McpClient("test", factory, 30000)
    client.connect() match
      case Left(msg) => assert(msg.contains("spawn boom"), msg)
      case Right(_)  => fail("expected the first connect to fail")
    client.connect() match
      case Left(msg) => assert(msg.contains("spawn boom"), msg)
      case Right(_)  => fail("expected the second connect to fail too (no hang, no memoised success)")
    assert(calls >= 2, s"each connect must retry the factory from a clean slate; calls=$calls")

  asyncTest("an inbound server→client request is answered with method-not-found"):
    val (client, transport) = clientWith(Map("initialize" -> Reply.Result(initResult)))
    assert(client.connect().isRight)
    transport().deliver(Json.Obj(List("jsonrpc" -> Json.Str("2.0"), "id" -> Json.num(99), "method" -> Json.Str("roots/list"))))
    val reply = transport().written.find(w => JsonView.field(w, "id").contains(Json.num(99))).getOrElse(fail("no reply to inbound request"))
    val error = JsonView.obj(reply, "error").getOrElse(fail("reply has no error object"))
    assertEquals(JsonView.str(error, "message"), Some("method not found"))
