package auk.runtime.repl

import scala.scalajs.js

import auk.TestFs
import auk.llm.tools.Json
import auk.platform.Platform

class WorkerLogSuite extends munit.FunSuite:

  private def tempDir(): String = TestFs.tempDir("auk-workerlog")

  private def lines(path: String): List[String] =
    TestFs.read(path).split("\n").toList.filter(_.nonEmpty)

  private def events(path: String): List[Json.Obj] =
    lines(path).map(l => Json.parse(l).toOption.get.asInstanceOf[Json.Obj])

  extension (obj: Json.Obj)
    private def field(key: String): Json = obj.fields.toMap.apply(key)
    private def str(key: String): String = obj.field(key).asInstanceOf[Json.Str].value
    private def has(key: String): Boolean = obj.fields.exists(_._1 == key)

  /** Advance the wall clock past the filesystem's mtime resolution. */
  private def waitAMoment(): Unit =
    val until = js.Date.now() + 5
    while js.Date.now() < until do ()

  // -- file naming ---------------------------------------------------------------

  test("the file is named by label, generation and pid"):
    val dir = tempDir()
    WorkerLogs(dir, "worker")(3, Some(4242)).killed("shutdown")
    assert(TestFs.exists(TestFs.join(dir, "worker-gen3-pid4242.jsonl")), Platform.fs.listDir(dir).toString)

  test("an unknown pid is spelled `na`"):
    val dir = tempDir()
    WorkerLogs(dir, "worker")(0, None).killed("shutdown")
    assert(TestFs.exists(TestFs.join(dir, "worker-gen0-pidna.jsonl")))

  test("the log directory is created on demand"):
    val dir = TestFs.join(tempDir(), "nested/logs")
    WorkerLogs(dir, "worker")(1, Some(7)).killed("shutdown")
    assert(TestFs.exists(TestFs.join(dir, "worker-gen1-pid7.jsonl")))

  // -- event records -------------------------------------------------------------

  test("each event appends exactly one JSON line, timestamped and named"):
    val dir = tempDir()
    val log = WorkerLogs(dir, "worker")(1, Some(11))
    log.spawned(List("node", "worker.js"), 1, List("DOTTY_CLASSPATH_BIN"), Some(11))
    log.sent("""{"op":"eval","code":"1 + 1"}""")
    log.recv("""{"op":"eval","ok":true}""", "reply")
    log.stderrChunk("warning: deprecated\n")
    log.settled("completed", "eval ok")
    log.liveness("silent", 3000L)
    log.killed("timeout")
    log.exit(-1, stale = true)

    val evs = events(TestFs.join(dir, "worker-gen1-pid11.jsonl"))
    assertEquals(
      evs.map(_.str("ev")),
      List("spawned", "sent", "recv", "stderr", "settled", "liveness", "killed", "exit")
    )
    assert(evs.forall(_.has("t")), "every event carries a timestamp")

  test("event payloads keep the field names of their signatures"):
    val dir = tempDir()
    val log = WorkerLogs(dir, "worker")(2, Some(12))
    log.spawned(List("node", "worker.js"), 2, List("PATH", "DOTTY_LINKER_LIBS_BIN"), Some(12))
    log.spawnFailed("ENOENT")
    log.recv("hello", "hello")
    log.liveness("responsive", 0L)
    log.exit(0, stale = false)

    val evs = events(TestFs.join(dir, "worker-gen2-pid12.jsonl"))
    val spawned = evs.head
    assertEquals(spawned.field("argv"), Json.Arr(List(Json.Str("node"), Json.Str("worker.js"))))
    assertEquals(spawned.field("generation"), Json.num(2))
    assertEquals(spawned.field("pid"), Json.num(12))
    assertEquals(
      spawned.field("envKeys"),
      Json.Arr(List(Json.Str("PATH"), Json.Str("DOTTY_LINKER_LIBS_BIN")))
    )
    assertEquals(evs(1).str("error"), "ENOENT")
    assertEquals(evs(2).str("line"), "hello")
    assertEquals(evs(2).str("kind"), "hello")
    assertEquals(evs(3).field("silentMs"), Json.num(0L))
    assert(!evs(3).has("phase"), "a worker that named no stage must not be given one")
    assertEquals(evs(4).field("code"), Json.num(0))
    assertEquals(evs(4).field("stale"), Json.Bool(false))

  test("a liveness event carries the stage the worker was in, when it named one"):
    val dir = tempDir()
    val log = WorkerLogs(dir, "worker")(1, Some(17))
    log.liveness("blocked", 92_000L, Some("compiling"))

    val ev = events(TestFs.join(dir, "worker-gen1-pid17.jsonl")).head
    assertEquals(ev.str("state"), "blocked")
    assertEquals(ev.field("silentMs"), Json.num(92_000L))
    // The whole picture in one event: a post-mortem reads this line and knows
    // what the worker was doing when it stopped, without reconstructing it from
    // the recv lines around it.
    assertEquals(ev.str("phase"), "compiling")

  test("a spawned event without a pid omits the field"):
    val dir = tempDir()
    val log = WorkerLogs(dir, "worker")(0, None)
    log.spawned(List("node"), 0, Nil, None)
    assert(!events(TestFs.join(dir, "worker-gen0-pidna.jsonl")).head.has("pid"))

  test("lines are recorded verbatim, JSON-escaped rather than mangled"):
    val dir = tempDir()
    val log = WorkerLogs(dir, "worker")(1, Some(13))
    val raw = "{\"code\":\"val s = \\\"a\\\"\"}\ttab [31mred[0m ünïcode"
    log.sent(raw)
    log.recv("multi\nline\r\nchunk", "garbage")

    val file = TestFs.join(dir, "worker-gen1-pid13.jsonl")
    assertEquals(lines(file).size, 2, "an embedded newline must not split the record")
    val evs = events(file)
    assertEquals(evs.head.str("line"), raw)
    assertEquals(evs(1).str("line"), "multi\nline\r\nchunk")

  // -- redaction -----------------------------------------------------------------

  test("environment values never reach the log"):
    val dir = tempDir()
    val env = Map("ANTHROPIC_API_KEY" -> "sk-secret-value", "DOTTY_CLASSPATH_BIN" -> "/opt/cp.bin")
    val log = WorkerLogs(dir, "worker")(1, Some(14))
    log.spawned(List("node", "worker.js"), 1, env.keys.toList.sorted, Some(14))

    val body = TestFs.read(TestFs.join(dir, "worker-gen1-pid14.jsonl"))
    assert(body.contains("ANTHROPIC_API_KEY"), body)
    env.values.foreach(v => assert(!body.contains(v), s"leaked env value: $v"))

  // -- failure containment -------------------------------------------------------

  test("a log that cannot be opened swallows every event"):
    val base = tempDir()
    val blocked = TestFs.join(base, "occupied")
    TestFs.write(blocked, "not a directory")

    val log = WorkerLogs(blocked, "worker")(1, Some(15))
    log.spawned(List("node"), 1, Nil, Some(15))
    log.sent("{}")
    log.recv("{}", "reply")
    log.stderrChunk("boom")
    log.settled("failed", "worker died")
    log.liveness("dead", 1L, Some("compiling"))
    log.killed("close")
    log.exit(1, stale = false)
    log.spawnFailed("ENOENT")

    assertEquals(TestFs.read(blocked), "not a directory")

  test("the noop log and factory accept every event"):
    WorkerLogs.NoopFactory(1, Some(16)).sent("{}")
    WorkerLog.Noop.exit(0, stale = false)

  // -- pruning -------------------------------------------------------------------

  test("opening a log prunes the oldest files beyond the cap"):
    val dir = tempDir()
    TestFs.mkdir(dir)
    val oldest = TestFs.join(dir, "worker-gen0-pid1.jsonl")
    TestFs.write(oldest, "{}\n")
    waitAMoment()
    (2 to 60).foreach(i => TestFs.write(TestFs.join(dir, s"worker-gen0-pid$i.jsonl"), "{}\n"))
    TestFs.write(TestFs.join(dir, "keep.txt"), "not a log")

    WorkerLogs(dir, "worker")(1, Some(999)).killed("shutdown")

    val logs = Platform.fs.listDir(dir).filter(_.name.endsWith(".jsonl"))
    assertEquals(logs.size, WorkerLogs.MaxFiles)
    assert(!TestFs.exists(oldest), "the oldest log survived pruning")
    assert(TestFs.exists(TestFs.join(dir, "worker-gen1-pid999.jsonl")))
    assert(TestFs.exists(TestFs.join(dir, "keep.txt")), "pruning only touches .jsonl files")

  test("a directory under the cap is left alone"):
    val dir = tempDir()
    TestFs.mkdir(dir)
    (1 to 10).foreach(i => TestFs.write(TestFs.join(dir, s"worker-gen0-pid$i.jsonl"), "{}\n"))

    WorkerLogs(dir, "worker")(1, Some(999)).killed("shutdown")

    assertEquals(Platform.fs.listDir(dir).count(_.name.endsWith(".jsonl")), 11)
