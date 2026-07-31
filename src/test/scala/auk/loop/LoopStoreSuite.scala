package auk.loop

import auk.TestFs
import auk.llm.tools.{Json, RuntimeContext}
import auk.platform.PathOps

/** Against a real filesystem in a temp directory, since what is being claimed here
  * is about files: that a ledger survives being written and read back, that a bad
  * line is reported by position rather than swallowed, and that the loop directory
  * keeps itself out of the git tree it is meanwhile editing.
  */
class LoopStoreSuite extends munit.FunSuite:

  /** A store rooted at the `.auk` directory of a fresh project. */
  private def newStore(): LoopStore =
    LoopStore(PathOps.join(TestFs.tempDir("auk-loop"), LoopStore.AukRelativePath))

  private def event(n: Int): LoopEvent =
    LoopEvent.GenerationStarted(n, if n == 1 then None else Some(n - 1), s"worker-$n", s"t$n")

  private val created = LoopEvent.LoopCreated("loop-1", "base", "head", "session-0", "t0")

  private def failure[A](result: Either[String, A])(using munit.Location): String = result match
    case Left(message) => message
    case Right(value)  => fail(s"expected a failure but got: $value")

  // --- ids ---------------------------------------------------------------------

  test("loop ids are letters, digits, underscore and dash, and nothing else"):
    val accepted = List("a", "loop-1", "LOOP_1", "0", "9f8e7d6c", "a-b_c-D-9")
    for id <- accepted do
      assertEquals(LoopStore.validateId(id), Right(()), s"'$id' should be a valid loop id")

    val rejected = List("", " ", "loop 1", "loop.1", "loop/1", ".", "..", "../escape", "loop\n1", "café", "loop:1")
    for id <- rejected do
      assert(LoopStore.validateId(id).isLeft, s"'$id' should not be a valid loop id")

  test("an invalid id is refused by every operation, with the id in the message"):
    val store = newStore()
    assertEquals(failure(store.ensure("../escape")), "invalid loop id '../escape': expected one or more of [A-Za-z0-9_-]")
    assert(store.append("../escape", created).isLeft)
    assert(store.readAll("../escape").isLeft)
    assert(store.state("../escape").isLeft)
    assert(store.readKnowledge("../escape").isLeft)
    assert(store.writeKnowledge("../escape", "x").isLeft)
    assert(store.appendDeadEnd("../escape", "x").isLeft)
    assertEquals(TestFs.exists(store.root), false, "a refused id should not have created anything")

  // --- layout ------------------------------------------------------------------

  test("ensure creates the loop directory and a self-ignoring .auk/.gitignore"):
    val store = newStore()
    assertEquals(store.ensure("loop-1"), Right(()))
    assert(TestFs.exists(store.dirFor("loop-1")), "the loop directory should exist")
    assertEquals(TestFs.read(PathOps.join(store.root, ".gitignore")), "*\n")
    assertEquals(store.dirFor("loop-1"), PathOps.join(store.root, "loops/loop-1"))
    assertEquals(store.ledgerPath("loop-1"), PathOps.join(store.root, "loops/loop-1/ledger.jsonl"))
    assertEquals(store.knowledgePath("loop-1"), PathOps.join(store.root, "loops/loop-1/knowledge.md"))

  test("ensure is idempotent and never overwrites a .gitignore the project already has"):
    val store = newStore()
    TestFs.write(PathOps.join(store.root, ".gitignore"), "sessions/\n")
    assertEquals(store.ensure("loop-1"), Right(()))
    assertEquals(store.ensure("loop-1"), Right(()))
    assertEquals(TestFs.read(PathOps.join(store.root, ".gitignore")), "sessions/\n")

  test("a store is rooted at the .auk directory of the context's project"):
    val project = TestFs.tempDir("auk-loop-ctx")
    assertEquals(LoopStore.in(RuntimeContext(project)).root, PathOps.join(project, ".auk"))

  // --- ledger ------------------------------------------------------------------

  test("appended events read back in order, one JSONL line each"):
    val store = newStore()
    val events = List(created, event(1), event(2))
    for e <- events do assertEquals(store.append("loop-1", e), Right(()))

    assertEquals(store.readAll("loop-1"), Right(events.toVector))
    assertEquals(TestFs.read(store.ledgerPath("loop-1")).linesIterator.size, 3)

  test("appending creates the .gitignore too, so no caller has to remember ensure"):
    val store = newStore()
    assertEquals(store.append("loop-1", created), Right(()))
    assertEquals(TestFs.read(PathOps.join(store.root, ".gitignore")), "*\n")

  test("a loop with no ledger yet reads as empty rather than as an error"):
    val store = newStore()
    assertEquals(store.readAll("never-written"), Right(Vector.empty))

  test("one undecodable line fails the whole read and says which line it was"):
    val store = newStore()
    store.append("loop-1", created)
    store.append("loop-1", event(1))
    TestFs.append(store.ledgerPath("loop-1"), "{\"type\":\"generation_started\",\"gen\":3}\n")
    store.append("loop-1", event(2))

    assertEquals(
      failure(store.readAll("loop-1")),
      "loop 'loop-1' ledger line 3: generation_started.sessionId: missing field"
    )

  test("a line that is not JSON is reported by position too"):
    val store = newStore()
    store.append("loop-1", created)
    TestFs.append(store.ledgerPath("loop-1"), "not json at all\n")
    assert(failure(store.readAll("loop-1")).startsWith("loop 'loop-1' ledger line 2: "))

  test("state folds the ledger it just read"):
    val store = newStore()
    for e <- List(created, event(1)) do store.append("loop-1", e)
    val state = store.state("loop-1") match
      case Right(s) => s
      case Left(m)  => fail(s"expected a folded state but got: $m")
    assertEquals(state.loopId, "loop-1")
    assertEquals(state.inFlight.map(_.gen), Some(1))

  test("a structurally impossible ledger fails the fold, not the read"):
    val store = newStore()
    store.append("loop-1", event(1)) // no loop_created at the head
    assertEquals(store.readAll("loop-1"), Right(Vector(event(1))))
    assert(failure(store.state("loop-1")).contains("must begin with a loop_created event"))

  // --- listing -----------------------------------------------------------------

  test("list reports only loops that have a ledger, sorted"):
    val store = newStore()
    assertEquals(store.list(), Nil, "a store with no loops lists nothing")

    store.append("zeta", created)
    store.append("alpha", created)
    store.ensure("no-ledger-yet")
    TestFs.write(PathOps.join(store.loopsDir, "stray-file"), "not a loop")

    assertEquals(store.list(), List("alpha", "zeta"))

  // --- knowledge ---------------------------------------------------------------

  test("knowledge round-trips, and a loop that has recorded none reads as empty"):
    val store = newStore()
    assertEquals(store.readKnowledge("loop-1"), Right(""))

    val text = "# What we learned\n\n- 目標 is reachable\n- the checker is slow\n"
    assertEquals(store.writeKnowledge("loop-1", text), Right(()))
    assertEquals(store.readKnowledge("loop-1"), Right(text))

    assertEquals(store.writeKnowledge("loop-1", "replaced"), Right(()))
    assertEquals(store.readKnowledge("loop-1"), Right("replaced"))

  test("writing knowledge prepares the loop directory like any other write"):
    val store = newStore()
    assertEquals(store.writeKnowledge("fresh", "note"), Right(()))
    assertEquals(TestFs.read(PathOps.join(store.root, ".gitignore")), "*\n")
    assertEquals(store.list(), Nil, "knowledge alone does not make a loop")

  // --- dead ends ---------------------------------------------------------------

  test("a dead end creates its heading the first time and joins it afterwards"):
    val store = newStore()
    assertEquals(store.appendDeadEnd("loop-1", "gen 1 (2 attempts): widened the cache — rejected: p99 regressed"), Right(()))
    assertEquals(
      store.readKnowledge("loop-1"),
      Right("## Dead ends\n\n- gen 1 (2 attempts): widened the cache — rejected: p99 regressed\n")
    )

    assertEquals(store.appendDeadEnd("loop-1", "gen 2 (no attempts): died with its session"), Right(()))
    assertEquals(
      store.readKnowledge("loop-1"),
      Right(
        "## Dead ends\n\n" +
          "- gen 1 (2 attempts): widened the cache — rejected: p99 regressed\n" +
          "- gen 2 (no attempts): died with its session\n"
      )
    )

  test("a dead end leaves what a worker already wrote alone"):
    val store = newStore()
    store.writeKnowledge("loop-1", "# What we learned\n\n- the tokenizer is the bottleneck\n")
    assertEquals(store.appendDeadEnd("loop-1", "gen 1 (1 attempt): rewrote it in Rust"), Right(()))
    assertEquals(
      store.readKnowledge("loop-1"),
      Right(
        "# What we learned\n\n- the tokenizer is the bottleneck\n\n" +
          "## Dead ends\n\n- gen 1 (1 attempt): rewrote it in Rust\n"
      )
    )

  test("a dead end joins its own section rather than the end of the file"):
    // A worker is free to put the heading wherever it likes, and to write below it.
    val store = newStore()
    store.writeKnowledge("loop-1", "## Dead ends\n\n- gen 1: nope\n\n## Still open\n\n- try SIMD\n")
    assertEquals(store.appendDeadEnd("loop-1", "gen 2: also nope"), Right(()))
    assertEquals(
      store.readKnowledge("loop-1"),
      Right("## Dead ends\n\n- gen 1: nope\n- gen 2: also nope\n\n## Still open\n\n- try SIMD\n")
    )

  test("a multi-line dead end is flattened, because a bullet that wraps is not a bullet"):
    val store = newStore()
    store.appendDeadEnd("loop-1", "gen 1 (1 attempt): tried this\n\nand then that")
    assertEquals(store.readKnowledge("loop-1"), Right("## Dead ends\n\n- gen 1 (1 attempt): tried this and then that\n"))

  test("a worker's replacement still replaces the whole file, dead ends included"):
    // The asymmetry is deliberate: the host appends its own record unconditionally, and
    // an accepted worker's knowledge is still the whole file's contents.
    val store = newStore()
    store.appendDeadEnd("loop-1", "gen 1 (2 attempts): widened the cache")
    assertEquals(store.writeKnowledge("loop-1", "everything I know now"), Right(()))
    assertEquals(store.readKnowledge("loop-1"), Right("everything I know now"))

  // --- schema comparison -------------------------------------------------------

  test("schemas match regardless of field order, which plain equality would miss"):
    val a = Json.Obj(List("x" -> Json.num(1), "y" -> Json.num(2)))
    val b = Json.Obj(List("y" -> Json.num(2), "x" -> Json.num(1)))
    assertNotEquals(a: Json, b: Json, "the Json ADT keeps field order, so == is the wrong test here")
    assert(LoopStore.schemasMatch(a, b))

  test("schemas match nested objects by key and arrays by position"):
    val nested = (order: Boolean) =>
      Json.Obj(List(
        "props" -> Json.Obj(
          if order then List("a" -> Json.Str("s"), "b" -> Json.Bool(true))
          else List("b" -> Json.Bool(true), "a" -> Json.Str("s"))
        ),
        "required" -> Json.Arr(List(Json.Str("a"), Json.Str("b")))
      ))
    assert(LoopStore.schemasMatch(nested(true), nested(false)))

    val reordered = Json.Obj(List("required" -> Json.Arr(List(Json.Str("b"), Json.Str("a")))))
    val original = Json.Obj(List("required" -> Json.Arr(List(Json.Str("a"), Json.Str("b")))))
    assert(!LoopStore.schemasMatch(original, reordered), "an array's order is part of its meaning")

  test("schemas match numbers by value and a duplicated key by its last occurrence"):
    assert(LoopStore.schemasMatch(Json.num(1), Json.Num(BigDecimal("1.00"))))
    assert(LoopStore.schemasMatch(
      Json.Obj(List("x" -> Json.num(1), "x" -> Json.num(2))),
      Json.Obj(List("x" -> Json.num(2)))
    ))

  test("schemas that really differ do not match"):
    assert(!LoopStore.schemasMatch(Json.Obj(List("x" -> Json.num(1))), Json.Obj(List("x" -> Json.num(2)))))
    assert(!LoopStore.schemasMatch(Json.Obj(List("x" -> Json.num(1))), Json.Obj(List("x" -> Json.num(1), "y" -> Json.Null))))
    assert(!LoopStore.schemasMatch(Json.Str("1"), Json.num(1)))
    assert(!LoopStore.schemasMatch(Json.Arr(List(Json.num(1))), Json.Arr(List(Json.num(1), Json.num(2)))))
    assert(LoopStore.schemasMatch(Json.Null, Json.Null))
