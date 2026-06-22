package auk.runtime

import auk.TestFs
import auk.llm.tools.Json
import auk.workflow.{Forest, ForestNode, NodeStatus, RunStatus}

class WorkflowDbSuite extends munit.FunSuite:

  private def db(): (WorkflowDb, String) =
    (WorkflowDb(TestFs.tempDir("auk-wfdb")), "sess1")

  private def sample(id: String, status: RunStatus): WorkflowRecord =
    WorkflowRecord(
      id = id,
      status = status,
      code = "wf.start:\n  agent[String](\"go\", id = \"a\")",
      results = Map("a" -> Json.Str("done-a"), "b" -> Json.Obj(List("n" -> Json.num(7)))),
      forest = Forest(
        nodes = Vector(
          ForestNode("a", Some("g1"), Nil, NodeStatus.Done, 10L, 20L, None, Some("a: done"), Some("Inspect a")),
          // 'b' was in flight when paused → Interrupted (also exercises that status's persistence).
          ForestNode("b", Some("g1"), List("a"), NodeStatus.Interrupted)
        ),
        logs = Vector("started"),
        code = Some("wf.start..."),
        status = status
      )
    )

  test("save then load round-trips a whole record"):
    val (d, sid) = db()
    val rec = sample("wf-1", RunStatus.Paused)
    assertEquals(d.save(sid, rec), Right(()))
    assertEquals(d.load(sid, "wf-1"), Some(rec))

  test("loading a missing workflow is None"):
    val (d, sid) = db()
    assertEquals(d.load(sid, "nope"), None)

  test("cached sub-agent outputs survive the round-trip, keyed by node id"):
    val (d, sid) = db()
    d.save(sid, sample("wf-2", RunStatus.Paused))
    val loaded = d.load(sid, "wf-2").get
    assertEquals(loaded.results("a"), Json.Str("done-a"))
    assertEquals(loaded.results("b"), Json.Obj(List("n" -> Json.num(7))))

  test("listPaused returns only the paused records for a session"):
    val (d, sid) = db()
    d.save(sid, sample("wf-run", RunStatus.Running))
    d.save(sid, sample("wf-paused", RunStatus.Paused))
    d.save(sid, sample("wf-done", RunStatus.Done))
    assertEquals(d.listPaused(sid).map(_.id).toSet, Set("wf-paused"))

  test("records are scoped per session id"):
    val (d, _) = db()
    d.save("sessionA", sample("wf-x", RunStatus.Paused))
    assertEquals(d.load("sessionB", "wf-x"), None)
    assertEquals(d.load("sessionA", "wf-x").map(_.id), Some("wf-x"))
