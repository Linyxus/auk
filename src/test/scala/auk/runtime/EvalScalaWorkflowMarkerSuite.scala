package auk.runtime

import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** The start-marker plumbing in `EvalScala`. Two kinds share it: `wf.start` prints a
  * `auk:workflow:start:<runId>` line per launched run and `lib.loop.start` prints a
  * `auk:loop:start:<loopId>` line per loop. The tool reads them to announce the eval's
  * source to the right bridge, and strips every marker line so neither the marker nor
  * the id leaks into the model-visible output. */
class EvalScalaWorkflowMarkerSuite extends munit.FunSuite:

  private def completed(stdout: String): ScalaRepl.EvalResult =
    ScalaRepl.EvalResult(
      ScalaRepl.Status.Completed(
        ReplProtocol.Response(op = "eval", ok = true, output = "", stdout = stdout, stderr = "", error = None, stateVersion = 1)
      ),
      restartedSession = false
    )

  test("workflowRunIds extracts a single run id from a marker line"):
    assertEquals(EvalScala.workflowRunIds(completed("auk:workflow:start:wf-123-1\n")), List("wf-123-1"))

  test("workflowRunIds extracts every run id when one eval starts multiple workflows"):
    val stdout = "auk:workflow:start:wf-7-1\nsome other output\nauk:workflow:start:wf-7-2\n"
    assertEquals(EvalScala.workflowRunIds(completed(stdout)), List("wf-7-1", "wf-7-2"))

  test("workflowRunIds is empty for a non-workflow eval"):
    assertEquals(EvalScala.workflowRunIds(completed("val res0: Int = 2\n")), Nil)

  test("stripMarkers removes the whole marker line — no marker or id leaks"):
    val stripped = EvalScala.stripMarkers("before\nauk:workflow:start:wf-9-1\nafter\n")
    assert(!stripped.contains("auk:workflow:start"), stripped)
    assert(!stripped.contains("wf-9-1"), stripped)
    assertEquals(stripped, "before\nafter\n")

  test("stripMarkers leaves non-workflow output untouched"):
    val text = "val res0: WorkflowRun[String] = WorkflowRun(id=wf-1-1, still running)\n"
    assertEquals(EvalScala.stripMarkers(text), text)

  // -- loop markers -------------------------------------------------------------

  test("loopIds extracts the loop id from a marker line, and the two kinds do not cross"):
    assertEquals(EvalScala.loopIds(completed("auk:loop:start:opt-tokenizer\n")), List("opt-tokenizer"))
    assertEquals(EvalScala.workflowRunIds(completed("auk:loop:start:opt-tokenizer\n")), Nil)
    assertEquals(EvalScala.loopIds(completed("auk:workflow:start:wf-1-1\n")), Nil)

  test("loopIds is empty for an eval that started no loop"):
    assertEquals(EvalScala.loopIds(completed("val res0: Int = 2\n")), Nil)

  test("stripMarkers removes loop markers too, including alongside a workflow one"):
    val stripped = EvalScala.stripMarkers("before\nauk:loop:start:opt\nauk:workflow:start:wf-1-1\nafter\n")
    assert(!stripped.contains("auk:loop:start"), stripped)
    assert(!stripped.contains("opt"), stripped)
    assertEquals(stripped, "before\nafter\n")

  test("stripMarkers leaves a loop handle's own rendering untouched"):
    val text = "val res0: LoopHandle = Loop(opt: validating)\n"
    assertEquals(EvalScala.stripMarkers(text), text)
