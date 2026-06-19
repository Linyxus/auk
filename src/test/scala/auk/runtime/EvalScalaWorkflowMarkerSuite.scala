package auk.runtime

import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** The workflow start-marker plumbing in `EvalScala`: `wf.start` prints a
  * `auk:workflow:start:<runId>` line per launched run, which the tool reads to
  * announce each run's source and then strips so neither the marker nor the run
  * id leaks into the model-visible output. */
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

  test("stripWorkflowMarkers removes the whole marker line — no marker or id leaks"):
    val stripped = EvalScala.stripWorkflowMarkers("before\nauk:workflow:start:wf-9-1\nafter\n")
    assert(!stripped.contains("auk:workflow:start"), stripped)
    assert(!stripped.contains("wf-9-1"), stripped)
    assertEquals(stripped, "before\nafter\n")

  test("stripWorkflowMarkers leaves non-workflow output untouched"):
    val text = "val res0: WorkflowRun[String] = WorkflowRun(id=wf-1-1, still running)\n"
    assertEquals(EvalScala.stripWorkflowMarkers(text), text)
