package auk.library

import scala.concurrent.Promise

/** The `WorkflowRun` handle returned by `wf.start` — a synchronous, pollable view
  * over the run's terminal `Future` (the worker cannot await, so the model polls
  * across eval calls). Pure: drives a `Promise` directly, no worker/bridge. */
class WorkflowRunSuite extends munit.FunSuite:

  test("a running run is not done, renders 'still running', and refuses early access"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-1", p.future)
    assertEquals(run.id, "wf-1")
    assert(!run.isDone)
    assertEquals(run.toString, "WorkflowRun(id=wf-1, still running)")
    intercept[IllegalStateException](run.getResult)
    intercept[IllegalStateException](run.getError)
    intercept[IllegalStateException](run.isOk)

  test("a succeeded run reports ok, the result, and renders 'done'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-2", p.future)
    p.success(42)
    assert(run.isDone)
    assert(run.isOk)
    assertEquals(run.getResult, 42)
    assertEquals(run.getError, None)
    assertEquals(run.toString, "WorkflowRun(id=wf-2, done)")

  test("a failed run reports the error, rethrows on getResult, and renders 'error: <msg>'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-3", p.future)
    p.failure(new RuntimeException("boom"))
    assert(run.isDone)
    assert(!run.isOk)
    assertEquals(run.getError.map(_.getMessage), Some("boom"))
    val thrown = intercept[RuntimeException](run.getResult)
    assertEquals(thrown.getMessage, "boom")
    assertEquals(run.toString, "WorkflowRun(id=wf-3, error: boom)")

  test("a failed run with a null exception message falls back to 'workflow failed'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-4", p.future)
    p.failure(new RuntimeException()) // null message
    assertEquals(run.toString, "WorkflowRun(id=wf-4, error: workflow failed)")
    assert(run.getError.isDefined)
