package auk.library

import scala.concurrent.Promise

/** The `WorkflowRun` handle returned by `wf.start` — a synchronous, pollable view
  * over the run's terminal `Future` (the worker cannot await, so the model polls
  * `status` across eval calls), plus a pause seam the host drives. Pure: drives a
  * `Promise` and a `paused` flag directly, no worker/bridge. */
class WorkflowRunSuite extends munit.FunSuite:

  test("a running run reports Running, renders 'still running', and refuses early access"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-1", p.future)
    assertEquals(run.id, "wf-1")
    assertEquals(run.status, WorkflowStatus.Running)
    assertEquals(run.toString, "WorkflowRun(id=wf-1, still running)")
    intercept[IllegalStateException](run.getResult)
    intercept[IllegalStateException](run.getError)
    intercept[IllegalStateException](run.isOk)

  test("a succeeded run reports Done(true), the result, and renders 'done'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-2", p.future)
    p.success(42)
    assertEquals(run.status, WorkflowStatus.Done(true))
    assert(run.isOk)
    assertEquals(run.getResult, 42)
    assertEquals(run.getError, None)
    assertEquals(run.toString, "WorkflowRun(id=wf-2, done)")
    // The status carries isOk and destructures.
    run.status match
      case WorkflowStatus.Done(ok) => assert(ok)
      case other                   => fail(s"expected Done(true), got $other")

  test("a failed run reports Done(false), the error, rethrows on getResult, and renders 'error: <msg>'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-3", p.future)
    p.failure(new RuntimeException("boom"))
    assertEquals(run.status, WorkflowStatus.Done(false))
    assert(!run.isOk)
    assertEquals(run.getError.map(_.getMessage), Some("boom"))
    val thrown = intercept[RuntimeException](run.getResult)
    assertEquals(thrown.getMessage, "boom")
    assertEquals(run.toString, "WorkflowRun(id=wf-3, error: boom)")

  test("a failed run with a null exception message falls back to 'workflow failed'"):
    val p = Promise[Int]()
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-4", p.future)
    p.failure(new RuntimeException()) // null message
    assertEquals(run.status, WorkflowStatus.Done(false))
    assertEquals(run.toString, "WorkflowRun(id=wf-4, error: workflow failed)")
    assert(run.getError.isDefined)

  test("a paused run reports Paused, renders 'paused', and refuses access with a paused message"):
    val p = Promise[Int]()
    var paused = true
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-5", p.future, () => paused)
    assertEquals(run.status, WorkflowStatus.Paused)
    assertEquals(run.toString, "WorkflowRun(id=wf-5, paused)")
    val e = intercept[IllegalStateException](run.getResult)
    assert(e.getMessage.contains("paused"), e.getMessage)
    intercept[IllegalStateException](run.getError)
    intercept[IllegalStateException](run.isOk)

  test("Paused masks the socket failure a cancelled run leaves on the Future"):
    // The real pause path: the host drops the connection, so the underlying calls
    // fail with 'workflow socket closed' — but the handle is paused, so it must
    // report Paused (not a failure) and not surface that socket error.
    val p = Promise[Int]()
    var paused = true
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-6", p.future, () => paused)
    p.failure(new RuntimeException("workflow socket closed"))
    assertEquals(run.status, WorkflowStatus.Paused)
    assertEquals(run.toString, "WorkflowRun(id=wf-6, paused)")
    val e = intercept[IllegalStateException](run.getResult)
    assert(e.getMessage.contains("paused"), e.getMessage)
    assert(!e.getMessage.contains("socket"), e.getMessage)

  test("clearing the pause flag returns the handle to Running, then Done as the Future settles"):
    val p = Promise[Int]()
    var paused = true
    val run: WorkflowRun[Int] = new WorkflowRunImpl("wf-7", p.future, () => paused)
    assertEquals(run.status, WorkflowStatus.Paused)
    paused = false
    // Not yet settled → Running.
    assertEquals(run.status, WorkflowStatus.Running)
    p.success(7)
    assertEquals(run.status, WorkflowStatus.Done(true))
    assertEquals(run.getResult, 7)

  test("pause() and resume() forward to the injected lifecycle requests, in any state"):
    val p = Promise[Int]()
    var pauses = 0
    var resumes = 0
    val run: WorkflowRun[Int] =
      new WorkflowRunImpl("wf-8", p.future, () => false, () => pauses += 1, () => resumes += 1)
    run.pause()
    run.resume()
    assertEquals((pauses, resumes), (1, 1))
    // The handle just forwards the request (the host validates state), so it stays
    // safe to call after the run has settled.
    p.success(1)
    run.pause()
    run.resume()
    assertEquals((pauses, resumes), (2, 2))
