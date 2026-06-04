package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.timers
import scala.util.{Success, Failure, Try}
import gears.async.{Async, Future}

/** A JS async-iterable (`for await … of`), accessed via the well-known symbol. */
@js.native
trait JsAsyncIterable extends js.Object:
  @JSName(js.Symbol.asyncIterator)
  def asyncIterator(): JsAsyncIterator = js.native

@js.native
trait JsAsyncIterator extends js.Object:
  def next(): js.Promise[JsIterResult] = js.native

@js.native
trait JsIterResult extends js.Object:
  val done: js.UndefOr[Boolean] = js.native
  val value: js.Dynamic = js.native

/** Bridges JS `Promise`s / async iterables into gears, suspending the Wasm stack
  * via JSPI. */
object Interop:
  /** Await a JS `Promise`, suspending the current fiber until it settles. */
  def await[T](p: js.Promise[T])(using Async): T =
    val pr = Future.Promise[T]()
    p.`then`[Unit](
      (v: T) => pr.complete(Success(v)),
      js.defined((e: Any) => pr.complete(Failure(js.JavaScriptException(e))))
    )
    pr.asFuture.await

  /** Await a JS `Promise`, but fail with `label` if it has not settled within
    * `timeoutMs`. A promise that never settles (a stalled/half-open network
    * connection) would otherwise suspend the fiber forever; this guarantees the
    * await always completes. The timer is cleared as soon as the promise settles
    * so it cannot keep the runtime alive or fire spuriously. */
  def awaitWithin[T](p: js.Promise[T], timeoutMs: Double, label: => String)(using Async): T =
    val pr = Future.Promise[T]()
    var settled = false // single-threaded JS: a plain flag is race-free
    def finish(r: Try[T]): Unit =
      if !settled then
        settled = true
        pr.complete(r)
    val handle = timers.setTimeout(timeoutMs):
      finish(Failure(js.JavaScriptException(new js.Error(label))))
    p.`then`[Unit](
      (v: T) => { timers.clearTimeout(handle); finish(Success(v)); () },
      js.defined((e: Any) => { timers.clearTimeout(handle); finish(Failure(js.JavaScriptException(e))); () })
    )
    pr.asFuture.await

  /** Drive a JS async iterable to exhaustion, invoking `f` on each yielded value.
    * Each `next()` suspends via JSPI, bounded by `idleTimeoutMs`: if no value
    * arrives within that window the iteration fails rather than hanging. */
  def forEachAsync(iterable: js.Any, idleTimeoutMs: Double)(f: js.Dynamic => Unit)(using Async): Unit =
    val it = iterable.asInstanceOf[JsAsyncIterable].asyncIterator()
    var continue = true
    while continue do
      val res = awaitWithin(it.next(), idleTimeoutMs, s"stream stalled: no data for ${(idleTimeoutMs / 1000).toInt}s")
      if res.done.getOrElse(false) then continue = false
      else f(res.value)
