package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.util.{Success, Failure}
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

  /** Drive a JS async iterable to exhaustion, invoking `f` on each yielded value.
    * Each `next()` suspends via JSPI. */
  def forEachAsync(iterable: js.Any)(f: js.Dynamic => Unit)(using Async): Unit =
    val it = iterable.asInstanceOf[JsAsyncIterable].asyncIterator()
    var continue = true
    while continue do
      val res = await(it.next())
      if res.done.getOrElse(false) then continue = false
      else f(res.value)
