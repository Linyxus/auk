package auk.runtime

import gears.async.Async

import auk.runtime.repl.ScalaRepl

/** A pool of [[ScalaRepl]] workers leased to workflow sub-agents for `eval_scala`.
  *
  * Leasing is gated by the [[WorkflowBridge]]'s concurrency cap, so at most that
  * many are out at once. Workers spawn lazily (only when a sub-agent actually
  * evaluates), so an idle lease costs nothing — and a sub-agent that never evals
  * never spawns a worker. Released repls are reused, keeping their warm session.
  */
final class ReplPool(make: () => ScalaRepl):
  private val idle = scala.collection.mutable.Queue.empty[ScalaRepl]

  def lease(): ScalaRepl = if idle.nonEmpty then idle.dequeue() else make()

  def release(r: ScalaRepl): Unit = idle.enqueue(r)

  def close()(using Async): Unit =
    val all = idle.toList
    idle.clear()
    all.foreach(_.close())
