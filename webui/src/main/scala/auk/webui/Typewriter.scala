package auk.webui

import scala.collection.mutable
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** A gradually-revealed ("typewriter") text element.
  *
  * The model delivers text in bursty, uneven deltas; rendering each delta
  * verbatim looks jumpy. This component decouples *display* from *delivery*: a
  * single owned `requestAnimationFrame` loop reveals the accumulated text at a
  * smooth, adaptive, frame-rate-independent pace, so however the chunks arrive the
  * reader sees a steady stream of characters.
  *
  * It owns its subtree imperatively rather than through reactive `children <--`,
  * because the per-frame mutation is character-level and a single growing text
  * node is far cheaper than a node per chunk:
  *
  * {{{
  * span.tw
  *   ├─ #text        settled chars (one Text node, grown via appendData)
  *   ├─ span.tw-tail just-revealed runs, one span.tw-ch each, fading in
  *   └─ span.tw-caret an inline caret riding the reveal point (optional)
  * }}}
  *
  * Each frame reveals a run of characters as one `span.tw-ch` (so the live span
  * count is bounded by `FadeMs`, not the message length); once a run's fade has
  * elapsed its text is folded into the settled node and the span is dropped. Live
  * DOM stays O(burst); total work is O(N).
  *
  *   - `text`     the full target string (chunks already joined upstream)
  *   - `animate`  true while this run is the live, streaming tail; false snaps to
  *                the full text instantly and runs no loop
  *   - `follow`   called after each visual advance so the caller can keep the view
  *                pinned to the bottom (a no-op when the reader has scrolled up)
  *   - `caret`    whether to draw the inline caret while animating
  *   - `isPrimed` false during the panel's initial mount (a pre-existing row, shown
  *                in full at once) and true afterwards (a freshly born row, typed
  *                from the start). See [[WorkflowRender]] for how this is threaded.
  */
object Typewriter:

  /** Chars/sec floor — never crawl, even when only a few chars are pending. */
  private val MinCps = 35.0
  /** Chars/sec cap — never blast a paragraph in a single frame. */
  private val MaxCps = 900.0
  /** Exponential catch-up rate (per second): reveal the backlog over ~1/CatchUp s. */
  private val CatchUp = 6.0
  /** Hard per-frame char cap (guards a long stall / backgrounded tab). */
  private val MaxPerFrame = 60
  /** Per-run fade-in duration (ms); MUST match the `tw-fade` CSS keyframe. */
  private val FadeMs = 200.0

  private lazy val reducedMotion: Boolean =
    try dom.window.matchMedia("(prefers-reduced-motion: reduce)").matches
    catch case _: Throwable => false

  private final class Fading(val el: dom.Element, val text: String, val ts: Double)

  def typed(
      text: Signal[String],
      animate: Signal[Boolean],
      follow: () => Unit,
      caret: Boolean = true,
      isPrimed: () => Boolean = () => true
  ): HtmlElement =
    // Raw DOM children, created once and managed imperatively (not by Laminar).
    val settledNode = dom.document.createTextNode("")
    val tailEl = dom.document.createElement("span"); tailEl.setAttribute("class", "tw-tail")
    val caretEl = dom.document.createElement("span"); caretEl.setAttribute("class", "tw-caret tw-caret--off")

    // Imperative animation state, captured by the rAF closure.
    var target: String = ""
    var displayed: Int = 0
    var carry: Double = 0.0
    var animating: Boolean = false
    var rafId: Int = -1
    var lastTs: Double = -1.0
    var built: Boolean = false
    var seeded: Boolean = false
    val fadeQ = mutable.Queue.empty[Fading]

    def showCaret(on: Boolean): Unit =
      if caret then caretEl.setAttribute("class", if on then "tw-caret" else "tw-caret tw-caret--off")

    def clearTail(): Unit =
      fadeQ.clear()
      tailEl.textContent = ""

    def cancelLoop(): Unit =
      if rafId >= 0 then dom.window.cancelAnimationFrame(rafId)
      rafId = -1
      lastTs = -1.0

    def snapToEnd(): Unit =
      cancelLoop()
      settledNode.data = target
      clearTail()
      displayed = target.length
      carry = 0.0
      showCaret(false)

    def revealRun(n: Int, ts: Double): Unit =
      val slice = target.substring(displayed, displayed + n)
      displayed += n
      val s = dom.document.createElement("span")
      s.setAttribute("class", "tw-ch")
      s.textContent = slice
      tailEl.appendChild(s)
      fadeQ.enqueue(new Fading(s, slice, ts))

    def expireFaded(ts: Double): Unit =
      while fadeQ.nonEmpty && (ts - fadeQ.front.ts) >= FadeMs do
        val f = fadeQ.dequeue()
        settledNode.appendData(f.text)
        tailEl.removeChild(f.el)

    def step(ts: Double): Unit =
      val dt = if lastTs < 0 then 0.0 else math.min(math.max((ts - lastTs) / 1000.0, 0.0), 0.05)
      lastTs = ts
      val backlog = target.length - displayed
      if backlog > 0 then
        val cps = math.min(math.max(backlog * CatchUp, MinCps), MaxCps)
        val want = cps * dt + carry
        val n = math.min(math.floor(want).toInt, MaxPerFrame)
        carry = want - n
        if n > 0 then
          revealRun(n, ts)
          follow()
      expireFaded(ts)
      if (target.length - displayed) > 0 || fadeQ.nonEmpty then
        rafId = dom.window.requestAnimationFrame((t: Double) => step(t))
      else
        rafId = -1
        lastTs = -1.0

    def ensureLoop(): Unit =
      if animating && !reducedMotion && rafId < 0 && ((target.length - displayed) > 0 || fadeQ.nonEmpty) then
        lastTs = -1.0
        rafId = dom.window.requestAnimationFrame((t: Double) => step(t))

    span(
      cls := "tw",
      onMountCallback { ctx =>
        val el = ctx.thisNode.ref
        if !built then
          el.appendChild(settledNode)
          el.appendChild(tailEl)
          if caret then el.appendChild(caretEl)
          built = true
        seeded = false
        // Subscribe to `animate` first so `animating` is current when `text` seeds.
        animate.foreach { a =>
          animating = a
          if reducedMotion then showCaret(a)
          else if !a then snapToEnd()
          else { showCaret(true); ensureLoop() }
        }(using ctx.owner)
        text.foreach { t =>
          target = t
          if !seeded then
            seeded = true
            carry = 0.0
            if reducedMotion then
              settledNode.data = t
              displayed = t.length
            else
              // A pre-existing row (panel still mounting) shows in full at once;
              // a freshly born streaming row types from the start.
              displayed = if animating && isPrimed() then 0 else t.length
              settledNode.data = t.substring(0, displayed)
              clearTail()
              if animating then { showCaret(true); ensureLoop() }
          else if reducedMotion then
            settledNode.data = t
            displayed = t.length
          else ensureLoop()
        }(using ctx.owner)
      },
      onUnmountCallback(_ => cancelLoop())
    )
