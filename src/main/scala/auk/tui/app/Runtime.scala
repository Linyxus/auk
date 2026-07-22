package auk.tui.app

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.AsyncOperations.sleep
import gears.async.default.given
import auk.tui.render.{Renderer, Terminal}

final case class RuntimeConfig(
    frameMs: Long = 16,
    quitKey: Key = Key.Ctrl('Q'),
    widthPollMs: Long = 500,
    enableMouse: Boolean = false
)

/** The gears-based Elm runtime.
  *
  * The whole event system is gears channels multiplexed by one `Async.select`:
  * parsed key events, internal messages (from `Cmd` and timer fibers), frame
  * ticks, and any app-subscribed channels (e.g. the engine event stream). State
  * lives in the single loop fiber — no locks. Rendering is dirty-flagged and
  * frame-rate-capped, so event bursts coalesce into one repaint and an idle
  * screen never repaints.
  *
  * Single-threaded on JS: fibers interleave cooperatively, so the cross-fiber
  * flags below are plain `var`s rather than atomics.
  */
object Runtime:
  /** Erased form of a `Sub.OnChannel` for heterogeneous storage. */
  private final case class ChannelSub[Msg](channel: ReadableChannel[Any], toMsg: Any => Msg, onClosed: Msg)

  def run[State, Msg](app: App[State, Msg], terminal: Terminal, config: RuntimeConfig)(using Async.Spawn): Unit =
      val renderer = Renderer(terminal.write, terminal.altScreenSetup, terminal.altScreenTeardown)

      // ---- the event bus: gears channels ----
      val keys = UnboundedChannel[Key]()
      val msgs = UnboundedChannel[Msg]()
      val frame = UnboundedChannel[Unit]()

      // ---- cross-fiber state (single-threaded JS: plain vars) ----
      var quit = false
      var resizePending = false
      val (initW, initR) = terminal.size()
      var curWidth = initW
      var curRows = initR

      // ---- loop-local state (only the loop fiber touches these) ----
      val (initState, initCmd) = app.init
      var state = initState
      var dirty = true
      var flushed = 0
      var committedEpoch: Long = 0
      // A resize seen during a fullscreen episode is consumed immediately (the
      // fullscreen path repaints itself via its dim check) but the inline
      // full-transcript reflow it implies is deferred to the eventual inline
      // return via this flag — committed lines were emitted once at the old width
      // and don't reflow, so the return must reprint them.
      var inlineResetPending = false
      var keyHandler: Key => Option[Msg] = _ => None
      var channelSubs: List[ChannelSub[Msg]] = Nil
      var closedChannels = Set.empty[ReadableChannel[?]]
      var timers = Map.empty[(Long, Msg), Future[Unit]]

      // ---- Sub flattening ----
      def collectKeys(sub: Sub[Msg]): List[Key => Option[Msg]] = sub match
        case Sub.Batch(ss)     => ss.flatMap(collectKeys)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil

      def collectTimers(sub: Sub[Msg]): List[(Long, Msg)] = sub match
        case Sub.Batch(ss)            => ss.flatMap(collectTimers)
        case Sub.TimeEveryMs(ms, msg) => List((ms, msg))
        case _                        => Nil

      def collectChannels(sub: Sub[Msg]): List[ChannelSub[Msg]] = sub match
        case Sub.Batch(ss) => ss.flatMap(collectChannels)
        case Sub.OnChannel(ch, toMsg, onClosed) =>
          List(ChannelSub(ch.asInstanceOf[ReadableChannel[Any]], toMsg.asInstanceOf[Any => Msg], onClosed))
        case _ => Nil

      def reconcile(): Unit =
        val sub = app.subscriptions(state)
        val handlers = collectKeys(sub)
        keyHandler = k => handlers.foldLeft(Option.empty[Msg])((acc, h) => acc.orElse(h(k)))
        channelSubs = collectChannels(sub)
        val wanted = collectTimers(sub).toSet
        for (k, f) <- timers if !wanted.contains(k) do f.cancel()
        timers = timers.filter((k, _) => wanted.contains(k))
        for k <- wanted if !timers.contains(k) do
          val (ms, msg) = k
          val f = Future:
            while !quit do
              sleep(ms)
              // A transient send failure (e.g. a channel closing during teardown)
              // must not silently kill the timer — that would freeze whatever it
              // drives (the typewriter reveal, the spinner).
              if !quit then
                try msgs.sendImmediately(msg)
                catch case _: Throwable => ()
          timers = timers.updated(k, f)

      // ---- Cmd execution (fibers in this scope, cancelled on quit) ----
      def exec(cmd: Cmd[Msg]): Unit = cmd match
        case Cmd.None       => ()
        case Cmd.Quit       => quit = true
        case Cmd.Refresh =>
          // Repaint from scratch: drop the alt-screen diff baseline (the next
          // fullscreen frame clears and fully repaints) and ride the inline
          // return through the same full-transcript reset a resize uses.
          renderer.invalidateFullscreen()
          inlineResetPending = true
          dirty = true
        case Cmd.Batch(cs)  => cs.foreach(exec)
        case Cmd.Fire(eff)  => Future { eff() }; ()
        case Cmd.Task(work, toMsg) =>
          Future {
            val result = try Right(work()) catch case e: Throwable => Left(s"$e")
            msgs.sendImmediately(toMsg(result))
          }
          ()

      def applyMsg(m: Msg): Unit =
        val (s2, cmd) = app.update(m, state)
        state = s2
        dirty = true
        exec(cmd)
        reconcile()

      def render(): Unit =
        val width = curWidth
        val rows = curRows
        // Consume a pending resize here, exactly once, in either mode: inline it
        // forces the full-transcript reflow now; fullscreen it defers that reflow
        // to the eventual inline return (the fullscreen path repaints itself via
        // renderFullscreen's own dim check).
        val resized = resizePending
        resizePending = false
        if resized then inlineResetPending = true
        val screen = app.view(state, Viewport(width, rows))
        screen.fullscreen match
          case Some(el) =>
            // Fullscreen takeover: paint ONLY this element into the alt-screen
            // buffer; the committed-flush bookkeeping stays frozen and the
            // transcript reflow rides `inlineResetPending` to the return frame.
            renderer.renderFullscreen(width, rows, Layout.lay(el, width))
            dirty = false
          case None =>
            // Inline: reprint the whole transcript on a pending reset — a resize
            // during this frame or a prior fullscreen episode, or an epoch bump —
            // which also cleanly re-establishes the primary buffer after an episode.
            val resetCommitted = inlineResetPending || screen.committedEpoch != committedEpoch
            inlineResetPending = false
            if resetCommitted then
              flushed = 0
              committedEpoch = screen.committedEpoch
            val committed = screen.committed
            val fresh = if committed.length > flushed then committed.drop(flushed) else Vector.empty
            val committedLines = fresh.flatMap(Layout.lay(_, width))
            if committed.length > flushed then flushed = committed.length
            val liveAll = Layout.lay(screen.live, width)
            val overlay = screen.overlay.map(Layout.lay(_, width))
            val maxLive = math.max(1, rows - 1)
            val live = if liveAll.length > maxLive then liveAll.takeRight(maxLive) else liveAll
            renderer.render(width, committedLines, live, hardReset = resetCommitted, overlay = overlay)
            dirty = false

      // Paint a pending change. A resize drives exactly one render in either mode
      // (render() always consumes `resizePending`), so there is no per-frame spin.
      def renderIfNeeded(): Unit =
        if (dirty || resizePending) && !quit then render()

      // Fold one key into the state (or arm quit). Reused by the key-drain below.
      def handleKey(k: Key): Unit =
        if k == config.quitKey then quit = true
        else keyHandler(k).foreach(applyMsg)

      // ---- startup ----
      terminal.enterRawMode()
      terminal.hideCursor()
      if config.enableMouse then terminal.enableMouse()

      // Input arrives push-style: stdin bytes are parsed into keys as they come.
      val parser = KeyParser()
      terminal.onByte(b => if b >= 0 then parser.feed(b).foreach(keys.sendImmediately))

      // Push-style resize: the OS resize signal nudges an immediate repaint rather
      // than waiting for the poller's next tick. Single-threaded JS under gears —
      // this callback runs on the loop's fiber like `onByte`, so the plain `var`
      // writes below are safe (no locking). The 500ms poller stays as a fallback
      // for terminals that don't emit the signal (tmux, some PTYs).
      terminal.onResize: () =>
        try
          val (w, r) = terminal.size()
          if w != curWidth || r != curRows then
            curWidth = w
            curRows = r
            resizePending = true
            try frame.sendImmediately(()) catch case _: Throwable => ()
        catch case _: Throwable => ()

      // The frame heartbeat drives every repaint; it must never die silently, or
      // the screen freezes even as state keeps updating. Guard its send.
      val ticker = Future:
        while !quit do
          sleep(config.frameMs)
          if !quit then
            try frame.sendImmediately(())
            catch case _: Throwable => ()

      val poller = Future:
        while !quit do
          sleep(config.widthPollMs)
          try
            val (w, r) = terminal.size()
            if w != curWidth || r != curRows then
              curWidth = w
              curRows = r
              resizePending = true
          catch case _: Throwable => ()

      exec(initCmd)
      reconcile()
      render()

      // ---- main select loop ----
      while !quit do
        val keyCase = keys.readSource.handle:
          case Right(k) =>
            handleKey(k)
            // Drain keys already buffered (paste / fast typing) so input coalesces
            // into one paint instead of one render per character, then repaint
            // immediately rather than waiting up to frameMs for the next tick.
            var pending = keys.readSource.poll()
            while pending.exists(_.isRight) && !quit do
              pending.foreach { case Right(k2) => handleKey(k2); case _ => () }
              pending = if quit then None else keys.readSource.poll()
            renderIfNeeded()
          case Left(_) => ()
        val msgCase = msgs.readSource.handle:
          case Right(m) => applyMsg(m)
          case Left(_)  => ()
        val frameCase = frame.readSource.handle(_ => renderIfNeeded())
        val chanCases = channelSubs.filterNot(c => closedChannels.contains(c.channel)).map: c =>
          c.channel.readSource.handle:
            case Right(a) => applyMsg(c.toMsg(a))
            case Left(_) =>
              closedChannels = closedChannels + c.channel
              applyMsg(c.onClosed)
        Async.select((keyCase :: msgCase :: frameCase :: chanCases)*)

      // ---- teardown ----
      quit = true
      timers.values.foreach(_.cancel())
      ticker.cancel()
      poller.cancel()
      keys.close()
      msgs.close()
      frame.close()
      // Unwind order: mouse off → alt-screen exit → show cursor → close (raw off).
      // Mouse reporting is disabled before leaving the alt buffer so no stray
      // reports arrive during the restore.
      terminal.disableMouse()
      // Leave the alt buffer if we quit straight from a fullscreen view, so the
      // user's terminal is never stranded in the alternate screen. When active this
      // also pops the alt buffer's kitty keyboard enhancement (before `?1049l`);
      // terminal.close() below pops the SEPARATE main-buffer push from enterRawMode.
      // Two balanced push/pop pairs — one per screen buffer — which is correct.
      renderer.exitFullscreen()
      terminal.showCursor()
      terminal.close()
