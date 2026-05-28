package auk.tui.app

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.AsyncOperations.sleep
import gears.async.default.given
import auk.tui.render.{Renderer, Terminal}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

final case class RuntimeConfig(
    frameMs: Long = 16,
    quitKey: Key = Key.Ctrl('Q'),
    widthPollMs: Long = 500
)

/** The gears-based Elm runtime.
  *
  * The whole event system is gears channels multiplexed by one `Async.select`:
  * parsed key events, internal messages (from `Cmd` and timer fibers), frame
  * ticks, and any app-subscribed channels (e.g. the engine event stream). State
  * lives in the single loop fiber — no locks. Rendering is dirty-flagged and
  * frame-rate-capped, so event bursts coalesce into one repaint and an idle
  * screen never repaints.
  */
object Runtime:
  /** Erased form of a `Sub.OnChannel` for heterogeneous storage. */
  private final case class ChannelSub[Msg](channel: ReadableChannel[Any], toMsg: Any => Msg, onClosed: Msg)

  def run[State, Msg](app: App[State, Msg], terminal: Terminal, config: RuntimeConfig): Unit =
    Async.blocking:
      val renderer = Renderer(terminal.write)

      // ---- the event bus: gears channels ----
      val keys = UnboundedChannel[Key]()
      val msgs = UnboundedChannel[Msg]()
      val frame = UnboundedChannel[Unit]()

      // ---- cross-fiber state ----
      val quit = new AtomicBoolean(false)
      val resizePending = new AtomicBoolean(false)
      val (initW, initR) = terminal.size()
      val curWidth = new AtomicInteger(initW)
      val curRows = new AtomicInteger(initR)

      // ---- loop-local state (only the loop fiber touches these) ----
      val (initState, initCmd) = app.init
      var state = initState
      var dirty = true
      var flushed = 0
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
            while !quit.get() do
              sleep(ms)
              msgs.sendImmediately(msg)
          timers = timers.updated(k, f)

      // ---- Cmd execution (fibers in this scope, cancelled on quit) ----
      def exec(cmd: Cmd[Msg]): Unit = cmd match
        case Cmd.None       => ()
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

      def render(fullReset: Boolean = false): Unit =
        // On a terminal resize, reprint the whole transcript (committed lines
        // were emitted once at the old width and won't reflow): re-flush from 0.
        if fullReset then flushed = 0
        val width = curWidth.get()
        val rows = curRows.get()
        val screen = app.view(state)
        val committed = screen.committed
        val fresh = if committed.length > flushed then committed.drop(flushed) else Vector.empty
        val committedLines = fresh.flatMap(Layout.lay(_, width))
        if committed.length > flushed then flushed = committed.length
        val liveAll = Layout.lay(screen.live, width)
        val maxLive = math.max(1, rows - 1)
        val live = if liveAll.length > maxLive then liveAll.takeRight(maxLive) else liveAll
        renderer.render(width, committedLines, live, hardReset = fullReset)
        dirty = false

      // ---- startup ----
      terminal.enterRawMode()
      terminal.hideCursor()

      val reader = Future:
        val parser = KeyParser()
        try
          var b = terminal.readByte() // blocks until a key (parks the VT)
          while b >= 0 && !quit.get() do
            parser.feed(b).foreach(keys.sendImmediately)
            b = terminal.readByte()
        catch case _: Throwable => () // terminal closed during teardown

      val ticker = Future:
        while !quit.get() do
          sleep(config.frameMs)
          frame.sendImmediately(())

      val poller = Future:
        while !quit.get() do
          sleep(config.widthPollMs)
          val (w, r) = terminal.size()
          if w != curWidth.get() || r != curRows.get() then
            curWidth.set(w)
            curRows.set(r)
            resizePending.set(true)

      exec(initCmd)
      reconcile()
      render()

      // ---- main select loop ----
      while !quit.get() do
        val keyCase = keys.readSource.handle {
          case Right(k) =>
            if k == config.quitKey then quit.set(true)
            else keyHandler(k).foreach(applyMsg)
          case Left(_) => ()
        }
        val msgCase = msgs.readSource.handle {
          case Right(m) => applyMsg(m)
          case Left(_)  => ()
        }
        val frameCase = frame.readSource.handle { _ =>
          val resized = resizePending.getAndSet(false)
          if (dirty || resized) && !quit.get() then render(fullReset = resized)
        }
        val chanCases = channelSubs.filterNot(c => closedChannels.contains(c.channel)).map { c =>
          c.channel.readSource.handle {
            case Right(a) => applyMsg(c.toMsg(a))
            case Left(_) =>
              closedChannels = closedChannels + c.channel
              applyMsg(c.onClosed)
          }
        }
        Async.select((keyCase :: msgCase :: frameCase :: chanCases)*)

      // ---- teardown ----
      quit.set(true)
      timers.values.foreach(_.cancel())
      reader.cancel()
      ticker.cancel()
      poller.cancel()
      keys.close()
      msgs.close()
      frame.close()
      terminal.showCursor()
      terminal.close()
