package auk.tui.app

/** An effect the runtime performs after an `update`, optionally producing a
  * follow-up message. Mirrors the layoutz surface the app was written against
  * (by-name `fire`, curried by-name `task`) so call sites port unchanged. */
enum Cmd[+Msg]:
  case None
  case Quit
  /** Repaint the screen from scratch: the runtime drops the renderer's diff
    * baselines (alt-screen surface; inline rides the resize hard-reset), so a
    * terminal whose real grid diverged from the model — a terminal bug, line
    * noise, a rogue writer on the tty — is squared up again. */
  case Refresh
  case Batch[Msg](cmds: List[Cmd[Msg]]) extends Cmd[Msg]
  /** A side effect with no resulting message. */
  case Fire(effect: () => Unit) extends Cmd[Nothing]
  /** Run `work` on a fiber and deliver its result (or the error message) as a
    * message. */
  case Task[A, Msg](work: () => A, toMsg: Either[String, A] => Msg) extends Cmd[Msg]

object Cmd:
  def none[Msg]: Cmd[Msg] = Cmd.None
  def quit[Msg]: Cmd[Msg] = Cmd.Quit
  def refresh[Msg]: Cmd[Msg] = Cmd.Refresh
  def batch[Msg](cmds: Cmd[Msg]*): Cmd[Msg] = Cmd.Batch(cmds.toList)
  def fire[Msg](effect: => Unit): Cmd[Msg] = Cmd.Fire(() => effect)
  def task[A, Msg](work: => A)(toMsg: Either[String, A] => Msg): Cmd[Msg] =
    Cmd.Task(() => work, toMsg)
