package auk.webui

import scala.scalajs.js

/** The URL fragment that carries what the page is looking at — `…/#wf-123-4` for a
  * workflow run, `…/#loop/better-readme` for a loop — so a reload keeps the
  * selection and a pasted link opens on the same thing.
  *
  * The two grammars cannot collide, because a run id is written percent-encoded and
  * an encoded id has no bare `/` in it: the `loop/` prefix is therefore a marker no
  * run can wear by accident.
  *
  * DOM-free on purpose — [[Main]] hands it `window.location.hash` and takes back a
  * fragment to write, and both directions are unit-tested without a browser. */
object UrlHash:

  private val LoopPrefix = "loop/"

  /** The target in a `window.location.hash` value: `""` and `"#"` yield none, the
    * leading `#` is dropped and the rest is URI-decoded. Undecodable text (a
    * hand-mangled `%`) is taken literally rather than failing the page's boot. */
  def parse(hash: String): Option[Target] =
    val raw = if hash.startsWith("#") then hash.substring(1) else hash
    if raw.startsWith(LoopPrefix) then
      Option(decode(raw.substring(LoopPrefix.length))).filter(_.nonEmpty).map(Target.Loop.apply)
    else Option(raw).filter(_.nonEmpty).map(r => Target.Run(decode(r)))

  /** The fragment for a selection, or nothing at all when nothing is selected (the
    * URL is then left bare rather than ending in a stray `#`). */
  def format(target: Option[Target]): String = target match
    case Some(Target.Run(id)) if id.nonEmpty  => "#" + js.URIUtils.encodeURIComponent(id)
    case Some(Target.Loop(id)) if id.nonEmpty => "#" + LoopPrefix + js.URIUtils.encodeURIComponent(id)
    case _                                    => ""

  private def decode(raw: String): String =
    try js.URIUtils.decodeURIComponent(raw)
    catch case _: js.JavaScriptException => raw
