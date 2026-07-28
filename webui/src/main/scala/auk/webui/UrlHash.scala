package auk.webui

import scala.scalajs.js

/** The URL fragment that carries the selected run id (`…/#wf-123-4`), so a reload
  * keeps the selection and a pasted link opens on that run.
  *
  * DOM-free on purpose — [[Main]] hands it `window.location.hash` and takes back a
  * fragment to write, and both directions are unit-tested without a browser. */
object UrlHash:

  /** The run id in a `window.location.hash` value: `""` and `"#"` yield none, the
    * leading `#` is dropped and the rest is URI-decoded. Undecodable text (a
    * hand-mangled `%`) is taken literally rather than failing the page's boot. */
  def parse(hash: String): Option[String] =
    val raw = if hash.startsWith("#") then hash.substring(1) else hash
    Option(raw).filter(_.nonEmpty).map(r => try js.URIUtils.decodeURIComponent(r) catch case _: js.JavaScriptException => r)

  /** The fragment for a selection: an encoded `#id`, or nothing at all when no run
    * is selected (the URL is then left bare rather than ending in a stray `#`). */
  def format(run: Option[String]): String =
    run.filter(_.nonEmpty).fold("")(r => "#" + js.URIUtils.encodeURIComponent(r))
