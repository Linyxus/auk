package auk.webui.dev

/** What a request resolves to. */
enum Route:
  case Static(path: String)
  case Events(scenario: String)
  /** An on-demand payload, as the path segments under `/api/` — the same shape the
    * real host's API area has, so the browser cannot tell the two apart. */
  case Api(segments: List[String])
  case NotFound(reason: String)

/** Pure HTTP routing for the mock server: `(method, url, rootDir) -> Route`, plus
  * content-type and a traversal-safe path join. No I/O, so it is unit-tested
  * directly. */
object Router:

  /** Everything under here is answered from the scenario rather than from disk. */
  private val ApiPath = "/api/"

  def route(method: String, url: String, rootDir: String): Route =
    if method != "GET" then Route.NotFound(s"method $method")
    else
      val (path, query) = splitQuery(url)
      if path == "/events" then Route.Events(param(query, "scenario").getOrElse("fanout"))
      else if path.startsWith(ApiPath) then
        Route.Api(path.substring(ApiPath.length).split("/").iterator.filter(_.nonEmpty).map(decode).toList)
      else
        val rel = if path == "/" || path.isEmpty then "index.html" else path.stripPrefix("/")
        safeJoin(rootDir, rel) match
          case Some(p) => Route.Static(p)
          case None    => Route.NotFound("path traversal")

  def contentType(path: String): String =
    val dot = path.lastIndexOf('.')
    val ext = if dot >= 0 then path.substring(dot + 1) else ""
    ext match
      case "html"  => "text/html; charset=utf-8"
      case "js"    => "text/javascript; charset=utf-8"
      case "map"   => "application/json; charset=utf-8"
      case "css"   => "text/css; charset=utf-8"
      case "json"  => "application/json; charset=utf-8"
      case "txt"   => "text/plain; charset=utf-8"
      case "woff2" => "font/woff2"
      case "woff"  => "font/woff"
      case _       => "application/octet-stream"

  private[dev] def splitQuery(url: String): (String, String) =
    val i = url.indexOf('?')
    if i < 0 then (url, "") else (url.substring(0, i), url.substring(i + 1))

  /** The browser percent-encodes each API path segment, so each one is decoded on
    * the way back. Undecodable text is taken literally rather than failing the
    * request — the segment simply matches nothing. */
  private[dev] def decode(seg: String): String =
    try scala.scalajs.js.URIUtils.decodeURIComponent(seg)
    catch case _: scala.scalajs.js.JavaScriptException => seg

  private[dev] def param(query: String, key: String): Option[String] =
    query.split("&").iterator
      .map(_.split("=", 2))
      .collectFirst { case Array(k, v) if k == key && v.nonEmpty => v }

  /** Join `rel` onto `root`, rejecting any path that escapes via `..`. */
  private[dev] def safeJoin(root: String, rel: String): Option[String] =
    val segs = rel.split("/").iterator.filter(s => s.nonEmpty && s != ".").toVector
    if segs.contains("..") then None
    else Some((root.stripSuffix("/") +: segs).mkString("/"))
