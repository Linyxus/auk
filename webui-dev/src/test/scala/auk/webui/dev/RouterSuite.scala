package auk.webui.dev

class RouterSuite extends munit.FunSuite:

  test("GET / routes to Static index.html under root"):
    assertEquals(Router.route("GET", "/", "/site"), Route.Static("/site/index.html"))

  test("GET a static asset routes to Static under root"):
    assertEquals(Router.route("GET", "/styles.css", "/site"), Route.Static("/site/styles.css"))

  test("GET /events?scenario=loop routes to Events(loop)"):
    assertEquals(Router.route("GET", "/events?scenario=loop", "/site"), Route.Events("loop"))

  test("GET /events with no scenario defaults to fanout"):
    assertEquals(Router.route("GET", "/events", "/site"), Route.Events("fanout"))

  test("a non-GET method is NotFound"):
    assert(Router.route("POST", "/", "/site").isInstanceOf[Route.NotFound])

  test("a path containing .. is rejected as NotFound (traversal guard)"):
    assert(Router.route("GET", "/../secret", "/site").isInstanceOf[Route.NotFound])
    assert(Router.route("GET", "/a/../../etc/passwd", "/site").isInstanceOf[Route.NotFound])

  test("contentType maps known extensions and falls back to octet-stream"):
    assertEquals(Router.contentType("/x/index.html"), "text/html; charset=utf-8")
    assertEquals(Router.contentType("/x/main.js"), "text/javascript; charset=utf-8")
    assertEquals(Router.contentType("/x/styles.css"), "text/css; charset=utf-8")
    assertEquals(Router.contentType("/x/main.js.map"), "application/json; charset=utf-8")
    assertEquals(Router.contentType("/x/font.woff2"), "font/woff2")
    assertEquals(Router.contentType("/x/font.woff"), "font/woff")
    assertEquals(Router.contentType("/x/OFL.txt"), "text/plain; charset=utf-8")
    assertEquals(Router.contentType("/x/data.bin"), "application/octet-stream")

  test("an /api path routes to Api with its decoded segments"):
    assertEquals(
      Router.route("GET", "/api/loop/tokenizer-p99/transcript/gen-3-worker", "/site"),
      Route.Api(List("loop", "tokenizer-p99", "transcript", "gen-3-worker")))
    assertEquals(
      Router.route("GET", "/api/loop/l/diff/6/2", "/site"),
      Route.Api(List("loop", "l", "diff", "6", "2")))

  test("an api segment is percent-decoded, as the browser wrote it"):
    assertEquals(Router.route("GET", "/api/loop/a%20b/transcript/x", "/site"), Route.Api(List("loop", "a b", "transcript", "x")))

  test("undecodable api text is taken literally rather than failing the request"):
    assertEquals(Router.route("GET", "/api/loop/a%zz", "/site"), Route.Api(List("loop", "a%zz")))

  test("an empty api path is an Api route with nothing in it (which answers nothing)"):
    assertEquals(Router.route("GET", "/api/", "/site"), Route.Api(Nil))

  test("an api path is never looked for on disk"):
    assert(!Router.route("GET", "/api/loop/x", "/site").isInstanceOf[Route.Static])

  test("splitQuery and param parse query parameters"):
    assertEquals(Router.splitQuery("/events?scenario=loop&x=1"), ("/events", "scenario=loop&x=1"))
    assertEquals(Router.param("scenario=loop&x=1", "scenario"), Some("loop"))
    assertEquals(Router.param("scenario=loop", "missing"), None)
