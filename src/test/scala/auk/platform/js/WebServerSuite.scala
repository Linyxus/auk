package auk.platform.js

/** The pure routing helpers behind [[WebServer]] (the full server is covered
  * end-to-end by `auk.runtime.WorkflowWebServerSuite`). */
class WebServerSuite extends munit.FunSuite:

  test("safeJoin joins normal paths and rejects traversal via .."):
    assertEquals(WebServer.safeJoin("/site", "index.html"), Some("/site/index.html"))
    assertEquals(WebServer.safeJoin("/site", "a/b.js"), Some("/site/a/b.js"))
    assertEquals(WebServer.safeJoin("/site", "./main.js"), Some("/site/main.js"))
    assertEquals(WebServer.safeJoin("/site", "../secret"), None)
    assertEquals(WebServer.safeJoin("/site", "a/../../etc/passwd"), None)

  test("contentType maps known extensions and falls back to octet-stream"):
    assertEquals(WebServer.contentType("/x/index.html"), "text/html; charset=utf-8")
    assertEquals(WebServer.contentType("/x/main.js"), "text/javascript; charset=utf-8")
    assertEquals(WebServer.contentType("/x/styles.css"), "text/css; charset=utf-8")
    assertEquals(WebServer.contentType("/x/main.js.map"), "application/json; charset=utf-8")
    assertEquals(WebServer.contentType("/x/font.woff2"), "font/woff2")
    assertEquals(WebServer.contentType("/x/font.woff"), "font/woff")
    assertEquals(WebServer.contentType("/x/OFL.txt"), "text/plain; charset=utf-8")
    assertEquals(WebServer.contentType("/x/data.bin"), "application/octet-stream")
