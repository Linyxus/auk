package auk.webui

class UrlHashSuite extends munit.FunSuite:

  test("parse strips the leading '#' and reads the run id"):
    assertEquals(UrlHash.parse("#wf-123-4"), Some("wf-123-4"))

  test("parse accepts a fragment without the '#'"):
    assertEquals(UrlHash.parse("wf-1-2"), Some("wf-1-2"))

  test("parse yields no run for an empty or bare fragment"):
    assertEquals(UrlHash.parse(""), None)
    assertEquals(UrlHash.parse("#"), None)

  test("parse URI-decodes the run id"):
    assertEquals(UrlHash.parse("#wf%20a%2Fb"), Some("wf a/b"))

  test("parse takes undecodable text literally instead of throwing"):
    assertEquals(UrlHash.parse("#wf-%zz"), Some("wf-%zz"))

  test("format writes an encoded fragment for a selected run"):
    assertEquals(UrlHash.format(Some("wf-123-4")), "#wf-123-4")
    assertEquals(UrlHash.format(Some("wf a/b")), "#wf%20a%2Fb")

  test("format writes nothing when no run is selected"):
    assertEquals(UrlHash.format(None), "")
    assertEquals(UrlHash.format(Some("")), "")

  test("format and parse round-trip an id with awkward characters"):
    val id = "wf a/b#c?d"
    assertEquals(UrlHash.parse(UrlHash.format(Some(id))), Some(id))
