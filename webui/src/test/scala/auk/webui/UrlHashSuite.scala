package auk.webui

class UrlHashSuite extends munit.FunSuite:

  test("parse strips the leading '#' and reads the run id"):
    assertEquals(UrlHash.parse("#wf-123-4"), Some(Target.Run("wf-123-4")))

  test("parse accepts a fragment without the '#'"):
    assertEquals(UrlHash.parse("wf-1-2"), Some(Target.Run("wf-1-2")))

  test("parse yields nothing for an empty or bare fragment"):
    assertEquals(UrlHash.parse(""), None)
    assertEquals(UrlHash.parse("#"), None)

  test("parse URI-decodes the run id"):
    assertEquals(UrlHash.parse("#wf%20a%2Fb"), Some(Target.Run("wf a/b")))

  test("parse takes undecodable text literally instead of throwing"):
    assertEquals(UrlHash.parse("#wf-%zz"), Some(Target.Run("wf-%zz")))

  test("format writes an encoded fragment for a selected run"):
    assertEquals(UrlHash.format(Some(Target.Run("wf-123-4"))), "#wf-123-4")
    assertEquals(UrlHash.format(Some(Target.Run("wf a/b"))), "#wf%20a%2Fb")

  test("format writes nothing when nothing is selected"):
    assertEquals(UrlHash.format(None), "")
    assertEquals(UrlHash.format(Some(Target.Run(""))), "")
    assertEquals(UrlHash.format(Some(Target.Loop(""))), "")

  test("format and parse round-trip a run id with awkward characters"):
    val id = "wf a/b#c?d"
    assertEquals(UrlHash.parse(UrlHash.format(Some(Target.Run(id)))), Some(Target.Run(id)))

  // -- loops -------------------------------------------------------------------

  test("a 'loop/' prefix names a loop"):
    assertEquals(UrlHash.parse("#loop/tokenizer-p99"), Some(Target.Loop("tokenizer-p99")))
    assertEquals(UrlHash.format(Some(Target.Loop("tokenizer-p99"))), "#loop/tokenizer-p99")

  test("a loop id is URI-decoded and re-encoded"):
    assertEquals(UrlHash.parse("#loop/a%20b"), Some(Target.Loop("a b")))
    assertEquals(UrlHash.format(Some(Target.Loop("a b"))), "#loop/a%20b")

  test("format and parse round-trip a loop id"):
    val id = "opt/tokenizer p99"
    assertEquals(UrlHash.parse(UrlHash.format(Some(Target.Loop(id)))), Some(Target.Loop(id)))

  /** The two grammars cannot collide: a run id is written percent-encoded, so an
    * encoded id never contains the bare `/` the loop prefix needs. */
  test("a run id that looks like a loop path round-trips as a run"):
    val id = "loop/not-really"
    val fragment = UrlHash.format(Some(Target.Run(id)))
    assertEquals(fragment, "#loop%2Fnot-really")
    assertEquals(UrlHash.parse(fragment), Some(Target.Run(id)))

  test("a bare 'loop/' with nothing after it names nothing"):
    assertEquals(UrlHash.parse("#loop/"), None)
