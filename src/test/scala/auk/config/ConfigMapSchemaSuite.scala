package auk.config

import scala.collection.immutable.VectorMap

case class Endpoint(
    url: String,
    retries: Option[Int],
    headers: VectorMap[String, String]
) derives ConfigSchema

case class Registry(endpoints: VectorMap[String, Endpoint]) derives ConfigSchema

case class Labels(tags: VectorMap[String, String]) derives ConfigSchema

/** The open-section schema: `VectorMap[String, A]` decodes every child one
  * segment below its path, in on-disk order.
  */
class ConfigMapSchemaSuite extends munit.FunSuite:

  private def parseOk[A](text: String)(using ConfigSchema[A]): A =
    Config.parse[A](text) match
      case Right(a)   => a
      case Left(errs) => fail(s"unexpected errors: ${errs.map(_.render).mkString("; ")}")

  private def parseErr[A](text: String)(using ConfigSchema[A]): List[ConfigError] =
    Config.parse[A](text) match
      case Left(errs) => errs
      case Right(a)   => fail(s"expected errors but got: $a")

  test("children decode in the order they appear on disk") {
    val cfg = parseOk[Labels]("""tags.zebra = z
                                |tags.apple = a
                                |tags.mango = m""".stripMargin)
    assertEquals(cfg.tags.keys.toList, List("zebra", "apple", "mango"))
    assertEquals(cfg.tags("apple"), "a")
  }

  test("a child named by several keys appears once, at its first key") {
    val cfg = parseOk[Registry]("""endpoints.b.url = second
                                  |endpoints.a.url = first
                                  |endpoints.b.retries = 3""".stripMargin)
    assertEquals(cfg.endpoints.keys.toList, List("b", "a"))
    assertEquals(cfg.endpoints("b").retries, Some(3))
  }

  test("an absent section decodes to the empty map, not an error") {
    assertEquals(parseOk[Labels](""), Labels(VectorMap.empty))
  }

  test("a value at the section's own path is an error") {
    val errs = parseErr[Labels]("tags = oops")
    assertEquals(errs.length, 1)
    assertEquals(errs.head.render, "line 1: tags: expected a section, found a value")
  }

  test("child errors accumulate and carry the child name") {
    val errs = parseErr[Registry]("""[endpoints.a]
                                    |retries = 1
                                    |[endpoints.b]
                                    |retries = 2""".stripMargin)
    assertEquals(errs.length, 2)
    assertEquals(
      errs.map(_.path).toSet,
      Set(List("endpoints", "a", "url"), List("endpoints", "b", "url"))
    )
  }

  test("a bad leaf inside one child does not hide a bad leaf in another") {
    val errs = parseErr[Registry]("""endpoints.a.url = x
                                    |endpoints.a.retries = nope
                                    |endpoints.b.url = y
                                    |endpoints.b.retries = also-nope""".stripMargin)
    assertEquals(errs.map(_.render.take(6)).toSet, Set("line 2", "line 4"))
  }

  test("a map of records nests a further map section") {
    val cfg = parseOk[Registry]("""[endpoints.primary]
                                  |url = https://one
                                  |headers.Accept = json
                                  |headers.X-Trace = "  padded  "
                                  |[endpoints.backup]
                                  |url = https://two""".stripMargin)
    assertEquals(cfg.endpoints.keys.toList, List("primary", "backup"))
    val primary = cfg.endpoints("primary")
    assertEquals(primary.headers.keys.toList, List("Accept", "X-Trace"))
    assertEquals(primary.headers("X-Trace"), "  padded  ")
    assertEquals(cfg.endpoints("backup").headers, VectorMap.empty[String, String])
  }

  test("a nested map section is free-form, and does not read as a typo") {
    // The strictness below must not fire on the open `headers` section: its
    // child names come from the document, so none of them can be misspelled.
    val cfg = parseOk[Registry]("endpoints.a.url = x\nendpoints.a.headers.Anything-At-All = y")
    assertEquals(cfg.endpoints("a").headers.keys.toList, List("Anything-At-All"))
  }

  test("a misspelled field inside a child is an unknown key") {
    // Otherwise a typo'd OPTIONAL field would decode cleanly and do nothing.
    val errs = parseErr[Registry]("endpoints.a.url = x\nendpoints.a.retrys = 1")
    assertEquals(errs.map(_.render), List("line 2: endpoints.a.retrys: unknown key"))
  }

  test("a child reports its missing field and its unknown key together") {
    val errs = parseErr[Registry]("endpoints.a.urll = x")
    assertEquals(
      errs.map(_.render),
      List("endpoints.a.url: missing required value", "line 1: endpoints.a.urll: unknown key")
    )
  }

  test("a key below a leaf child is an unknown key") {
    val errs = parseErr[Labels]("tags.a.b = x")
    assertEquals(
      errs.map(_.render),
      List("tags.a: missing required value", "line 1: tags.a.b: unknown key")
    )
  }

  test("an unknown key beside the section is still reported") {
    val errs = parseErr[Registry]("endpoints.a.url = x\nbogus = 1")
    assertEquals(errs.length, 1)
    assert(errs.head.message.contains("unknown key 'bogus'"), errs.head.render)
  }
