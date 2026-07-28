package auk.runtime.skills

import auk.platform.Platform

/** Pure tests of the skill shape scanner: interface extraction and the `=` cut. */
class SkillCodeSuite extends munit.FunSuite:

  private def iface(id: String, code: String): String =
    SkillCode.interface(id, code) match
      case Right(s)  => s
      case Left(err) => fail(s"extraction failed: $err")

  test("brace-style object: public signatures kept, private members dropped"):
    val code =
      """object Greeter {
        |  def greet(name: String): String = s"hi, $name"
        |  private def secret: Int = 1
        |  val Base: String = "hi"
        |}""".stripMargin
    assertEquals(
      iface("Greeter", code),
      """object Greeter:
        |  def greet(name: String): String
        |  val Base: String""".stripMargin
    )

  test("colon-style object with an end marker"):
    val code =
      """object Colon:
        |  def a(x: Int): Int =
        |    x + 1
        |  var count: Int = 0
        |end Colon""".stripMargin
    assertEquals(
      iface("Colon", code),
      """object Colon:
        |  def a(x: Int): Int
        |  var count: Int""".stripMargin
    )

  test("a doc comment above a member rides along with it"):
    val code =
      """object Doc {
        |  /** Doubles a number. */
        |  def twice(x: Int): Int = x * 2
        |}""".stripMargin
    assertEquals(
      iface("Doc", code),
      """object Doc:
        |  /** Doubles a number. */
        |  def twice(x: Int): Int""".stripMargin
    )

  test("multi-line signatures are cut at the body's `=` and keep their shape"):
    val code =
      """object Multi {
        |  def wide(
        |      a: String,
        |      b: Int
        |  ): List[String] = List(a) ++ List(b.toString)
        |}""".stripMargin
    assertEquals(
      iface("Multi", code),
      """object Multi:
        |  def wide(
        |      a: String,
        |      b: Int
        |  ): List[String]""".stripMargin
    )

  test("function-type results and default parameters don't confuse the cut"):
    val code =
      """object Fn {
        |  val h: Int => Int = identity
        |  def d(x: Int = 3): Int = x
        |  def hof(g: Int => Int): Int = g(1)
        |}""".stripMargin
    assertEquals(
      iface("Fn", code),
      """object Fn:
        |  val h: Int => Int
        |  def d(x: Int = 3): Int
        |  def hof(g: Int => Int): Int""".stripMargin
    )

  test("braces inside string literals are ignored"):
    val code =
      """object Str {
        |  def json(k: String): String = s"{ $k: 1 }"
        |  def brace: String = "}"
        |}""".stripMargin
    assertEquals(
      iface("Str", code),
      """object Str:
        |  def json(k: String): String
        |  def brace: String""".stripMargin
    )

  test("leading imports are allowed; anything else before the object is not"):
    val ok =
      """import scala.collection.mutable
        |
        |object Imp {
        |  def m: mutable.Map[String, Int] = mutable.Map.empty
        |}""".stripMargin
    assert(SkillCode.interface("Imp", ok).isRight)
    val bad = "val stray = 1\nobject Imp {\n  def m: Int = 1\n}"
    assert(SkillCode.interface("Imp", bad).isLeft)

  test("a public member without an explicit result type is rejected"):
    val code = "object Bad {\n  def f(x: Int) = x\n}"
    SkillCode.interface("Bad", code) match
      case Left(err) => assert(err.contains("explicit result type"), err)
      case Right(s)  => fail(s"expected a rejection, got:\n$s")

  test("the object name must match the skill id"):
    SkillCode.interface("Right", "object Wrong {\n  def f: Int = 1\n}") match
      case Left(err) => assert(err.contains("Wrong"), err)
      case Right(s)  => fail(s"expected a rejection, got:\n$s")

  test("a second top-level definition is rejected"):
    val code = "object A {\n  def f: Int = 1\n}\nobject B {\n  def g: Int = 2\n}"
    assert(SkillCode.interface("A", code).isLeft)

  test("nested case classes keep their constructor signature and derives clause"):
    val code =
      """object Types {
        |  case class Row(path: String, hits: Int) derives CanEqual
        |  def rows: List[Row] = Nil
        |}""".stripMargin
    val out = iface("Types", code)
    assert(out.contains("case class Row(path: String, hits: Int) derives CanEqual"), out)
    assert(out.contains("def rows: List[Row]"), out)

class SkillBlobSuite extends munit.FunSuite:

  private val a = Skill("A", "a", "object A {\n  def one: Int = 1\n}", Nil)
  private val b = Skill("B", "b", "object B {\n  def two: Int = A.one + 1\n}", Nil)

  test("build concatenates with one blank separator and tracks spans"):
    val (blob, spans) = SkillBlob.build(List(a, b))
    assertEquals(spans, List(SkillBlob.Span("A", 1, 3), SkillBlob.Span("B", 5, 7)))
    val lines = blob.linesIterator.toVector
    assertEquals(lines(0), "object A {")
    assertEquals(lines(3), "")
    assertEquals(lines(4), "object B {")

  test("culprits maps diagnostic line references back to skills"):
    val spans = List(SkillBlob.Span("A", 1, 3), SkillBlob.Span("B", 5, 7))
    val diag =
      """-- [E006] Not Found Error: rs$line$2:6:17
        |6 |  def two: Int = A.one + 1
        |  |                 ^^^
        |  |                 Not found: A""".stripMargin
    assertEquals(SkillBlob.culprits(diag, spans), List("B"))

class SkillStoreSuite extends munit.FunSuite:

  private val root = s"target/skillstore-test-${java.lang.System.currentTimeMillis()}"
  private val store = SkillStore(root)

  override def afterAll(): Unit = Platform.fs.removeAll(root)

  test("skill files round-trip through render and parse"):
    val s = Skill("Greeter", "greets people", "object Greeter {\n  def hi: String = \"hi\"\n}", List("assert(true)"))
    val rendered = SkillStore.renderSkillFile(s)
    SkillStore.parseSkillFile(rendered) match
      case Right((description, code)) =>
        assertEquals(description, s.description)
        assertEquals(code, s.code)
      case Left(err) => fail(err)

  test("a file without the description header is rejected"):
    assert(SkillStore.parseSkillFile("object X {\n}").isLeft)

  test("persist and loadAll round-trip a skill set, removing stale entries"):
    val one = Skill("One", "first", "object One {\n  def n: Int = 1\n}", List("assert(One.n == 1)"))
    val two = Skill("Two", "second", "object Two {\n  def n: Int = 2\n}", List("assert(Two.n == 2)", "assert(true)"))
    store.persist(List(one, two))
    val (loaded, warnings) = store.loadAll()
    assertEquals(warnings, Nil)
    assertEquals(loaded, List(one, two))
    // Persisting a smaller set removes the dropped skill's directory.
    store.persist(List(one))
    val (after, _) = store.loadAll()
    assertEquals(after.map(_.id), List("One"))
    assert(!Platform.fs.exists(s"$root/Two"))

  test("a malformed entry yields a warning, not a failure"):
    Platform.fs.createDirectories(s"$root/Broken")
    Platform.fs.writeString(s"$root/Broken/skill.scala", "object Broken {}")
    val (loaded, warnings) = store.loadAll()
    assertEquals(loaded.map(_.id), List("One"))
    assert(warnings.exists(_.contains("Broken")), warnings.toString)
