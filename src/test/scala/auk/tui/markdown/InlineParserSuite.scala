package auk.tui.markdown

class InlineParserSuite extends munit.FunSuite:
  import Inline.*

  private def parse(s: String): Vector[Inline] = InlineParser.parse(s)
  private def t(s: String): Inline = Text(s)

  test("plain text is a single text node"):
    assertEquals(parse("hello world"), Vector(t("hello world")))

  test("emphasis and strong"):
    assertEquals(parse("*a*"), Vector(Emph(Vector(t("a")))))
    assertEquals(parse("_a_"), Vector(Emph(Vector(t("a")))))
    assertEquals(parse("**a**"), Vector(Strong(Vector(t("a")))))
    assertEquals(parse("__a__"), Vector(Strong(Vector(t("a")))))

  test("triple emphasis nests strong inside emph"):
    assertEquals(parse("***a***"), Vector(Emph(Vector(Strong(Vector(t("a")))))))

  test("nested emphasis inside emphasis"):
    assertEquals(parse("*a **b** c*"), Vector(Emph(Vector(t("a "), Strong(Vector(t("b"))), t(" c")))))

  test("intraword underscore does not emphasise; asterisk does"):
    assertEquals(parse("a_b_c"), Vector(t("a_b_c")))
    assertEquals(parse("a*b*c"), Vector(t("a"), Emph(Vector(t("b"))), t("c")))

  test("unmatched delimiters stay literal (streaming half-state)"):
    assertEquals(parse("**bold"), Vector(t("**bold")))
    assertEquals(parse("a * b"), Vector(t("a * b")))

  test("inline code span, verbatim, binds tighter than emphasis"):
    assertEquals(parse("`a*b*c`"), Vector(Code("a*b*c")))
    assertEquals(parse("a `code` b"), Vector(t("a "), Code("code"), t(" b")))

  test("code span with double backticks holds a single backtick"):
    assertEquals(parse("``a`b``"), Vector(Code("a`b")))

  test("code span strips one surrounding space"):
    assertEquals(parse("` a `"), Vector(Code("a")))

  test("unterminated code span is literal"):
    assertEquals(parse("`abc"), Vector(t("`abc")))

  test("strikethrough"):
    assertEquals(parse("~~gone~~"), Vector(Strikethrough(Vector(t("gone")))))

  test("strikethrough wraps nested emphasis"):
    assertEquals(parse("~~a **b**~~"), Vector(Strikethrough(Vector(t("a "), Strong(Vector(t("b")))))))

  test("inline link with and without title"):
    assertEquals(parse("[x](http://e.com)"), Vector(Link(Vector(t("x")), "http://e.com", None)))
    assertEquals(parse("""[x](http://e.com "t")"""), Vector(Link(Vector(t("x")), "http://e.com", Some("t"))))

  test("link text may itself contain emphasis"):
    assertEquals(parse("[*x*](u)"), Vector(Link(Vector(Emph(Vector(t("x")))), "u", None)))

  test("image"):
    assertEquals(parse("![alt](img.png)"), Vector(Image("alt", "img.png", None)))

  test("angle-bracket and email autolinks"):
    assertEquals(parse("<http://e.com>"), Vector(Autolink("http://e.com", isEmail = false)))
    assertEquals(parse("<a@b.com>"), Vector(Autolink("a@b.com", isEmail = true)))

  test("unmatched brackets are literal; reference links unsupported"):
    assertEquals(parse("[x]"), Vector(t("[x]")))
    assertEquals(parse("[x][y]"), Vector(t("[x][y]")))

  test("backslash escapes punctuation"):
    assertEquals(parse("\\*not emph\\*"), Vector(t("*not emph*")))
    assertEquals(parse("a\\\\b"), Vector(t("a\\b")))

  test("backslash before a non-punctuation char stays literal"):
    assertEquals(parse("a\\b"), Vector(t("a\\b")))

  test("two trailing spaces make a hard break; a bare newline is soft"):
    assertEquals(parse("a  \nb"), Vector(t("a"), HardBreak, t("b")))
    assertEquals(parse("a\nb"), Vector(t("a"), SoftBreak, t("b")))

  test("backslash newline is a hard break"):
    assertEquals(parse("a\\\nb"), Vector(t("a"), HardBreak, t("b")))
