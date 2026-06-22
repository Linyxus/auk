package auk.session

import auk.TestFs

class JsonlLogSuite extends munit.FunSuite:

  test("append creates missing parent directories and reads the lines back in order"):
    val path = TestFs.join(TestFs.tempDir("auk-jsonl"), "a/b/c.jsonl")
    assertEquals(JsonlLog.append(path, """{"x":1}"""), Right(()))
    assertEquals(JsonlLog.append(path, """{"x":2}"""), Right(()))
    assertEquals(JsonlLog.readLines(path), Right(List("""{"x":1}""", """{"x":2}""")))

  test("reading a missing file yields no lines"):
    assertEquals(JsonlLog.readLines(TestFs.join(TestFs.tempDir("auk-jsonl"), "nope.jsonl")), Right(Nil))

  test("blank lines are skipped on read"):
    val path = TestFs.join(TestFs.tempDir("auk-jsonl"), "log.jsonl")
    JsonlLog.append(path, "a")
    TestFs.append(path, "\n")
    JsonlLog.append(path, "b")
    assertEquals(JsonlLog.readLines(path), Right(List("a", "b")))
