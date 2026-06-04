package auk.config

class ConfigErrorSuite extends munit.FunSuite:

  test("prefix prepends path segments outermost-last") {
    val e = ConfigError("boom").prefix("lat").prefix("coord").prefix("model")
    assertEquals(e.path, List("model", "coord", "lat"))
  }

  test("at is first-wins so the leaf line survives unwinding") {
    val e = ConfigError("boom").at(7).at(99)
    assertEquals(e.line, Some(7))
  }

  test("at does not overwrite an explicitly-set line") {
    val e = ConfigError("boom", line = Some(3)).at(50)
    assertEquals(e.line, Some(3))
  }

  test("render formats line + dotted path + message") {
    val e = ConfigError("expected number", path = List("model", "coord", "lat"), line = Some(7))
    assertEquals(e.render, "line 7: model.coord.lat: expected number")
  }

  test("render without a line omits the line prefix") {
    val e = ConfigError("missing required value", path = List("model", "id"))
    assertEquals(e.render, "model.id: missing required value")
  }

  test("render with only a message") {
    assertEquals(ConfigError("bad").render, "bad")
  }

  test("render with array segment uses [i] without a leading dot") {
    val e = ConfigError("expected integer", path = List("ports", "[0]"))
    assertEquals(e.render, "ports[0]: expected integer")
  }
