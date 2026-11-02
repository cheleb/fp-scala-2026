package fp.raw

class RawSuite extends munit.FunSuite:
  test("greet"):
    assertEquals(greet("raw Scala"), "Hello from raw Scala!")
