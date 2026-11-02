package fp.kyo

import kyo.*

class KyoSuite extends munit.FunSuite:
  test("greet"):
    assertEquals(Main.greet("Kyo").eval, "Hello from Kyo!")
