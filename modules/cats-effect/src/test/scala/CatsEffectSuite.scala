package fp.ce

import munit.CatsEffectSuite

class MainSuite extends CatsEffectSuite:
  test("greet"):
    Main.greet("Cats Effect").assertEquals("Hello from Cats Effect!")
