package fp.zio

import zio.test.*

object MainSpec extends ZIOSpecDefault:
  def spec = suite("Main")(
    test("greet"):
      for msg <- Main.greet("ZIO 2.1.7")
      yield assertTrue(msg == "Hello from ZIO 2.1.7!")
  )
