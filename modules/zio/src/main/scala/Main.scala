package fp.zio

import zio.*

object Main extends ZIOAppDefault:
  def greet(name: String): UIO[String] = ZIO.succeed(s"Hello from $name!")

  val run = greet("ZIO").flatMap(Console.printLine(_))
