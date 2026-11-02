package fp.kyo

import kyo.*

object Main extends KyoApp:
  def greet(name: String): String < Any = s"Hello from $name!"

  run {
    greet("Kyo").map(Console.printLine(_))
  }
