package fp.raw

@main def run(): Unit =
  println(greet("raw Scala"))

def greet(name: String): String = s"Hello from $name!"
