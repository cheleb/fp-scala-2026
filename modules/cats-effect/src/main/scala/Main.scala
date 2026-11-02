package fp.ce

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple:
  def greet(name: String): IO[String] = IO.pure(s"Hello from $name!")

  val run: IO[Unit] = greet("Cats Effect").flatMap(IO.println)
