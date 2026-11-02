package fp.zio.astar

import fp.common.*
import zio.*
import zio.test.*
import fp.common.astar.Status

object AStarFibersSpec extends ZIOSpecDefault:
  def solve(lines: String*): IO[String, Status] =
    ZIO.fromEither(AStarFibers(Cell.parse(lines*))).flatMap(_.run)

  def spec = suite("AStarFibers")(
    test("straight line"):
      for status <- solve("S  T")
      yield assertTrue(
        status == Status.Found(List(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3)))
      )
    ,
    test("goes around a wall"):
      for status <- solve(
                      "S#  ",
                      " # T",
                      "    "
                    )
      yield status match
        case Status.Found(path) =>
          assertTrue(
            path.head == Position(0, 0),
            path.last == Position(1, 3),
            path.sliding(2).forall {
               case Seq(a, b) => math.abs(a.row - b.row) + math.abs(a.col - b.col) == 1 
               case _ => false
               }
          )
        case other => assertNever(s"Unexpected $other")
    ,
    test("no path when the target is walled off"):
      for status <- solve("S #T")
      yield assertTrue(status == Status.NoPath)
    ,
    test("missing start or target"):
      for
        noStart  <- solve("  T").either
        noTarget <- solve("S  ").either
      yield assertTrue(noStart.isLeft, noTarget.isLeft)
    ,
    test("every reachable cell is claimed once when there is no path"):
      for
        search <- ZIO.fromEither(AStarFibers(Cell.parse("S   #T", "    # ")))
        state  <- Ref.make(Snapshot.empty)
        _      <- search.run(state)
        s      <- state.get
      yield assertTrue(s.closed.size == 8, s.cameFrom.size == 7)
    ,
    test("demo maze is solvable"):
      for status <- ZIO.fromEither(AStarFibers(AStarFibersApp.maze)).flatMap(_.run)
      yield assertTrue(status.isInstanceOf[Status.Found])
  ) @@ TestAspect.repeats(20) // fibers are scheduled non-deterministically
