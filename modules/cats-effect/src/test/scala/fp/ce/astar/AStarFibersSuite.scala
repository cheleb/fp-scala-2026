package fp.ce.astar

import cats.effect.{IO, Ref}
import fp.common.*
import fp.common.astar.Status
import munit.CatsEffectSuite

class AStarFibersSuite extends CatsEffectSuite:
  def solve(lines: String*): IO[Status] =
    IO.fromEither(AStarFibers(Cell.parse(lines*)).left.map(new IllegalArgumentException(_)))
      .flatMap(_.run)

  /** Fibers are scheduled non-deterministically: run each scenario many times. */
  def repeated(name: String)(body: => IO[Unit]): Unit =
    test(name)(body.replicateA_(20))

  repeated("straight line"):
    solve("S  T").assertEquals(
      Status.Found(List(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3)))
    )

  repeated("goes around a wall"):
    solve(
      "S#  ",
      " # T",
      "    "
    ).map:
      case Status.Found(path) =>
        assertEquals(path.head, Position(0, 0))
        assertEquals(path.last, Position(1, 3))
        assert(path.sliding(2).forall {
          case Seq(a, b) => math.abs(a.row - b.row) + math.abs(a.col - b.col) == 1
          case _         => false
        })
      case other => fail(s"Unexpected $other")

  repeated("no path when the target is walled off"):
    solve("S #T").assertEquals(Status.NoPath)

  test("missing start or target"):
    assert(AStarFibers(Cell.parse("  T")).isLeft)
    assert(AStarFibers(Cell.parse("S  ")).isLeft)

  repeated("every reachable cell is claimed once when there is no path"):
    for
      search <- IO.fromEither(AStarFibers(Cell.parse("S   #T", "    # ")).left.map(new IllegalArgumentException(_)))
      state  <- Ref.of[IO, Snapshot](Snapshot.empty)
      _      <- search.run(state)
      s      <- state.get
    yield
      assertEquals(s.closed.size, 8)
      assertEquals(s.cameFrom.size, 7)

  repeated("demo maze is solvable"):
    IO.fromEither(AStarFibers(AStarFibersApp.maze).left.map(new IllegalArgumentException(_)))
      .flatMap(_.run)
      .map(status => assert(status.isInstanceOf[Status.Found]))
