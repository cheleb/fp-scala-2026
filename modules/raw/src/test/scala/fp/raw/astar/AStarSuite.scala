package fp.raw.astar

import fp.common.*
import fp.common.astar.Status

class AStarSuite extends munit.FunSuite:
  def solve(lines: String*) = AStarManathan(Cell.parse(lines*)).map(_.run.status)

  test("straight line"):
    assertEquals(
      solve("S  T"),
      Right(Status.Found(List(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3))))
    )

  test("goes around a wall with an optimal path"):
    solve(
      "S#  ",
      " # T",
      "    "
    ) match
      case Right(Status.Found(path)) =>
        assertEquals(path.head, Position(0, 0))
        assertEquals(path.last, Position(1, 3))
        assertEquals(path.size - 1, 6)
      case other => fail(s"Unexpected $other")

  test("no path when the target is walled off"):
    assertEquals(solve("S #T"), Right(Status.NoPath))

  test("missing start or target"):
    assert(solve("  T").isLeft)
    assert(solve("S  ").isLeft)

  test("each step expands one node, which becomes explored"):
    val s0 = AStarManathan(Cell.parse("S  T")).toOption.get
    val s1 = s0.step
    assertEquals(s1.current, Some(Position(0, 0)))
    assertEquals(s1.closed, Set(Position(0, 0)))

  test("demo maze is solvable"):
    assert(AStarManathan(AStarApp.maze).toOption.get.run.status.isInstanceOf[Status.Found])
