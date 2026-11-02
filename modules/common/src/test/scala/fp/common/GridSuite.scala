package fp.common

class GridSuite extends munit.FunSuite:
  val grid = Grid.tabulate(2, 3)(pos => pos.row * 3 + pos.col)

  test("dimensions"):
    assertEquals((grid.n, grid.p), (2, 3))
    assertEquals(grid.dimension.size, 6)

  test("get inside and outside bounds"):
    assertEquals(grid.get(Position(1, 2)), Some(5))
    assertEquals(grid.get(Position(2, 0)), None)

  test("updated is immutable"):
    val g2 = grid.updated(Position(0, 0), 42)
    assertEquals(g2.get(Position(0, 0)), Some(42))
    assertEquals(grid.get(Position(0, 0)), Some(0))

  test("rows and columns"):
    assertEquals(grid.row(1), Vector(3, 4, 5))
    assertEquals(grid.column(1), Vector(1, 4))

  test("neighbours of a corner"):
    assertEquals(grid.neighbours(Position(0, 0)).map(_._2).toSet, Set(1, 3, 4))

  test("invalid cells are rejected"):
    intercept[IllegalArgumentException](Grid(Dimension(2, 2), Vector(Vector(1, 2))))
