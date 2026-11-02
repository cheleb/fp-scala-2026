package fp.common

import eu.joaocosta.minart.graphics.*

class GridRendererSuite extends munit.FunSuite:
  val grid = Cell.parse("# ", " #")

  test("parse"):
    assertEquals(grid.get(Position(0, 0)), Some(Cell.Wall))
    assertEquals(grid.get(Position(0, 1)), Some(Cell.Empty))
    assertEquals(grid.get(Position(1, 1)), Some(Cell.Wall))

  test("draw paints each cell with its palette colour"):
    val surface = new RamSurface(8, 8, Color(1, 2, 3))
    GridRenderer.draw(surface, grid, cellSize = 4, gap = 0)
    def at(x: Int, y: Int) = surface.getPixel(x, y)
    assertEquals(at(0, 0), Some(GridRenderer.defaultPalette(Cell.Wall)))
    assertEquals(at(5, 1), Some(GridRenderer.defaultPalette(Cell.Empty)))
    assertEquals(at(1, 5), Some(GridRenderer.defaultPalette(Cell.Empty)))
    assertEquals(at(7, 7), Some(GridRenderer.defaultPalette(Cell.Wall)))

  test("gap leaves background between cells"):
    val surface = new RamSurface(8, 8, Color(1, 2, 3))
    GridRenderer.draw(surface, grid, cellSize = 4, gap = 1, background = Color(9, 9, 9))
    assertEquals(surface.getPixel(3, 0), Some(Color(9, 9, 9)))

  test("parse start and target"):
    val g = Cell.parse("S.T")
    assertEquals(g.row(0), Vector(Cell.Start, Cell.Empty, Cell.Target))

  test("start is a circle on an empty background"):
    val surface = new RamSurface(16, 16, Color(1, 2, 3))
    GridRenderer.draw(surface, Cell.parse("S"), cellSize = 16, gap = 0)
    assertEquals(surface.getPixel(8, 8), Some(GridRenderer.defaultPalette(Cell.Start)))
    assertEquals(surface.getPixel(0, 0), Some(GridRenderer.defaultPalette(Cell.Empty)))

  test("target is a cross on an empty background"):
    val surface = new RamSurface(16, 16, Color(1, 2, 3))
    GridRenderer.draw(surface, Cell.parse("T"), cellSize = 16, gap = 0)
    val red = Some(GridRenderer.defaultPalette(Cell.Target))
    val empty = Some(GridRenderer.defaultPalette(Cell.Empty))
    assertEquals(surface.getPixel(4, 4), red)    // on the diagonal
    assertEquals(surface.getPixel(11, 4), red)   // on the anti-diagonal
    assertEquals(surface.getPixel(8, 3), empty)  // between the arms
    assertEquals(surface.getPixel(0, 0), empty)  // margin
