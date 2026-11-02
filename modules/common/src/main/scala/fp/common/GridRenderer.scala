package fp.common

import eu.joaocosta.minart.backend.defaults.given
import eu.joaocosta.minart.graphics.*
import eu.joaocosta.minart.runtime.*

/** Renders a `Grid[Cell]` with minart. */
object GridRenderer:

  /** Default colour of each kind of cell. */
  def defaultPalette(cell: Cell): Color = cell match
    case Cell.Wall   => Color(60, 60, 60)
    case Cell.Empty  => Color(230, 230, 230)
    case Cell.Start  => Color(40, 160, 70)
    case Cell.Target => Color(200, 40, 40)

  /** Draws `grid` on `surface`, each cell being a `cellSize` x `cellSize` square.
    *
    * Rows go down the y axis and columns along the x axis. A `gap` of
    * background-coloured pixels can be left between cells. `Start` and
    * `Target` are drawn as a circle and a cross over an `Empty` cell, using
    * their palette colour for the symbol.
    *
    * `highlight` can override the background colour of any non-wall cell,
    * e.g. to show the state of a search algorithm.
    */
  def draw(
      surface: MutableSurface,
      grid: Grid[Cell],
      cellSize: Int,
      gap: Int = 1,
      background: Color = Color(0, 0, 0),
      palette: Cell => Color = defaultPalette,
      highlight: Position => Option[Color] = _ => None
  ): Unit =
    surface.fill(background)
    for pos <- grid.dimension.positions do
      grid.get(pos).foreach: cell =>
        drawCell(
          surface,
          cell,
          pos.col * cellSize,
          pos.row * cellSize,
          cellSize - gap,
          palette,
          highlight(pos)
        )

  /** Draws one cell as a `side` x `side` square whose top-left corner is (x, y).
    *
    * `background` replaces the Empty colour behind non-wall cells.
    */
  def drawCell(
      surface: MutableSurface,
      cell: Cell,
      x: Int,
      y: Int,
      side: Int,
      palette: Cell => Color = defaultPalette,
      background: Option[Color] = None
  ): Unit =
    val bg = background.getOrElse(palette(Cell.Empty))
    cell match
      case Cell.Wall =>
        surface.fillRegion(x, y, side, side, palette(Cell.Wall))
      case Cell.Empty =>
        surface.fillRegion(x, y, side, side, bg)
      case Cell.Start =>
        surface.fillRegion(x, y, side, side, bg)
        drawCircle(surface, x, y, side, palette(Cell.Start))
      case Cell.Target =>
        surface.fillRegion(x, y, side, side, bg)
        drawCross(surface, x, y, side, palette(Cell.Target))

  /** Draws a line of the given `thickness` through the centres of the cells of `path`. */
  def drawPath(
      surface: MutableSurface,
      path: Seq[Position],
      cellSize: Int,
      color: Color,
      gap: Int = 1,
      thickness: Int = 3
  ): Unit =
    def centre(pos: Position) =
      val half = (cellSize - gap) / 2
      (pos.col * cellSize + half, pos.row * cellSize + half)
    val r = thickness / 2
    def dot(x: Int, y: Int) =
      surface.fillRegion(x - r, y - r, thickness, thickness, color)
    path.map(centre) match
      case Seq(single) => dot(single._1, single._2)
      case points =>
        points.sliding(2).foreach:
          case Seq((x0, y0), (x1, y1)) =>
            val steps = math.max(math.abs(x1 - x0), math.abs(y1 - y0)).max(1)
            for i <- 0 to steps do
              dot(x0 + (x1 - x0) * i / steps, y0 + (y1 - y0) * i / steps)
          case _ => ()

  /** A filled disc centred in the square, about 70% of its width. */
  private def drawCircle(surface: MutableSurface, x: Int, y: Int, side: Int, color: Color): Unit =
    val c = side / 2.0
    val r = side * 0.35
    for
      i <- 0 until side
      j <- 0 until side
      dx = i + 0.5 - c
      dy = j + 0.5 - c
      if dx * dx + dy * dy <= r * r
    do surface.putPixel(x + i, y + j, color)

  /** An X spanning the square, leaving a margin on each side. */
  private def drawCross(surface: MutableSurface, x: Int, y: Int, side: Int, color: Color): Unit =
    val margin = side / 5
    val thickness = math.max(1, side / 8)
    for
      i <- margin until side - margin
      j <- margin until side - margin
      if math.abs(i - j) < thickness || math.abs(i + j - (side - 1)) < thickness
    do surface.putPixel(x + i, y + j, color)

  /** Canvas settings sized to fit the whole grid. */
  def settingsFor(grid: Grid[Cell], cellSize: Int, scale: Int = 1): Canvas.Settings =
    Canvas.Settings(
      width = grid.p * cellSize,
      height = grid.n * cellSize,
      scale = Some(scale),
      title = s"Grid ${grid.n} x ${grid.p}"
    )

  /** Opens a window showing `grid`. Blocks until the window is closed. */
  def show(grid: Grid[Cell], cellSize: Int = 16, scale: Int = 1): Unit =
    AppLoop
      .statelessRenderLoop: (canvas: Canvas) =>
        draw(canvas, grid, cellSize)
        canvas.redraw()
      .configure(settingsFor(grid, cellSize, scale), LoopFrequency.Never)
      .run() : Unit

/** Demo: `sbt common/runMain fp.common.showGridDemo` */
@main def showGridDemo(): Unit =
  GridRenderer.show(
    Cell.parse(
      "##########",
      "#S       #",
      "#  ##    #",
      "#   #  # #",
      "#      #T#",
      "##########"
    ),
    cellSize = 32
  )
