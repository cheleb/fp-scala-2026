package fp.zio.astar

import eu.joaocosta.minart.backend.defaults.given
import eu.joaocosta.minart.graphics.*
import fp.common.*
import zio.*
import fp.common.astar.Status
import fp.common.astar.StarAppDefault

/** Animates the fiber-based search: one fiber runs the search (itself forking a
  * fiber per explored cell), another one renders the shared `Ref` periodically.
  *
  *   - light yellow: cells currently being expanded by a fiber
  *   - light green: explored (claimed) cells
  *   - blue line: the path, once found
  *
  * Run with `sbt "zio/runMain fp.zio.astar.AStarFibersApp"`.
  */
object AStarFibersApp extends ZIOAppDefault with StarAppDefault:

  val frameRate = 30.millis
  val stepDelay = sleep.millis


  def drawFrame(canvas: Canvas, grid: Grid[Cell], s: Snapshot): Unit =
    GridRenderer.draw(
      canvas,
      grid,
      cellSize,
      highlight = pos =>
        if s.current(pos) then Some(CurrentColor)
        else if s.closed(pos) then Some(ExploredColor)
        else None
    )
    s.status match
      case Status.Found(path) =>
        GridRenderer.drawPath(canvas, path, cellSize, PathColor, thickness = cellSize / 5)
      case _ => ()
    canvas.redraw()

  /** A window, closed when the scope ends. */
  def window(grid: Grid[Cell]): ZIO[Scope, Throwable, LowLevelCanvas] =
    ZIO.acquireRelease(
      ZIO.attempt(
        LowLevelCanvas
          .create()
          .init(GridRenderer.settingsFor(grid, cellSize).copy(title = "A* with ZIO fibers"))
      )
    )(canvas => ZIO.succeed(canvas.close()))

  /** Redraws `state` until the user closes the window. */
  def renderLoop(canvas: LowLevelCanvas, grid: Grid[Cell], state: Ref[Snapshot]): UIO[Unit] =
    (state.get.flatMap(s => ZIO.succeed(drawFrame(canvas, grid, s))) *> ZIO.sleep(frameRate))
      .repeatWhile(_ => canvas.isCreated())

  def report(status: Status, s: Snapshot): String = status match
    case Status.Found(path) => s"Path found: ${path.size - 1} moves, ${s.closed.size} nodes explored"
    case Status.NoPath      => s"No path, ${s.closed.size} nodes explored"
    case Status.Running     => "Window closed before the search ended"

  val run =
    for
      search <- ZIO.fromEither(AStarFibers(maze, stepDelay)).mapError(new IllegalArgumentException(_))
      state  <- Ref.make(Snapshot.empty)
      _      <- ZIO.scoped:
                  for
                    canvas <- window(maze)
                    fiber  <- search
                                .run(state)
                                .tap(status => state.get.flatMap(s => Console.printLine(report(status, s)).orDie))
                                .fork
                    _      <- renderLoop(canvas, maze, state)
                    _      <- fiber.interrupt // window closed: stop the search if still running
                  yield ()
    yield ()
