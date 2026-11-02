package fp.ce.astar

import cats.effect.{IO, IOApp, Ref, Resource}
import eu.joaocosta.minart.backend.defaults.given
import eu.joaocosta.minart.graphics.*
import fp.common.*
import fp.common.astar.{StarAppDefault, Status}

import scala.concurrent.duration.*

/** Animates the fiber-based search: one fiber runs the search (itself running a
  * fiber per explored cell), another one renders the shared `Ref` periodically.
  *
  *   - light yellow: cells currently being expanded by a fiber
  *   - light green: explored (claimed) cells
  *   - blue line: the path, once found
  *
  * Run with `sbt "catsEffect/runMain fp.ce.astar.AStarFibersApp"`.
  */
object AStarFibersApp extends IOApp.Simple with StarAppDefault:

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

  /** A window, closed when the resource is released. */
  def window(grid: Grid[Cell]): Resource[IO, LowLevelCanvas] =
    Resource.make(
      IO(
        LowLevelCanvas
          .create()
          .init(GridRenderer.settingsFor(grid, cellSize).copy(title = "A* with Cats Effect fibers"))
      )
    )(canvas => IO(canvas.close()))

  /** Redraws `state` until the user closes the window. */
  def renderLoop(canvas: LowLevelCanvas, grid: Grid[Cell], state: Ref[IO, Snapshot]): IO[Unit] =
    (state.get.flatMap(s => IO(drawFrame(canvas, grid, s))) >> IO.sleep(frameRate))
      .iterateWhile(_ => canvas.isCreated())

  def report(status: Status, s: Snapshot): String = status match
    case Status.Found(path) => s"Path found: ${path.size - 1} moves, ${s.closed.size} nodes explored"
    case Status.NoPath      => s"No path, ${s.closed.size} nodes explored"
    case Status.Running     => "Window closed before the search ended"

  val run: IO[Unit] =
    for
      search <- IO.fromEither(AStarFibers(maze, stepDelay).left.map(new IllegalArgumentException(_)))
      state  <- Ref.of[IO, Snapshot](Snapshot.empty)
      _      <- window(maze).use: canvas =>
                  // `background` starts the search in a fiber, cancelled when the
                  // render loop ends (window closed) if it is still running.
                  search
                    .run(state)
                    .flatTap(status => state.get.flatMap(s => IO.println(report(status, s))))
                    .background
                    .surround(renderLoop(canvas, maze, state))
    yield ()
