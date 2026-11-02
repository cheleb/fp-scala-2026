package fp.kyo.astar

import eu.joaocosta.minart.backend.defaults.given
import eu.joaocosta.minart.graphics.*
import fp.common.*
import fp.common.astar.{StarAppDefault, Status}
import kyo.*

/** Animates the fiber-based search: one fiber runs the search (itself running a
  * fiber per explored cell), another one renders the shared `AtomicRef`
  * periodically.
  *
  *   - light yellow: cells currently being expanded by a fiber
  *   - light green: explored (claimed) cells
  *   - blue line: the path, once found
  *
  * Run with `sbt "kyo/runMain fp.kyo.astar.AStarFibersApp"`.
  */
object AStarFibersApp extends KyoApp with StarAppDefault:

  val frameRate: Duration = 30.millis
  val stepDelay: Duration = sleep.millis

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

  /** A window, closed when the enclosing `Scope` ends. */
  def window(grid: Grid[Cell]): LowLevelCanvas < (Scope & Sync) =
    Scope.acquireRelease(
      Sync.defer(
        LowLevelCanvas
          .create()
          .init(GridRenderer.settingsFor(grid, cellSize).copy(title = "A* with Kyo fibers"))
      )
    )(canvas => Sync.defer(canvas.close()))

  /** Redraws `state` until the user closes the window. */
  def renderLoop(canvas: LowLevelCanvas, grid: Grid[Cell], state: AtomicRef[Snapshot]): Unit < Async =
    for
      s    <- state.get
      _    <- Sync.defer(drawFrame(canvas, grid, s))
      _    <- Async.sleep(frameRate)
      open <- Sync.defer(canvas.isCreated())
      _    <- if open then renderLoop(canvas, grid, state) else Kyo.unit
    yield ()

  def report(status: Status, s: Snapshot): String = status match
    case Status.Found(path) => s"Path found: ${path.size - 1} moves, ${s.closed.size} nodes explored"
    case Status.NoPath      => s"No path, ${s.closed.size} nodes explored"
    case Status.Running     => "Window closed before the search ended"

  run:
    for
      search <- Abort.get(AStarFibers(maze, stepDelay))
      state  <- AtomicRef.init(Snapshot.empty)
      canvas <- window(maze)
      // `Fiber.init` ties the search fiber to the app's `Scope`: it is
      // interrupted when the scope ends (window closed) if still running.
      _      <- Fiber.init:
                  for
                    status <- search.run(state)
                    s      <- state.get
                    _      <- Console.printLine(report(status, s))
                  yield ()
      _      <- renderLoop(canvas, maze, state)
    yield ()
