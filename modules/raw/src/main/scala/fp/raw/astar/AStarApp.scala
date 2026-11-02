package fp.raw.astar

import eu.joaocosta.minart.backend.defaults.given
import eu.joaocosta.minart.graphics.*
import eu.joaocosta.minart.runtime.*
import fp.common.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import fp.common.astar.Status
import fp.common.astar.StarAppDefault

/** Animates an A* search: one step is expanded per frame.
  *
  *   - light yellow: the node being expanded
  *   - light green: explored (closed) nodes
  *   - blue line: the optimal path, once found
  *
  * Run with `sbt "raw/runMain fp.raw.astar.aStarDemo"`.
  */
object AStarApp extends StarAppDefault:
  

  /** Draws one frame of the search on any surface (window or off-screen image).
    */
  def drawFrame(surface: MutableSurface, search: AStar, cellSize: Int): Unit =
    GridRenderer.draw(
      surface,
      search.grid,
      cellSize,
      highlight = pos =>
        if search.current.contains(pos) then Some(CurrentColor)
        else if search.closed(pos) then Some(ExploredColor)
        else None
    )
    search.status match
      case Status.Found(path) =>
        GridRenderer.drawPath(
          surface,
          path,
          cellSize,
          PathColor,
          thickness = cellSize / 5
        )
      case _ => ()

  def render(canvas: Canvas, search: AStar, cellSize: Int): Unit =
    drawFrame(canvas, search, cellSize)
    canvas.redraw()


  def run(
      aStar: Either[String, AStar],
      cellSize: Int = AStarApp.cellSize,
      stepsPerSecond: Int = AStarApp.stepsPerSecond
  ): Unit =
    aStar match
      case Left(error)    => Console.err.println(error)
      case Right(initial) =>
        val loop = AppLoop
          .statefulRenderLoop: (search: AStar) =>
            (canvas: Canvas) =>
              val next = search.step
              render(canvas, next, cellSize)
              next
          .configure(
            GridRenderer
              .settingsFor(initial.grid, cellSize)
              .copy(title = "A* exploration"),
            LoopFrequency.fromHz(stepsPerSecond),
            initial
          )
          .run()
        val result = Await.result(loop, Duration.Inf)
        result.status match
          case Status.Found(path) =>
            println(
              s"Path found: ${path.size - 1} moves, ${result.closed.size} nodes explored"
            )
          case Status.NoPath =>
            println(s"No path, ${result.closed.size} nodes explored")
          case Status.Running =>
            println("Window closed before the search ended")

  

@main def allStarDemo(): Unit =
  val manhattan = AStar(AStarApp.maze, (grid, start, target) => AStarManathan(grid, start, target))
  val naive = AStar(AStarApp.maze, (grid, start, target) => AStarNaive(grid, start, target))

  import scala.concurrent.ExecutionContext.Implicits.global

  val man = Future:
    AStarApp.run(manhattan)

  val na = Future:
    AStarApp.run(naive)

  Await.ready(
    for
      _ <- man
      _ <- na
    yield (),
    Duration.Inf
  ) : Unit
@main def naiveStarDemo(): Unit =
  val naive = AStar(AStarApp.maze, (grid, start, target) => AStarNaive(grid, start, target))

  AStarApp.run(naive)

@main def manhattanStarDemo(): Unit =
  val manhattan = AStar(AStarApp.maze, (grid, start, target) => AStarManathan(grid, start, target))
  val naive = AStar(AStarApp.maze, (grid, start, target) => AStarNaive(grid, start, target))

  import scala.concurrent.ExecutionContext.Implicits.global

  
  AStarApp.run(manhattan)


