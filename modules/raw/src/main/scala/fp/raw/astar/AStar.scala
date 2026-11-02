package fp.raw.astar

import fp.common.*
import scala.annotation.tailrec
import fp.common.astar.Status

trait AStar:
  def grid: Grid[Cell]
  def current: Option[Position]
  def closed: Set[Position]
  def status: Status
  def step: AStar
  def cameFrom: Map[Position, Position]

  def isDone: Boolean = status != Status.Running

  def neighbours(pos: Position): Seq[Position] =
    ??? // TODO 03

  def pathTo(pos: Position): List[Position] =
    ??? // TODO 04

  /** Runs the search to completion. */
  @tailrec
  final def run: AStar = if isDone then this else step.run

object AStar:
  /** Starts a search between the `Start` and `Target` cells of `grid`. */
  def apply(
      grid: Grid[Cell],
      builder: (Grid[Cell], Position, Position) => AStar
  ): Either[String, AStar] =
    def find(cell: Cell) =
      grid.dimension.positions
        .find(grid.get(_).contains(cell))
        .toRight(s"No $cell cell in the grid")
    for
      start <- find(Cell.Start)
      target <- find(Cell.Target)
    yield builder(grid, start, target)
