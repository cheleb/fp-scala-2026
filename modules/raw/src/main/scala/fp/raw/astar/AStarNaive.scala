package fp.raw.astar

import fp.common.*

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import fp.common.astar.Status



/** An immutable snapshot of an A* search on a grid, advanced one step at a time.
  *
  * Moves are 4-directional with unit cost; the heuristic is the Manhattan
  * distance, which is consistent, so a closed node is never reopened.
  *
  * @param open     frontier ordered by f score (may hold stale duplicates)
  * @param g        best known cost from the start to each discovered position
  * @param cameFrom predecessor of each discovered position on its best path
  * @param closed   positions already expanded (the "explored" set)
  * @param current  position expanded by the last step
  */
final case class AStarNaive(
    grid: Grid[Cell],
    start: Position,
    target: Position,
    /** Position to visit in that order */
    open: Seq[Position],
    /** Map Current -> Previous */
    cameFrom: Map[Position, Position],
    /** Already visited position */
    closed: Set[Position],
    current: Option[Position],
    status: Status
) extends AStar:

  /** Expands the best node of the frontier.
   * 
   *  3 cases:
   *  - open is empty (or already visited) -> Status.NoPath
   *  - target is reached -> Status.Found
   *  - need to recurse...
   */
  def step: AStar =
    ??? // TODO 05

object AStarNaive:

  def apply(grid: Grid[Cell], start: Position, target: Position): AStarNaive =
    AStarNaive(
      grid = grid,
      start = start,
      target = target,
      open = Seq(start),
      cameFrom = Map.empty,
      closed = Set.empty,
      current = None,
      status = Status.Running
    )
