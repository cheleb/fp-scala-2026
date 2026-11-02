package fp.raw.astar

import fp.common.*

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import fp.common.astar.Status

/** An entry of the open set: a position with its f = g + h score. */
final case class Node(f: Int, h: Int, pos: Position)

object Node:
  /** Lowest f first; ties broken on h (closest to the target), then position. */
  given Ordering[Node] = Ordering.by(n => (n.f, n.h, n.pos.row, n.pos.col))


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

final case class AStarManathan(
    grid: Grid[Cell],
    start: Position,
    target: Position,
    open: TreeSet[Node],
    g: Map[Position, Int],
    cameFrom: Map[Position, Position],
    closed: Set[Position],
    current: Option[Position],
    status: Status
) extends AStar:

  def heuristic(pos: Position): Int =
    math.abs(pos.row - target.row) + math.abs(pos.col - target.col)


  /** Expands the best node of the frontier. */
  def step: AStar =
    ??? // TODO 06


object AStarManathan:
  /** Starts a search between the `Start` and `Target` cells of `grid`. */
  def apply(grid: Grid[Cell]): Either[String, AStar] =
    AStar(grid, (grid, start, target) => AStarManathan(grid, start, target))

  def apply(grid: Grid[Cell], start: Position, target: Position): AStarManathan =
    val h = math.abs(start.row - target.row) + math.abs(start.col - target.col)
    AStarManathan(
      grid = grid,
      start = start,
      target = target,
      open = TreeSet(Node(h, h, start)),
      g = Map(start -> 0),
      cameFrom = Map.empty,
      closed = Set.empty,
      current = None,
      status = Status.Running
    )
