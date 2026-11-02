package fp.zio.astar

import fp.common.*
import zio.*
import fp.common.astar.Status


/** What the search looks like at a given instant, shared between fibers.
  *
  * @param closed   positions already claimed by a fiber (the "explored" set)
  * @param cameFrom predecessor of each claimed position
  * @param current  positions whose fiber is currently expanding them
  */
final case class Snapshot(
    closed: Set[Position],
    cameFrom: Map[Position, Position],
    current: Set[Position],
    status: Status
):
  def pathTo(pos: Position): List[Position] =
    @annotation.tailrec
    def loop(p: Position, acc: List[Position]): List[Position] =
      cameFrom.get(p) match
        case Some(prev) => loop(prev, p :: acc)
        case None       => p :: acc
    loop(pos, Nil)

object Snapshot:
  val empty: Snapshot = Snapshot(Set.empty, Map.empty, Set.empty, Status.Running)

/** The naive search (see `fp.raw.astar.AStarNaive`), but where every frontier
  * node is explored by its own fiber.
  *
  * Exploring a position:
  *   1. atomically claims it in the shared `Ref` (a position is claimed once,
  *      by the first fiber that reaches it, which records where it came from);
  *   2. if it is the target, completes the `found` promise;
  *   3. otherwise forks one child fiber per free neighbour and joins them.
  *
  * The whole exploration is raced against `found.await`: as soon as the target
  * is reached, the race interrupts the exploration fiber, and structured
  * concurrency interrupts all of its descendants. If the exploration finishes
  * and the promise is still empty, every reachable cell was claimed without
  * reaching the target.
  *
  * With the same `stepDelay` everywhere, fibers advance as a BFS wavefront; the
  * path is the one of the fiber that won each cell, so (as for the naive
  * version) it is not guaranteed to be optimal.
  */
final class AStarFibers(
    val grid: Grid[Cell],
    val start: Position,
    val target: Position,
    val stepDelay: Duration
):

  def neighbours(pos: Position): Seq[Position] =
    Seq(Position(-1, 0), Position(1, 0), Position(0, -1), Position(0, 1))
      .map(pos + _)
      .filter(n => grid.get(n).exists(_ != Cell.Wall))

  /** Runs the search, publishing its progress in `state`. */
  def run(state: Ref[Snapshot]): UIO[Status] =
    for
      found  <- Promise.make[Nothing, Position]
      // Stops at the first of: target found, or every reachable cell explored.
      // Both sides may finish together, so the outcome is read from the promise.
      _      <- explore(start, None, state, found).race(found.await.unit)
      target <- found.poll.flatMap(ZIO.foreach(_)(identity))
      status <- state.modify: s =>
                  val status = target.fold(Status.NoPath)(pos => Status.Found(s.pathTo(pos)))
                  (status, s.copy(current = Set.empty, status = status))
    yield status

  /** Same as `run`, starting from an empty snapshot. */
  def run: UIO[Status] = Ref.make(Snapshot.empty).flatMap(run)

  private def explore(
      pos: Position,
      from: Option[Position],
      state: Ref[Snapshot],
      found: Promise[Nothing, Position]
  ): UIO[Unit] =
    state
      .modify: s =>
        if s.closed(pos) then (false, s)
        else
          (
            true,
            s.copy(
              closed = s.closed + pos,
              cameFrom = from.fold(s.cameFrom)(f => s.cameFrom + (pos -> f)),
              current = s.current + pos
            )
          )
      .flatMap: claimed =>
        if !claimed then ZIO.unit
        else if pos == target then found.succeed(pos).unit
        else
          for
            _        <- ZIO.sleep(stepDelay).unless(stepDelay.isZero)
            _        <- state.update(s => s.copy(current = s.current - pos))
            children <- ZIO.foreachPar(neighbours(pos))(n => explore(n, Some(pos), state, found))
          yield ()

object AStarFibers:
  /** Prepares a search between the `Start` and `Target` cells of `grid`. */
  def apply(grid: Grid[Cell], stepDelay: Duration = Duration.Zero): Either[String, AStarFibers] =
    def find(cell: Cell) =
      grid.dimension.positions
        .find(grid.get(_).contains(cell))
        .toRight(s"No $cell cell in the grid")
    for
      start  <- find(Cell.Start)
      target <- find(Cell.Target)
    yield new AStarFibers(grid, start, target, stepDelay)
