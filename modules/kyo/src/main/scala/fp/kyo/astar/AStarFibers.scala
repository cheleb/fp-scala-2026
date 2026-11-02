package fp.kyo.astar

import fp.common.*
import fp.common.astar.Status
import kyo.*

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
  * node is explored by its own Kyo fiber.
  *
  * Exploring a position:
  *   1. atomically claims it in the shared `AtomicRef` (a position is claimed
  *      once, by the first fiber that reaches it, which records where it came
  *      from);
  *   2. if it is the target, completes the `found` promise with it;
  *   3. otherwise explores every free neighbour in its own fiber
  *      (`Async.foreachDiscard`) and waits for all of them.
  *
  * The whole exploration is raced against `found.get` (`Async.race`): as soon
  * as the target is reached, the losing exploration fiber is interrupted, and
  * so are the fibers started by `Async.foreachDiscard`. If the exploration
  * finishes first, every reachable cell was claimed: it completes the promise
  * with `None` (a no-op if the target won meanwhile) and returns its value.
  *
  * Both sides of the race read the promise, and the promise is always complete
  * before the race ends: in Kyo, interrupting a fiber waiting on a promise can
  * interrupt the promise itself, so it must never be left pending.
  *
  * Kyo's `AtomicRef` has no `modify`, so a claim is a `getAndUpdate` followed
  * by a look at the previous value: the update is atomic, and only the fiber
  * that saw the position unclaimed goes on with it.
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

  def neighbours(pos: Position): List[Position] =
    List(Position(-1, 0), Position(1, 0), Position(0, -1), Position(0, 1))
      .map(pos + _)
      .filter(n => grid.get(n).exists(_ != Cell.Wall))

  /** Runs the search, publishing its progress in `state`. */
  def run(state: AtomicRef[Snapshot])(using Frame): Status < Async =
    for
      found  <- Fiber.Promise.init[Option[Position], Any]
      // Stops at the first of: target found, or every reachable cell explored.
      exhausted = explore(start, None, state, found)
                    .andThen(found.completeDiscard(Result.succeed(None)))
                    .andThen(found.get)
      target <- Async.race(exhausted, found.get)
      after  <- state.updateAndGet: s =>
                  val status = target.fold(Status.NoPath)(pos => Status.Found(s.pathTo(pos)))
                  s.copy(current = Set.empty, status = status)
    yield after.status

  /** Same as `run`, starting from an empty snapshot. */
  def run(using Frame): Status < Async = AtomicRef.init(Snapshot.empty).map(run(_))

  private def explore(
      pos: Position,
      from: Option[Position],
      state: AtomicRef[Snapshot],
      found: Fiber.Promise[Option[Position], Any]
  )(using Frame): Unit < Async =
    state
      .getAndUpdate: s =>
        if s.closed(pos) then s
        else
          s.copy(
            closed = s.closed + pos,
            cameFrom = from.fold(s.cameFrom)(f => s.cameFrom + (pos -> f)),
            current = s.current + pos
          )
      .map: previous =>
        if previous.closed(pos) then ()
        else if pos == target then found.completeDiscard(Result.succeed(Some(pos)))
        else
          for
            _ <- if stepDelay == Duration.Zero then Kyo.unit else Async.sleep(stepDelay)
            _ <- state.updateAndGet(s => s.copy(current = s.current - pos))
            toVisits = neighbours(pos)
            _ <- Async.foreachDiscard(toVisits, concurrency = toVisits.size )(n => explore(n, Some(pos), state, found))
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
