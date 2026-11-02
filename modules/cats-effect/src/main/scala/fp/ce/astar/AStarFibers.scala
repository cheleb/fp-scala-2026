package fp.ce.astar

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fp.common.*
import fp.common.astar.Status

import scala.concurrent.duration.*

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
  * node is explored by its own Cats Effect fiber.
  *
  * Exploring a position:
  *   1. atomically claims it in the shared `Ref` (a position is claimed once,
  *      by the first fiber that reaches it, which records where it came from);
  *   2. if it is the target, completes the `found` deferred;
  *   3. otherwise explores every free neighbour in its own fiber
  *      (`parTraverse_`) and waits for all of them.
  *
  * The whole exploration is raced against `found.get`: as soon as the target is
  * reached, the race cancels the exploration fiber. `parTraverse_` propagates
  * the cancellation to the child fibers it started. (A fiber started with
  * `.start` would NOT be cancelled with its parent: unlike ZIO, Cats Effect
  * does not supervise forked fibers.) If the exploration finishes and the
  * deferred is still empty, every reachable cell was claimed without reaching
  * the target.
  *
  * With the same `stepDelay` everywhere, fibers advance as a BFS wavefront; the
  * path is the one of the fiber that won each cell, so (as for the naive
  * version) it is not guaranteed to be optimal.
  */
final class AStarFibers(
    val grid: Grid[Cell],
    val start: Position,
    val target: Position,
    val stepDelay: FiniteDuration
):

  def neighbours(pos: Position): List[Position] =
    List(Position(-1, 0), Position(1, 0), Position(0, -1), Position(0, 1))
      .map(pos + _)
      .filter(n => grid.get(n).exists(_ != Cell.Wall))

  /** Runs the search, publishing its progress in `state`. */
  def run(state: Ref[IO, Snapshot]): IO[Status] =
    for
      found  <- Deferred[IO, Position]
      // Stops at the first of: target found, or every reachable cell explored.
      // Both sides may finish together, so the outcome is read from the deferred.
      _      <- IO.race(explore(start, None, state, found), found.get)
      target <- found.tryGet
      status <- state.modify: s =>
                  val status = target.fold(Status.NoPath)(pos => Status.Found(s.pathTo(pos)))
                  (s.copy(current = Set.empty, status = status), status)
    yield status

  /** Same as `run`, starting from an empty snapshot. */
  def run: IO[Status] = Ref.of[IO, Snapshot](Snapshot.empty).flatMap(run)

  private def explore(
      pos: Position,
      from: Option[Position],
      state: Ref[IO, Snapshot],
      found: Deferred[IO, Position]
  ): IO[Unit] =
    state
      .modify: s =>
        if s.closed(pos) then (s, false)
        else
          (
            s.copy(
              closed = s.closed + pos,
              cameFrom = from.fold(s.cameFrom)(f => s.cameFrom + (pos -> f)),
              current = s.current + pos
            ),
            true
          )
      .flatMap: claimed =>
        if !claimed then IO.unit
        else if pos == target then found.complete(pos).void
        else
          for
            _ <- IO.sleep(stepDelay).whenA(stepDelay > Duration.Zero)
            _ <- state.update(s => s.copy(current = s.current - pos))
            _ <- neighbours(pos).parTraverse_(n => explore(n, Some(pos), state, found))
          yield ()

object AStarFibers:
  /** Prepares a search between the `Start` and `Target` cells of `grid`. */
  def apply(grid: Grid[Cell], stepDelay: FiniteDuration = Duration.Zero): Either[String, AStarFibers] =
    def find(cell: Cell) =
      grid.dimension.positions
        .find(grid.get(_).contains(cell))
        .toRight(s"No $cell cell in the grid")
    for
      start  <- find(Cell.Start)
      target <- find(Cell.Target)
    yield new AStarFibers(grid, start, target, stepDelay)
