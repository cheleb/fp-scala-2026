package fp.kyo.astar

import fp.common.*
import fp.common.astar.Status
import kyo.*

class AStarFibersSuite extends munit.FunSuite:
  import AllowUnsafe.embrace.danger

  /** Runs a Kyo computation to completion on the current thread. */
  def runSync[A](v: A < (Async & Scope & Abort[Any])): A =
    KyoApp.Unsafe.runAndBlock(10.seconds)(v).getOrThrow

  def solve(lines: String*): Status =
    runSync(Abort.get(AStarFibers(Cell.parse(lines*))).map(_.run))

  /** Fibers are scheduled non-deterministically: run each scenario many times. */
  def repeated(name: String)(body: => Unit): Unit =
    test(name)((1 to 20).foreach(_ => body))

  repeated("straight line"):
    assertEquals(
      solve("S  T"),
      Status.Found(List(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3)))
    )

  repeated("goes around a wall"):
    solve(
      "S#  ",
      " # T",
      "    "
    ) match
      case Status.Found(path) =>
        assertEquals(path.head, Position(0, 0))
        assertEquals(path.last, Position(1, 3))
        assert(path.sliding(2).forall {
          case Seq(a, b) => math.abs(a.row - b.row) + math.abs(a.col - b.col) == 1
          case _         => false
        })
      case other => fail(s"Unexpected $other")

  repeated("no path when the target is walled off"):
    assertEquals(solve("S #T"), Status.NoPath)

  test("missing start or target"):
    assert(AStarFibers(Cell.parse("  T")).isLeft)
    assert(AStarFibers(Cell.parse("S  ")).isLeft)

  repeated("every reachable cell is claimed once when there is no path"):
    val s = runSync:
      for
        search <- Abort.get(AStarFibers(Cell.parse("S   #T", "    # ")))
        state  <- AtomicRef.init(Snapshot.empty)
        _      <- search.run(state)
        s      <- state.get
      yield s
    assertEquals(s.closed.size, 8)
    assertEquals(s.cameFrom.size, 7)

  repeated("demo maze is solvable"):
    assert(runSync(Abort.get(AStarFibers(AStarFibersApp.maze)).map(_.run)).isInstanceOf[Status.Found])

  test("remaining fibers are interrupted once the target is found"):
    val (atEnd, later) = runSync:
      for
        search <- Abort.get(AStarFibers(AStarFibersApp.maze.map(c => if c == Cell.Target then Cell.Empty else c)
                                          .updated(Position(1, 3), Cell.Target), 5.millis))
        state  <- AtomicRef.init(Snapshot.empty)
        _      <- search.run(state)
        atEnd  <- state.get
        _      <- Async.sleep(100.millis)
        later  <- state.get
      yield (atEnd.closed.size, later.closed.size)
    assertEquals(later, atEnd)
