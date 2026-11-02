package fp.common

/** A position in a grid: `row` in [0, n), `col` in [0, p). */
final case class Position(row: Int, col: Int) derives CanEqual:
  def +(other: Position): Position = Position(row + other.row, col + other.col)

/** Dimensions of a grid: `n` rows by `p` columns. */
final case class Dimension(n: Int, p: Int):
  require(n >= 0 && p >= 0, s"Dimensions must be non-negative, got $n x $p")

  def size: Int = n * p

  def contains(pos: Position): Boolean =
    pos.row >= 0 && pos.row < n && pos.col >= 0 && pos.col < p

  def positions: Seq[Position] =
    for
      r <- 0 until n
      c <- 0 until p
    yield Position(r, c)

/** An immutable grid of n * p cells holding values of type `A`. */
final case class Grid[A](dimension: Dimension, cells: Vector[Vector[A]]):
  require(
    cells.size == dimension.n && cells.forall(_.size == dimension.p),
    s"Cells do not match dimension ${dimension.n} x ${dimension.p}"
  )

  def n: Int = dimension.n
  def p: Int = dimension.p

  def get(pos: Position): Option[A] =
    Option.when(dimension.contains(pos))(cells(pos.row)(pos.col))

  def updated(pos: Position, value: A): Grid[A] =
    if dimension.contains(pos) then
      copy(cells = cells.updated(pos.row, cells(pos.row).updated(pos.col, value)))
    else this

  def map[B](f: A => B): Grid[B] = Grid(dimension, cells.map(_.map(f)))

  def row(r: Int): Vector[A] = cells(r)
  def column(c: Int): Vector[A] = cells.map(_(c))

  def neighbours(pos: Position): Seq[(Position, A)] =
    for
      dr <- -1 to 1
      dc <- -1 to 1
      if dr != 0 || dc != 0
      np = pos + Position(dr, dc)
      a <- get(np)
    yield np -> a

object Grid:
  def fill[A](n: Int, p: Int)(value: => A): Grid[A] =
    Grid(Dimension(n, p), Vector.fill(n, p)(value))

  def tabulate[A](n: Int, p: Int)(f: Position => A): Grid[A] =
    ??? // TODO 01
