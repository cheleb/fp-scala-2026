package fp.common

/** The content of a grid cell. More elements will be added later. */
enum Cell derives CanEqual:
  case Wall, Empty, Start, Target

object Cell:
  /** Parses a grid from text: `#` Wall, `S` Start, `T` Target, anything else Empty. */
  def parse(lines: String*): Grid[Cell] =
    ??? // TODO 02
