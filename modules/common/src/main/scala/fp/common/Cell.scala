package fp.common

/** The content of a grid cell. More elements will be added later. */
enum Cell derives CanEqual:
  case Wall, Empty, Start, Target

object Cell:
  /** Parses a grid from text: `#` Wall, `S` Start, `T` Target, anything else Empty.
   * 
   * 
   * ```scala 
   * Cell.parse(
   *  "##########",
   *  "#S       #",
   *  "#  ##    #",
   *  "#   #  # #",
   *  "#      #T#",
   *  "##########"
   * )
   * ```
   * 
   * 
   */
  def parse(lines: String*): Grid[Cell] =
    // TODO 02
    // Max length of lines
    val p: Int = ???
    // Leverage Grid.tabulate and Seq#lift
    ???  
