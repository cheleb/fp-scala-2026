package fp.common.astar

import fp.common.*
import eu.joaocosta.minart.graphics.*


trait StarAppDefault {
    val CurrentColor = Color(255, 245, 160)
    val ExploredColor = Color(170, 230, 170)
    val PathColor = Color(40, 90, 220)
    val cellSize = 24
    val sleep = 50
    val stepsPerSecond = 1000 / 50 


    val maze: Grid[Cell] = Cell.parse(
    "##############################",
    "#S     #             #       #",
    "#      #   ######    #   #   #",
    "#  #####        #    #   #   #",
    "#      #        #        #   #",
    "#      #   #    ######   #   #",
    "#          #         #   #   #",
    "#######    #######   #   #####",
    "#               #    #       #",
    "#   ######      #    ####    #",
    "#        #      #       #    #",
    "#        #  #########   #  T #",
    "#        #              #    #",
    "##############################"
  )
}
