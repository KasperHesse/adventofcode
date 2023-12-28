package y2023

import common._

class Day23 extends Solution {

  /**
   * Convert the input 2D graph to a graph where only legal edges are present
   * @param map The input 2D map
   * @param nbGraph The possible neighbours for each field in the input map
   * @return A mapping for each legal Vec2D to all legal neighbours that it can access
   */
  def createMap(map: Map[(Int, Int), Char], nbGraph: Map[(Int, Int), List[(Int,Int)]]): Map[Vec2D, Set[Vec2D]] = {
    map.collect{case ((x,y),v) if v != '#' =>
      val nbs = v match {
        case '.' => nbGraph((x,y)).filter(nb => (nb, map(nb)) match { //When '.', check adjacent neighbours to see if edge is legal
          case ((x2,y2),c) => c == '.' || //if dot, always legal neighbour
            ((y == y2 && ((x2 == x + 1 && c == '>') || (x2 == x - 1 && c == '<'))) || //if same y, must be valid arrow right/left
              (x == x2 && ((y2 == y - 1 && c == '^') || (y2 == y + 1 && c == 'v')))) //if same x, must be valid arrow up/down
        }).toSet
        case '>' => Set((x+1,y)) //Arrows are assumed to always point to something valid and not a wall
        case '<' => Set((x-1, y))
        case 'v' => Set((x, y+1))
        case '^' => Set((x, y-1))
      }
      (Vec2D(x,y), nbs.map(xy => Vec2D(xy._1, xy._2)))
    }
  }


  override def solvePart1(inp: List[String]): Any = {
    /*
    Need to parse into graph. match letters
    # => no neighbours
    . => all neighbours that are not #
    <v>^ => only neighbour in that direction

    Once parsed, perform a DFS search of the space, remembering longest distance found to goal
     */

    val initGraph = Solution.parseAsMap(inp)
    val bounds = Solution.getRectMapBounds(initGraph)
    val nbs = Solution.gen2DMap(bounds._1, bounds._2, bounds._3, bounds._4)
    val map = createMap(initGraph, nbs)
    val G = DirectedGraph(map).contracted

    val topLeft = Vec2D(1, 0)
    val botRight = Vec2D(bounds._2 -1, bounds._4) //maxX - 1, maxY
    GraphOps.longestPath(G, topLeft, botRight)
  }

  override def solvePart2(inp: List[String]): Any = {
    val initGraph = Solution.parseAsMap(inp)
    val bounds = Solution.getRectMapBounds(initGraph)
    val nbs = Solution.gen2DMap(bounds._1, bounds._2, bounds._3, bounds._4)
    val map = createMap(initGraph, nbs)
    val G = UndirectedGraph(map).contracted

    val topLeft = Vec2D(1, 0)
    val botRight = Vec2D(bounds._2 -1, bounds._4) //maxX - 1, maxY
    GraphOps.longestPath(G, topLeft, botRight)
  }
}

object Day23 extends App {
  Solution.solve(new Day23, 23, 2023, 94, 154)
}
