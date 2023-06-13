class Day15 extends Solution {

  override def solvePart1(inp: List[String]): Any = {
    //A simple Dijkstra implementation should suffice here
    val V = Solution.parseAsIntMap(inp)
    val (minX, maxX, minY, maxY) = Solution.getRectMapBounds(V)
    val E = Solution.genIntMapNeighbours(minX, maxX, minY, maxY)
    val SPT = Dijkstra((minX,minY), (maxX, maxY), V, E)
    SPT.last._2
  }

  override def solvePart2(inp: List[String]): Any = {
    /**
     * Helper function to generate a new map
     * @param m Original map
     * @param dx Offset (steps) from original map in x-direction
     * @param dy Offset (steps) from original map in y-direction
     * @return
     */
    def newMap(m: Map[(Int,Int), Int], dx: Int, dy: Int): Map[(Int,Int), Int] = {
      val factor = m.keys.maxBy(_._1)._1 + 1
      //update all x-coords by dx*factor, all y-coords by dy*factor
      //All values become (v+dx+dy)%10, but 1 if it is higher
      m.foldLeft(Map.empty[(Int,Int), Int]){case (m,((x,y),v)) =>
        val v2 = v+dx+dy
        m.updated((x+dx*factor, y+dy*factor), if (v2 > 9) v2%10+1 else v2)
      }
    }
    //Take V, duplicate it and stitch thing together by combining maps
    val orig = Solution.parseAsIntMap(inp)
    //Coordinate offsets for each of the maps to be generated
    val mapdxy = Seq.tabulate(5)(x => Seq.tabulate(5)(y => (x,y))).flatten
    //From each of those, create a new map based on orig, then combine all maps
    val maps = mapdxy.map(dxy => newMap(orig, dxy._1, dxy._2))
    val V = maps.reduce(_++_)
    //Remaining is same as part 1
    val (minX, maxX, minY, maxY) = Solution.getRectMapBounds(V)
    val E = Solution.genIntMapNeighbours(minX, maxX, minY, maxY)
    val SPT = Dijkstra((minX,minY), (maxX, maxY), V, E)
    SPT.last._2
  }
}

object Day15 extends App {
  Solution.solve(new Day15, 15, 40, 315)
}