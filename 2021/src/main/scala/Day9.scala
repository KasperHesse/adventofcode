import scala.collection.immutable.Queue

class Day9 extends Solution {

  def genMap(lines: List[String], y: Int, map: Map[(Int,Int), Int]): Map[(Int, Int), Int] = {
    lines match {
      case line :: tail =>
        val map2 = line.map(_.toInt - 48).zipWithIndex.foldLeft(map)((m,tup) => m.updated((tup._2, y), tup._1))
        genMap(tail, y+1, map2)
      case Nil => map
    }
  }

  def findLows(m: Map[(Int,Int), Int]): List[((Int,Int), Int)] = {
    def isLow(m: Map[(Int,Int), Int], x: Int, y: Int): Boolean = {
      val delta = List((-1,0), (1,0), (0,-1), (0,1))
      delta.map{case (dx,dy) => (x+dx, y+dy)}
        .filter(m.isDefinedAt)
        .forall(d => m(d) > m((x,y)))
    }
    m.filter{case ((x,y),_) => isLow(m, x, y)}.toList
  }

  override def solvePart1(inp: List[String]): Any = {
    val m = genMap(inp, 0, Map.empty)
    val lows = findLows(m)
    lows.map(_._2 + 1).sum
  }

  //floodfill function: recursive function. Takes the map and a queue of points still to be visited
  //Accumulator counting the number of nodes visited = size of the region. visited list =
  def floodVisit(map: Map[(Int, Int), Int], q: Queue[(Int,Int)], visited: List[(Int,Int)], visits: Int): Int = {
    if (q.isEmpty) {
      visits
    } else {
      val ((x,y), q2) = q.dequeue
      val v = map((x,y))
      val delta = List((-1,0), (1,0), (0,-1), (0,1))
      //Get all neighbours of xy which are exactly 1 greater but not 9
      val nbs = delta.foldLeft(List.empty[(Int,Int)]){
        case (acc,(dx,dy)) => val v2 = map.getOrElse((x+dx, y+dy), 10)
                               if (v2 > v && v2 < 9 && !visited.contains((x+dx,y+dy))) (x+dx,y+dy)::acc else acc}
      floodVisit(map, q2.enqueueAll(nbs), nbs.concat(visited), visits+1)
    }
  }

  override def solvePart2(inp: List[String]): Any = {
    /*
    To find basins
    Use findLows to find the low point of all basins
    Perform a BFS scan / flood fill from that point. A neighbour is accepted if it is >v, but not 9
     */
    val m = genMap(inp, 0, Map.empty)
    val lows = findLows(m)
    val basinSizes = lows.map(l => floodVisit(m, Queue(l._1), List.empty, 0)).sortWith((x,y) => x>y)
    basinSizes.take(3).product
  }
}

object Day9 extends App {
  Solution.solve(new Day9, 9, 15, 1134)
}