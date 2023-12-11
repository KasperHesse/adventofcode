package y2023

import common._

import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.tailrec

class Day10 extends Solution {

  /**
   * Finds the legal neighbours for a given node in the map
   * @param c
   * @param xy
   * @return
   */
  def neighbours(c: Char, xy: (Int, Int), b: (Int, Int, Int, Int)): List[(Int, Int)] = {
    val dxy = c match {
      case '|' => List((0, 1), ( 0,-1))
      case '-' => List((1, 0), (-1, 0))
      case 'L' => List((0,-1), ( 1, 0))
      case 'J' => List((0,-1), (-1, 0))
      case '7' => List((0, 1), (-1, 0))
      case 'F' => List((0, 1), ( 1, 0))
      case '.' => List.empty
      case _ => throw new IllegalArgumentException(s"Unable to find neighours for $c at $xy")
    }
    dxy.map{case (dx,dy) => (xy._1 + dx, xy._2 + dy)}
      .filter{xy => withinBounds(xy, b)}
  }

  @tailrec
  final def exploreLoop(node: (Int, Int), path: List[(Int, Int)], m: Map[(Int, Int), Char], b: (Int, Int, Int, Int)): List[(Int, Int)] = {
    if (m(node) == 'S') {
      path
    } else {
//      if (path.size % 1000 == 0) {
//        println("X")
////        return path
//      }
      val next = neighbours(m(node), node, b).filterNot(_==path.head).head
      exploreLoop(next, node::path, m ,b)
    }
  }

  def withinBounds(xy: (Int, Int), b: (Int, Int, Int, Int)): Boolean = {
    val (minX, maxX, minY, maxY) = b
    (minX <= xy._1) && (xy._1 <= maxX) && (minY <= xy._2) && (xy._2 <= maxY)
  }

  @tailrec
  final def doFloodfill(startCands: Set[(Int, Int)], visited: Set[(Int, Int)], m: Map[(Int, Int), List[(Int, Int)]]): Set[(Int,Int)] = {
    startCands match {
      case x if x.isEmpty => visited
      case _ => {
        val h = startCands.head
        val newVisited = floodfill(Queue(h), visited, m)
        val newCands = startCands.filterNot(newVisited)
        doFloodfill(newCands, newVisited, m)
      }
    }
  }

  @tailrec
  final def floodfill(queue: Queue[(Int,Int)], visited: Set[(Int,Int)], m: Map[(Int,Int), List[(Int,Int)]]): Set[(Int,Int)] = {
    val (h,q) = queue.deq()
    h match {
      case None => visited
      case Some(x) => {
        //If already visited, skip the node
        if (visited.contains(x)) {
          floodfill(q, visited, m)
        } else {
          val neighbours = m(x).filterNot(visited)
          val q2 = neighbours.foldLeft(q) { case (qq, nb) => qq.enq(nb) }
          floodfill(q2, visited.incl(x), m)
        }
      }
    }
  }


  override def solvePart1(inp: List[String]): Any = {
    val map = Solution.parseAsMap(inp)
    val Scoord = map.find{ case (_, c) => c == 'S'}.get._1
    val bounds = Solution.getRectMapBounds(map)
    //Find a legal neighbour for S (Something that could connect to S)
    val goodNeighbour = List((-1,0), (1,0), (0,-1), (0,1)).map{case (dx,dy) => (Scoord._1 + dx, Scoord._2 + dy)}
      .filter{xy => withinBounds(xy, bounds)}
      .find(nb => neighbours(map(nb), nb, bounds).contains(Scoord)).get

    val path = exploreLoop(goodNeighbour, List(Scoord), map, bounds)
    path.size/2
  }

  override def solvePart2(inp: List[String]): Any = {
    //Start by finding the original looping path, same as part1
    val map = Solution.parseAsMap(inp)
    val Scoord = map.find{ case (_, c) => c == 'S'}.get._1
    val bounds = Solution.getRectMapBounds(map)
    val goodNeighbour = List((-1,0), (1,0), (0,-1), (0,1)).map{case (dx,dy) => (Scoord._1 + dx, Scoord._2 + dy)}
      .filter{xy => withinBounds(xy, bounds)}
      .find(nb => neighbours(map(nb), nb, bounds).contains(Scoord)).get
    val path = exploreLoop(goodNeighbour, List(Scoord), map, bounds)

    //Now, upsample the grid to be twice as large in both directions. This will allow us to perform a flood fill
    //and find all nodes which are outside the loop. Knowing loop and outside nodes, can determine inside nodes

    //Upsample path: Adding link from last node discovered back to origin
    val newPath = (Scoord::path).map{case (x,y) => (x*2, y*2)}
      .pairwise
      .flatMap {
      case ((x1,y1), (x2,y2)) =>
        if (x1 == x2) {
          //if y2>y1, then y2-y1==2 and (y2-y1)/2 == 1
          //if y2<y1, then y2-y1==-2 and (y2-y1)/2 == -1
          List((x1,y1), (x1, y1 + (y2-y1)/2))
        } else { //y1 == y2
          List((x1,y1), (x1 + (x2-x1)/2, y1))
        }
    }
    //2D map with -x, +x, -y, +y connections for easy flood fill
    val newBounds = (0, bounds._2*2 +1, 0, bounds._4 * 2 + 1)
    val newMap = Solution.gen2DMap(newBounds._1, newBounds._2, newBounds._3, newBounds._4)
    //"Already visited" vertices for BFS flood fill
    val visited = newPath.toSet

    /*
    Candidates for being start nodes for flood fill
      - Bottom right corner. This should get all nodes connected to bottom and right edges due to padding fields
      being added
      - All of top row and leftmost column that is not included in path: This is because the pipe loop may obstruct the
      flood fill, so we need to specifically test all of those nodes
     */
    val startCands = Set(Set.tabulate(newBounds._2 + 1)(x => (x,0)), Set.tabulate(newBounds._4 + 1)(y => (0,y))).flatten
      .incl((newBounds._2, newBounds._4))
      .filterNot(visited)

    val outside = doFloodfill(startCands, visited, newMap).filterNot(visited)
    val outCount = outside.count { case (x, y) => x % 2 == 0 && y % 2 == 0 }
    val inCount = (bounds._2 + 1) * (bounds._4 + 1) - path.size - outCount
    inCount
  }
}

object Day10 extends App {
  Solution.solve(new Day10, 10, 2023, 80, 10)
}