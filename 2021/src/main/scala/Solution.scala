import scala.io.Source

abstract class Solution {

  def solvePart1(inp: List[String]): Any = {
    0
  }

  def solvePart2(inp: List[String]): Any = {
    0
  }

}

object Solution extends App {
  def readInput(f: String): List[String] = {
    val src = Source.fromFile(f"src/main/resources/$f.txt")

    val lines = src.getLines().toList
    src.close()
    lines
  }

  def parseAsIntMap(inp: List[String]): Map[(Int,Int), Int] = {
    val z = inp.zipWithIndex.flatMap{case (s,y) => s.zipWithIndex.map{case (c,x) => (c.toInt - 48, x, y)}} //converted to list of (value, x, y)
      .foldLeft(Map.empty[(Int,Int), Int]){case (m, (v, x, y)) => m.updated((x,y), v)}
    z
  }

  def getRectMapBounds[T](V: Map[(Int,Int), T]): (Int,Int,Int,Int) = {
    val (maxX, maxY) = (V.keys.maxBy(_._1)._1, V.keys.maxBy(_._2)._2)
    val (minX, minY) = (V.keys.minBy(_._1)._1, V.keys.minBy(_._2)._2)
    (minX, maxX, minY, maxY)
  }

  def genIntMapNeighbours(minX: Int, maxX: Int, minY: Int, maxY: Int): Map[(Int,Int), List[(Int,Int)]] = {
    val dxy = List((-1,0), (1,0), (0,-1), (0,1)) //Possible neighbours
    val xy = Seq.tabulate(maxX+1)(x => Seq.tabulate(maxY+1)(y => (x,y))).flatten //X,Y coords to start from
    val E = xy.foldLeft(Map.empty[(Int,Int), List[(Int,Int)]]) { case (m, (x, y)) =>
      val nbs = dxy.map { case (dx, dy) => (x + dx, y + dy) } //Map to neighbour coords
        .filter(XY => (minX <= XY._1 && XY._1 <= maxX) && (minY <= XY._2 && XY._2 <= maxY)) //Filter for legal neighbours
      m.updated((x, y), nbs)
    }
    E
  }


  def solve(solver: Solution, day: Int, part1TestExp: Any, part2TestExp: Any): Unit = {
    val inp = readInput(f"d$day")
    val testInp = readInput(s"d${day}test")

    val part1Test = solver.solvePart1(testInp)
    if (part1Test == part1TestExp) {
      println(s"Solution for part 1: ${solver.solvePart1(inp)}")
    } else {
      println(s"ERR: Bad solution for part 1 test:\nExpected: $part1TestExp\nGot: $part1Test")
    }

    val part2Test = solver.solvePart2(testInp)
    if (part2Test == part2TestExp) {
      println(s"Solution for part 2: ${solver.solvePart2(inp)}")
    } else {
      println(s"ERR: Bad solution for part 2 test:\n  Expected: $part2TestExp\n  Got: $part2Test")
    }
  }
}