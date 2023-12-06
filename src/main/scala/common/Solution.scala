package common

import scala.io.Source

abstract class Solution {

  implicit class ListOps[T](xs: List[T]) {
    /**
     * Generates all pairs of elements in the two lists
     * @param that
     * @return
     */
    def allPairs(that: List[T]): List[(T,T)] = for (x <- xs; y <- that) yield (x,y)
    def pairwise(): List[(T,T)] = xs zip xs.tail
  }

  implicit class SetOps[T](set: Set[T]) {
    def allPairs(that: Set[T]): Set[(T,T)] = for (x <- set; y <- that) yield (x,y)
  }


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

  /**
   * Parse the input as a map, returning a map that is (x,y) indexed, values being the
   * Int value at a given location
   * @param inp
   * @return
   */
  def parseAsIntMap(inp: List[String]): Map[(Int,Int), Int] = {
    parseAsMap(inp).map{case (k,v) => (k, v.toInt-48)}
  }

  /**
   * Parse the input as a map, returning a map that is (x,y) indexed, values being the Char
   * at a given location
   * @param inp
   * @return
   */
  def parseAsMap(inp: List[String]): Map[(Int,Int), Char] = {
    inp.zipWithIndex.flatMap{case (s,y) => s.zipWithIndex.map{case (c,x) => ((x,y),c)}}.toMap
  }

  /**
   * Get the bounds of a rectangular map, returning the min and max x,y values that can be indexed
   * @param V
   * @tparam T
   * @return (minX, maxX, minY, maxY)
   */
  def getRectMapBounds[T](V: Map[(Int,Int), T]): (Int,Int,Int,Int) = {
    val (maxX, maxY) = (V.keys.maxBy(_._1)._1, V.keys.maxBy(_._2)._2)
    val (minX, minY) = (V.keys.minBy(_._1)._1, V.keys.minBy(_._2)._2)
    (minX, maxX, minY, maxY)
  }

  /**
   * Generates a map from (x,y) to all legal neighbours of that (x,y) location in a map
   * @param minX
   * @param maxX
   * @param minY
   * @param maxY
   * @return
   */
  def getMapNeighbours(minX: Int, maxX: Int, minY: Int, maxY: Int): Map[(Int,Int), List[(Int,Int)]] = {
    val dxy = List((-1,0), (1,0), (0,-1), (0,1)) //Possible neighbours
    val xy = Seq.tabulate(maxX+1)(x => Seq.tabulate(maxY+1)(y => (x,y))).flatten //X,Y coords to start from
    val E = xy.foldLeft(Map.empty[(Int,Int), List[(Int,Int)]]) { case (m, (x, y)) =>
      val nbs = dxy.map { case (dx, dy) => (x + dx, y + dy) } //Map to neighbour coords
        .filter(XY => (minX <= XY._1 && XY._1 <= maxX) && (minY <= XY._2 && XY._2 <= maxY)) //Filter for legal neighbours
      m.updated((x, y), nbs)
    }
    E
  }

  /**
   * Split the input into multiple sub-lists, splitting on lines that only consist of a newline character
   * @param inp
   * @return
   */
  def splitOnBlankLines(inp: List[String]): List[List[String]] = {
    val split = inp.foldLeft(List(List.empty[String])){case (h::t, s) => if (s.isBlank) {
      List.empty[String]::(h::t)
    } else {
      (s::h)::t
    }}
    //Each block is added to sublist in revers order, so must reverse all at the end + reverse order of parsed list
    split.map(_.reverse).reverse
  }

  def solve2023(solver: Solution, day: Int, part1TestExp: Any, part2TestExp: Any): Unit = {
    solve(solver, day, 2023, part1TestExp, part2TestExp)
  }

  def solve(solver: Solution, day: Int, year: Int, part1TestExp: Any, part2TestExp: Any): Unit = {
    val inp = readInput(f"y$year/d$day")
    val testInp = readInput(s"y$year/d${day}test")

    val part1Test = solver.solvePart1(testInp)
    if (part1Test == part1TestExp) {
      println(s"Solution for part 1: ${solver.solvePart1(inp)}")
    } else {
      println(s"ERR: Bad solution for part 1 test:\nExpected: $part1TestExp\nGot:      $part1Test")
    }

    val part2Test = solver.solvePart2(testInp)
    if (part2Test == part2TestExp) {
      println(s"Solution for part 2: ${solver.solvePart2(inp)}")
    } else {
      println(s"ERR: Bad solution for part 2 test:\n  Expected: $part2TestExp\n  Got: $part2Test")
    }
  }

  def solve2021(solver: Solution, day: Int, part1TestExp: Any, part2TestExp: Any): Unit = {
    solve(solver, day, 2021, part1TestExp, part2TestExp)
  }
}