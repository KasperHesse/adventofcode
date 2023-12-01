package y2021

import common.Solution

import scala.annotation.tailrec

sealed trait Cucumber
case class Down() extends Cucumber
case class Right() extends Cucumber


class Day25 extends Solution {

  def printMap(map: Map[(Int,Int), Cucumber], maxX: Int, maxY: Int): Unit = {
    for(y <- 0 until maxY) {
      for (x <- 0 until maxX) {
        val s = map.get((x,y)) match {
          case None => "."
          case Some(Down()) => "v"
          case Some(Right()) => ">"
        }
        print(s)
      }
      println()
    }
    println()
  }
  @tailrec
  final def moveCucumbers(map: Map[(Int,Int), Cucumber], maxX: Int, maxY: Int, steps: Int): Int = {
    //Attempt to move right first, then down
    def nextR(x: Int, y: Int): (Int, Int) = ((x+1) % maxX, y)
    def nextD(x: Int, y: Int): (Int, Int) = (x, (y+1) % maxY)
    val movedRight = map.map{
      case ((x,y), Right()) => if (map.contains(nextR(x,y))) ((x,y), Right()) else (nextR(x,y), Right())
      case o => o
    }
    val movedDown = movedRight.map{
      case ((x,y), Down()) => if (movedRight.contains(nextD(x,y))) ((x,y), Down()) else (nextD(x,y), Down())
      case o => o
    }
    if (movedDown == map) {
//    if (steps == 1) {
//      printMap(map, maxX, maxY)
      steps+1
    } else {
      moveCucumbers(movedDown, maxX, maxY, steps+1)
    }
  }
  override def solvePart1(inp: List[String]): Any = {
    val m1 = Solution.parseAsMap(inp)
    val map: Map[(Int,Int), Cucumber] = m1.collect{case (xy, '>') => (xy, Right()); case (xy, 'v') => (xy, Down())}
    val maxX = m1.keys.map(_._1).max + 1
    val maxY = m1.keys.map(_._2).max + 1
    println(s"maxX: $maxX,  maxY: $maxY")
//    printMap(map, maxX, maxY)
    val r = moveCucumbers(map, maxX, maxY, 0)
    println(r)
    r
  }
}

object Day25 extends App {
  Solution.solve2021(new Day25, 25, 58, 1)
}