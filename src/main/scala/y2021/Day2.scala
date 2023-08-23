package y2021

import common.Solution

class Day2 extends Solution {
  sealed trait Move
  case class Forward(x: Int) extends Move
  case class Down(x: Int) extends Move
  case class Up(x: Int) extends Move

  def toMove(s: String): Move = {
    val x = s.split(" ")
    x(0) match {
      case "forward" => Forward(x(1).toInt)
      case "down" => Down(x(1).toInt)
      case "up" => Up(x(1).toInt)
      case _ => throw new IllegalArgumentException("Did not recognize move")
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    //Compute (hor pos, depth)
    val z = inp.map(toMove).foldLeft((0,0)){case ((h,d),m) => m match {
      case Forward(x) => (h, d+x)
      case Down(x) => (h+x, d)
      case Up(x) => (h-x, d)
    }}
    z._1 * z._2
  }

  override def solvePart2(inp: List[String]): Any = {
    //Compute (hor,depth,aim)
    val z = inp.map(toMove).foldLeft((0,0,0))({case ((h,d,a),m) => m match {
      case Forward(x) => (h+x, d+a*x, a)
      case Down(x) => (h, d, a+x)
      case Up(x) => (h, d, a-x)
      }})
    z._1 * z._2
  }
}

object Day2 extends App {
  Solution.solve(new Day2, 2, 150, 900)
}