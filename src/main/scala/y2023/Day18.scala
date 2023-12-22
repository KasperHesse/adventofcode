package y2023

import common._

import scala.annotation.tailrec

class Day18 extends Solution {

  /**
   * Use the shoelace theorem to calculate area of a polygon
   * @param nodes The coordinates of the nodes. Assumed that nodes.head == nodes.last
   * @param acc Accumulating parameter
   * @return The area of the polygon described by the nodes
   */
  @tailrec
  final def shoelace(nodes: List[Vec2D], acc: Long = 0): Long = {
    nodes match {
      case v1::v2::tail =>
        val a = acc + ((v1.x * v2.y) - (v2.x * v1.y))
        shoelace(v2::tail, a)
      case _ => acc.abs/2
    }
  }

  /**
   * Parse the given input steps, returning a list of coordinates in the plane
   * @param inp The inputs from the problem
   * @return List of coordinates of the corners of the polygon. return.head == return.last == (0,0)
   */
  def parseSteps(inp: List[String]): List[Vec2D] = {
    inp.foldLeft(List(Vec2D(0,0))){case (acc, s"$dir $amt $_") =>
      (dir, amt.toInt) match {
        case ("R", dx) => (acc.head + Vec2D(dx, 0))::acc
        case ("U", dy)  => (acc.head + Vec2D(0, dy))::acc
        case ("L", dx) => (acc.head + Vec2D(-dx, 0))::acc
        case ("D", dy) => (acc.head + Vec2D(0, -dy))::acc
      }
    }
  }

  /**
   * Map the hex color codes to instruction steps, such that they can be parsed by [[parseSteps]]
   * @param in The input problem string to map
   * @return A new problem string, now using the hex-encoded instructions as the original instruction part
   */
  def mapColorCode(in: String): String = {
    val numToDir = Map('0' -> 'R', '1' -> 'D', '2' -> 'L', '3' -> 'U')
    in match {
      case s"$_ (#$color)" =>
        val dir = numToDir(color.last)
        val steps = Integer.parseInt(color.dropRight(1), 16)
        s"$dir $steps rest"
      case _ => throw new IllegalArgumentException("Could not parse input")
    }
  }

  /**
   * Count the number of boundary points on a polygon
   * @param nodes The coordinates of the nodes. Assumed that nodes.head == nodes.last
   * @param acc Accumulating parameter
   * @return The number of boundary points on the polygon
   */
  @tailrec
  final def countBoundaryPoints(nodes: List[Vec2D], acc: Long = 0): Long = {
    nodes match {
      case v1::v2::tail =>
        val a = acc + (v2.x - v1.x).abs + (v2.y - v1.y).abs
        countBoundaryPoints(v2::tail, a)
      case _ => acc
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    /*
    Using shoelace theorem and Pick's theorem
    shoelace: gives area A of bounded polygon
    pick's theorem: A = i + b/2 - 1, where i,b are interior and boundary points with integer coordinates

    Can solve for (i+b), in Pick's theorem and get
    i + b = A + b/2 + 1
     */
    val nodes = parseSteps(inp)
    val A = shoelace(nodes)
    val b = countBoundaryPoints(nodes)
    A + b/2 + 1
  }

  override def solvePart2(inp: List[String]): Any = {
    val nodes = parseSteps(inp.map(mapColorCode))
    val A = shoelace(nodes)
    val b = countBoundaryPoints(nodes)
    A + b/2 + 1
  }
}

object Day18 extends App {
  Solution.solve(new Day18, 18, 2023, 62, 952408144115L)
}
