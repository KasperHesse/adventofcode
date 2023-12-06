package y2023

import common._

class Day6 extends Solution {

  /**
   * Compute the race distances
   * @param time
   * @return
   */
  def computeRaceDistances(time: Int): List[Int] = {
    List.tabulate(time)(n => n * (time-n))
  }


  /**
   * Solve the problem as solving an equation.
   * @param time
   * @return
   */
  def solveAsEqn(t: Long, d: Long): Long = {
    /*
    Distance travelled as function of windup-time describes a parabola
    Original zero crossings of that parabola are known. x0=0 and x1=t
    Parabola describing distance travelled by boat: -(x*(x-t)) = -x^2 + t*x

    Offset that parabola by -(dist+1) to get new parabola where intersections are the
    t1,t2 locations where we first,last get better distance

    New equation: -x^2 + t*x - (d+1)
    Quadratic equation with a=-1, b=t and c= - (d+1)
    Solve as normally known with quadratic equation
     */
    val a = -1.0
    val b = t
    val c = -d-1

    //Ceil of lower value, floor of upper value to constrain range
    val t1 = ((-b + math.sqrt(math.pow(t,2) - 4*a*c))/(2*a)).ceil.toLong
    val t2 = ((-b - math.sqrt(math.pow(t,2) - 4*a*c))/(2*a)).floor.toLong

    t2 - t1 + 1L
  }

  override def solvePart1(inp: List[String]): Any = {
    //Parse time and distance values
    val times = inp.head.split(" +").tail.map(_.toInt).toList
    val dists = inp(1).split(" +").tail.map(_.toInt).toList

    times.zip(dists).map{case (t,d) => solveAsEqn(t,d)}.product
  }


  override def solvePart2(inp: List[String]): Any = {
    val t = inp.head.split(":").last.trim.split(" +").mkString("","","").toLong
    val d = inp.last.split(":").last.trim.split(" +").mkString("").toLong

    solveAsEqn(t,d)
  }
}

object Day6 extends App {
  Solution.solve(new Day6, 6, 2023, 288, 71503)
}
