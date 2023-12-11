package y2023

import common._

class Day9 extends Solution {

  def genTriangle(ints: List[Long], acc: List[List[Long]]): List[List[Long]] = {
    if (ints.forall(_==0L)) {
      acc
    } else {
      val next = ints.sliding(2).map(x => x.last - x.head).toList
      genTriangle(next, next::acc)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val ints = inp.map(_.split(" ").map(_.toLong).toList)
    val triangles = ints.map(x => genTriangle(x, List(x)))
    triangles.map(_.map(_.last).sum).sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val ints = inp.map(_.split(" ").map(_.toLong).toList)
    //Compute new first value
    //Given by simply reversing the inputs (does this always work??)
    val triangles = ints.map(x => genTriangle(x.reverse, List(x.reverse)))
    triangles.map(_.map(_.last).sum).sum
  }
}

object Day9 extends App {
  Solution.solve(new Day9, 9, 2023, 114, 2)
}
