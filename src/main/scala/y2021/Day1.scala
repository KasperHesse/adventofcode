package y2021

import common.Solution

class Day1 extends Solution {

  override def solvePart1(inp: List[String]): Any = {
    //Part 1: How many times does the depth measurement increase?
    inp.map(_.toInt).sliding(2).count(x => x.head < x.last)
  }

  override def solvePart2(inp: List[String]): Any = {
    //Part 2: How many times does it increase (sliding window of size 3)
    inp.map(_.toInt).sliding(3).map(_.sum).sliding(2).count(x => x.head < x.last)
  }
}

object Day1 extends App {
  Solution.solve(new Day1,1,  7, 5)
}