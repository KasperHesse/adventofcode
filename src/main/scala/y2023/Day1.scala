package y2023

import common.Solution

class Day1 extends Solution {
  override def solvePart1(inp: List[String]): Any = {
    inp.map(_.collect{case x: Char if x.isDigit => x.asDigit})
      .map(x => x.head * 10 + x.last)
      .sum
  }

  override def solvePart2(inp: List[String]): Any = {
    //Also allow overlapping matches
    val r = "(?=(one|two|three|four|five|six|seven|eight|nine|\\d))".r

    def toInt(x: String): Int = x match {
      case "one" | "1" => 1
      case "two" | "2" => 2
      case "three" | "3" => 3
      case "four" | "4" => 4
      case "five" | "5" => 5
      case "six" | "6" => 6
      case "seven" | "7" => 7
      case "eight" | "8" => 8
      case "nine" | "9" => 9
      case x => throw new IllegalArgumentException(s"Unknown string $x")
    }

    inp.map(x =>
      r.findAllMatchIn(x) //Match with regex. Due to lookahead matching, must use this construct
        .map(_.group(1))
        .toList)
      .map(x => toInt(x.head) * 10 + toInt(x.last))
      .sum
  }
}

object Day1 extends App {
  Solution.solve(new Day1, 1, 2023, 142, 142)
}