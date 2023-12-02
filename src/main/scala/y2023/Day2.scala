package y2023

import common.Solution

class Day2 extends Solution {

  //Parse a pull: Return (red, green, blue)
  def parsePull(p: String): (Int, Int, Int) = {
    val s = p.trim.split(" ")
    s.last match {
      case "red" => (s.head.toInt, 0, 0)
      case "green" => (0, s.head.toInt, 0)
      case "blue" => (0, 0, s.head.toInt)
      case _ => throw new IllegalArgumentException(s"Unknown color ${s.last}")
    }
  }

  def isLegalPull(pull: (Int, Int, Int)): Boolean = {
    val maxRed = 12
    val maxGreen = 13
    val maxBlue = 14
    pull._1 <= maxRed && pull._2 <= maxGreen && pull._3 <= maxBlue
  }

  def parseGames(inp: List[String]): List[(Int, Array[(Int, Int, Int)])] = {
    inp.map { x =>
      val s = x.split(":")
      val id = s.head.split(" ").last.toInt
      //replace+split since we don't care about difference between "," and ";"
      val pulls = s.last.replace(';', ',').split(",").map(parsePull)
      (id, pulls)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val games = parseGames(inp)
    //flatMap with Some/None return is a nice way of filter+mapping
    games.flatMap{case (id,pulls) => if (pulls.forall(isLegalPull)) Some(id) else None}
      .sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val games = parseGames(inp)
    games.map{case (_,pulls) => pulls.foldLeft((0,0,0)){case (cnt, pull) =>
      (cnt._1.max(pull._1), cnt._2.max(pull._2), cnt._3.max(pull._3))
    }}
      .map(x => x._1 * x._2 * x._3)
      .sum
  }
}

object Day2 extends App {
  Solution.solve2023(new Day2, 2, 8, 2286)
}