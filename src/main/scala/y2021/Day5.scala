package y2021

import common.Solution

class Day5 extends Solution {

  case class Line(val x1: Int, val y1: Int, val x2: Int, val y2: Int)

  def parseLine(l: String): Line = {
    val s = l.split(" -> ").map(_.split(","))
    Line(s(0)(0).toInt, s(0)(1).toInt, s(1)(0).toInt, s(1)(1).toInt)
  }

  def mapOverLines(lines: List[Line], map: Map[(Int,Int), Int]): Map[(Int, Int), Int] = {
    if (lines.isEmpty) {
      map
    } else {
      val l = lines.head
      //Generate range of coords
      val map2 = if (l.x1 == l.x2) {
        val r = Range.inclusive(Math.min(l.y1, l.y2), Math.max(l.y1, l.y2))
        r.foldLeft(map)((m, y) => m.updated((l.x1, y), m.getOrElse((l.x1,y), 0)+1))
      } else if (l.y1 == l.y2) {
        val r = Range.inclusive(Math.min(l.x1, l.x2), Math.max(l.x1, l.x2))
        r.foldLeft(map)((m,x) => m.updated((x, l.y1), m.getOrElse((x, l.y1), 0)+1))
      } else { //Diagonal line
        val xs = Range.inclusive(l.x1, l.x2, if (l.x1 > l.x2) -1 else 1)
        val ys = Range.inclusive(l.y1, l.y2, if (l.y1 > l.y2) -1 else 1)
        xs.zip(ys).foldLeft(map)((m,xy) => m.updated(xy, m.getOrElse(xy, 0)+1))
      }
      mapOverLines(lines.tail, map2)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    //We keep a map of (int,int)->int, update it throughout
    //For each input, lazily(?) generate all indices between the two endpoints. then increment all positions on the map
    //skip all diagonal line segments
    //parse line segments into Line objects
    val lines = inp.map(parseLine)
      .filter(l => l.x1 == l.x2 || l.y1 == l.y2)
    val m = mapOverLines(lines, Map.empty)
    m.count(kv => kv._2 > 1)
    //Sum over lines
  }

  override def solvePart2(inp: List[String]): Any = {
    val lines = inp.map(parseLine)
    val m = mapOverLines(lines, Map.empty)
    m.count(kv => kv._2 > 1)
  }
}

object Day5 extends App {
  Solution.solve2021(new Day5, 5, 5, 12)
}