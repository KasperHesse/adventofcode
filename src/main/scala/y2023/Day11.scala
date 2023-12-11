package y2023

import common._

class Day11 extends Solution{

  def expandTheUniverse(inp: List[String], expand: Long): Long = {
    val map = Solution.parseAsMap(inp)
    val galaxies = map.filter{case (_,v) => v == '#'}.keys.map{case (x,y) => (x.toLong, y.toLong)}.toSet
    //Find all indices of rows,columns that do not have any #'s
    val rows = inp.zipWithIndex.collect{ case (str, i) if !str.contains('#') => i}.toSet
    val cols = inp.transpose.zipWithIndex.collect{case (str,i) if !str.contains('#') => i}.toSet

    //Compute new galaxy coordinates after expanding the universe
    val newCoords = galaxies.map{case (x,y) =>
      val dx = cols.count(_<x).toLong
      val dy = rows.count(_<y).toLong
      (x + dx * expand - dx, y + dy * expand - dy)
    }

    //Generating all pairs: Fold over input set, reducing set of candidates to join with each time
    val (_, pairs) = newCoords.foldLeft((newCoords, Set.empty[((Long,Long), (Long,Long))])){case ((rem,res),s) =>
      val rem2 = rem.excl(s)
      val res2 = res.union(Set(s).allPairs(rem2))
      (rem2, res2)
    }

    //Note, must throw to list first to avoid set gobbling duplicate distances
    pairs.toList.map{case ((x1,y1), (x2,y2)) => (x1 - x2).abs + (y1-y2).abs}.sum
  }

  override def solvePart1(inp: List[String]): Any = {
    expandTheUniverse(inp, 2L)
  }

  override def solvePart2(inp: List[String]): Any = {
    expandTheUniverse(inp, 1_000_000L)
  }
}

object Day11 extends App {
  Solution.solve(new Day11, 11, 2023, 374, 82000210)
}
