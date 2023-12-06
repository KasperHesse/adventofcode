package y2023

import common.Solution

class Day3 extends Solution {

  case class PInt(x: Int, y: Int, len: Int, v: Int)

  //Parse an integer from the input
  //Return the length of the int + the parsed value
  def parseInt(s: String, x: Int, y: Int): (Int, PInt) = {
    val len = s.indexWhere(!_.isDigit)
    //Need this additional handling of 'len' when value is at end of the line
    //In that case, len=-1, so we just need to return remaining string length
    val v = if (len >= 0) s.substring(0, len).toInt else s.toInt
    (if (len >= 0) len else s.length, PInt(x, y, len, v))

  }

  def parseLine(s: String, x: Int, y: Int, acc: List[PInt]): List[PInt] = {
    if (s.isEmpty) acc else s.head match {
      case c if c.isDigit => val (len, p) = parseInt(s, x, y)
                             parseLine(s.drop(len), x+len, y, p::acc)
      case _ => parseLine(s.tail, x+1, y, acc)
    }
  }

  def getNeighbourSymbols(inpMap: Map[(Int, Int), Char], pint: PInt): List[(Int, Int, Char)] = {
    //Get all neighbour indices
    //Seq from (x-1,y-1) to (x+1, y-1), (x-1,y), (x+1,y), (x-1,y+1)-> (x+1,y+1)
    val xs = Seq.range(pint.x-1, pint.x + pint.len + 1)
    val nbs = List(xs.map(x => (x, pint.y-1)), Seq((pint.x-1, pint.y), (pint.x+pint.len, pint.y)), xs.map(x => (x, pint.y+1))).flatten
    nbs.flatMap(nb => inpMap.get(nb).map(c => (nb._1, nb._2, c))).filter(xyc => xyc._3 != '.')
  }

  def computeGearRatios(nbs: List[(PInt, (Int, Int, Char))], acc: Int): Int = {
    if (nbs.isEmpty) acc else {
      val h = nbs.head
      val f = nbs.tail.filter(_._2 == h._2)
      if (f.length == 1) { //If f.length == 1,
        computeGearRatios(nbs.tail, acc + h._1.v * f.head._1.v)
      } else {
        computeGearRatios(nbs.tail, acc)
      }
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    //Find integers on each line
    //Get all inputs
    val ints = inp.zipWithIndex.flatMap{case (s,i) => parseLine(s, 0, i, List.empty)}
    val inpMap = Solution.parseAsMap(inp)

    val legal = ints.filter(i => getNeighbourSymbols(inpMap, i).nonEmpty)
    legal.map(_.v).sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val ints = inp.zipWithIndex.flatMap{case (s,i) => parseLine(s, 0, i, List.empty)}
    val inpMap = Solution.parseAsMap(inp)

    //Map ints to their value and neighbour-symbol + location of neighbour symbol
    //Only extract the ones that have a * as neighbour
    val nbs = ints.map(i => (i, getNeighbourSymbols(inpMap, i)))
      .flatMap{case (i,l) => if (l.nonEmpty && l.head._3 == '*') Some(i, l.head) else None}

    //Find all items that are exactly twice in the list
    //Then converting to set afterwards to remove duplicates
    val gears = nbs.foldLeft(List.empty[(Int,Int,Char)]){case (acc, (_, nb)) =>
      if (nbs.count(_._2 == nb) == 2) nb::acc else acc
    }.toSet

    //Get sum
    gears.foldLeft(0){case (acc, g) => val f = nbs.filter(_._2 == g)
      acc + f(0)._1.v * f(1)._1.v}
  }
}

object Day3 extends App {
  Solution.solve(new Day3, 3, 2023, 4361, 467835)
}
