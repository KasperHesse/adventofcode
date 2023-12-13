package y2023

import common._

class Day13 extends Solution {

  /**
   * Parse a block, returning the integer value associated with the valid fold
   * @param block The block to parse
   * @param verifier Function taking the list of String-pairs to be folded that verifies whether the fold is valid
   * @return
   */
  def parseBlock(block: List[String], verifier: List[(String, String)] => Boolean): Int = {
    def canFold(inp: List[String], idx: Int): Boolean = {
      //Idx denotes location where first range ends
      val size = (idx + 1).min(inp.length - (idx + 1))
      val pairs = List.tabulate(size)(x => (inp(idx - x), inp(idx + x + 1)))
      verifier(pairs)
    }

    val rows = block.length
    val cols = block.head.length

    val h = List.range(0, rows-1).find(idx => canFold(block, idx))
    //In all cases, must add 1 to result as AoC expects 1-indexed but we are 0-indexed
    if (h.isDefined) { //Horizontal fold was valid
      (h.get + 1) * 100
    } else {
      val t = block.transpose.map(_.toString) //Must map back to string as tranpose returns a List[List[Char]]
      List.range(0, cols-1).find(idx => canFold(t, idx)).get + 1
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val blocks = Solution.splitOnBlankLines(inp)
    //Verifier: All pairs of strings must match exactly
    blocks.map(b => parseBlock(b, pairs => pairs.forall(tup => tup._1 == tup._2))).sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val blocks = Solution.splitOnBlankLines(inp)
    //Verifier: Sum of hamming distances between string pairs must be exactly one.
    blocks.map(b => parseBlock(b, pairs => pairs.map{case (x,y) => x.zip(y).count{case (c1,c2) => c1 != c2}}.sum == 1)).sum
  }
}

object Day13 extends App {
  Solution.solve(new Day13, 13, 2023, 405, 400)
}
