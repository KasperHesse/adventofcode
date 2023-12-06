package y2023

import common.Solution

class Day4 extends Solution {
  case class ScratchCard(id: Int, win: Set[Int], scratch: Set[Int], numWin: Int)

  /**
   * Parse a line of the input
   * @param line
   * @return Two sets. First set, the winning numbers. Second set, the scratched numbers
   */
  def parseLine(line: String): ScratchCard = {
    val s = line.split(":")
    val id = s(0).split(" +").last.toInt

    val sets = s(1).split('|').map(_.trim)
      .map(_.split(" +").map(_.toInt).toSet) //split each segment on spaces, parse as int, convert to set

    ScratchCard(id, sets(0), sets(1), sets(1).count(sets(0)))
  }


  override def solvePart1(inp: List[String]): Any = {
    //Part 1: SC value is 2^numWins-1 if numWins>0, else 0
    val r = inp.map(parseLine)
    r.foldLeft(0){case (acc,sc) => if (sc.numWin > 0) acc + math.pow(2, sc.numWin - 1).toInt else acc}
  }

  /**
   * Compute the score for part 2.
   * For each winning card at an index, increase win-cnt at next indices
   * @param idxs Remaining indices to parse
   * @param cardCount Number of each card that we own
   * @param cardMap Mapping from card ID to
   * @return
   */
  def scorePart2(idxs: List[Int], cardCount: Map[Int, Int], cardMap: Map[Int, ScratchCard]): Map[Int, Int] = {
    idxs match {
      case Nil => cardCount
      case idx :: _ =>
        val nw = cardMap(idx).numWin
        val cc2 = if (nw == 0) { //Card has no wins, don't modify structure
          cardCount
        } else { //this card has some wins.
          val incs = Seq.range(idx + 1, idx + 1 + nw) //Indices to increment
          val thisCnt = cardCount(idx) //Value to increment by
          incs.foldLeft(cardCount) { case (cc, idx) => cc.updatedWith(idx) { case Some(x) => Some(x + thisCnt) } }
        }
        scorePart2(idxs.tail, cc2, cardMap)
    }
  }

  override def solvePart2(inp: List[String]): Any = {
    val allCards = inp.map(parseLine)
    val cardMap = allCards.zipWithIndex.map{case (sc,i) => (i+1,sc)}.toMap
    val cardCount = List.range(1, 1 + allCards.length).map(v => (v,1)).toMap

    val finalCards = scorePart2(List.range(1, 1 + allCards.length), cardCount, cardMap)
    finalCards.values.sum
  }
}

object Day4 extends App {
  Solution.solve(new Day4, 4, 2023, 13, 30)
}