package y2023

import common._

import scala.annotation.tailrec

class Day8 extends Solution {

  /**
   * Walk from a start location to a destination, counting steps
   * @param steps The list of unused steps
   * @param usedSteps The list of used steps. Once steps is emptied, usedSteps will be reversed and re-used for stepping
   * @param loc The current location
   * @param destPred Predicate used to detect when the destination is reached
   * @param map The map of (loc => (left,right))
   * @param cnt Number of steps taken
   * @return
   */
  @tailrec
  final def doWalk(steps: List[Char],
                   usedSteps: List[Char],
                   loc: String,
                   destPred: String => Boolean,
                   map: Map[String, (String, String)],
                   cnt: Int): Int = {
    if (destPred(loc)) {
      cnt
    } else {
      if (steps.isEmpty) {
        doWalk(usedSteps.reverse, List.empty, loc, destPred, map, cnt)
      } else {
        val next = if (steps.head == 'L') map(loc)._1 else map(loc)._2
        doWalk(steps.tail, steps.head::usedSteps, next, destPred, map, cnt+1)
      }
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val steps = inp.head.toList
    val map = inp.drop(2).foldLeft(Map.empty[String, (String,String)]){
      case (m,s"$o = ($l, $r)") => m.updated(o, (l,r))
    }

    //Part 1: Finished when node "ZZZ" is reached
    doWalk(steps, Nil, "AAA", s => s == "ZZZ", map, 0)
  }

  override def solvePart2(inp: List[String]): Any = {
    val steps = inp.head.toList
    val map = inp.drop(2).foldLeft(Map.empty[String, (String,String)]){
      case (m,s"$o = ($l, $r)") => m.updated(o, (l,r))
    }
    val starts = map.keys.filter(_.endsWith("A")).toList

    /*
    For part 2: Each destination has a fixed cycle time
    Once cycle time is found, must find least common multiple of all cycle times
    to get first time all destination nodes are reached at the same time
     */
    //Part 2: Finished when node ending with "Z" is reached
    val cnts = starts.map(start => doWalk(steps, Nil, start, s => s.endsWith("Z"), map, 0))
    leastCommonMultiple(cnts.map(_.toLong))
  }
}

object Day8 extends App {
  Solution.solve(new Day8, 8, 2023, 6, 6)
}
