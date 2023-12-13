package y2023

import common._

class Day12 extends Solution {
  case class State(pos: Int, hashCnt: Int, hashLens: List[Int])

  def findCombinations(
                            str: String,
                            s: State,
                            memo: Map[State, Long]
                      ): Map[State, Long] = {

    def genDotNS(s: State): Option[State] = {
      /*
      Three options
      - hashCnt = 0. NS has pos+1 but otherwise unchanged values
      - hashCnt = hashLen. NS has pos+1, hashCnt=0, lens.tail
      - hashCnt < hashLen. Bad state, not worth investigating
       */
      if (s.hashCnt == 0) {
        Some(State(s.pos + 1, 0, s.hashLens))
      } else if (s.hashCnt == s.hashLens.head) {
        Some(State(s.pos + 1, 0, s.hashLens.tail))
      } else {
        None
      }
    }

    def genHashNS(s: State): Option[State] = {
      /*
      Two options
      - hashCnt < hashLens.head: OK to discover '#' in that case
      - otherwise: Should not disover a '#'
       */
      if (s.hashLens.isEmpty || (s.hashCnt == s.hashLens.head)) {
        None
      } else { //This clause last, as we would have to probe s.hashLens.head which might be empty
        Some(State(s.pos + 1, s.hashCnt + 1, s.hashLens))
      }
    }

    /*
    For each choice: Search sub-space for that choice, find combinations on that sub-state, then retrieve value from returned map
    Accumulate, then update mapping for this state
     */
    if (memo.contains(s)) { //State already computed, just return memo without updating
      memo
    } else if (s.pos == str.length) {
      //EOL, finish state
      if ((s.hashLens.isEmpty && s.hashCnt == 0) || (s.hashLens.length == 1 && s.hashLens.head == s.hashCnt)) {
        //Good finish state
        memo.updated(s, 1)
      } else { //Bad finish state, still need to find some '#' values
        memo.updated(s, 0)
      }
    } else if (s.hashLens.isEmpty) {
      //Early finish when no more '#' required
      if (str.substring(s.pos).count(_=='#') == 0) {
        //No more '#' present in substr, good finish state
        memo.updated(s, 1)
      } else {
        //Still some '#' present but none required, bad finish state
        memo.updated(s, 0)
      }
    } else { //Not EOL
      val ns = (str(s.pos) match {
        case '.' => List(genDotNS(s))
        case '#' => List(genHashNS(s))
        case '?' => List(genDotNS(s), genHashNS(s))
        case _ => throw new IllegalArgumentException(s"Unknown character ${str(s.pos)} in $str")
      }).flatten //Flatten to throw away Options, leave only valid next-states

      //If no next state, this state cannot lead to anything good. Update memo for this to 0
      //If next-states exist, sum up those next-state counts, add to this state
      if (ns.isEmpty) {
        memo.updated(s, 0)
      } else {
        val newMemo = ns.foldLeft(memo){case (memo,state) => findCombinations(str, state, memo)}
        val res = ns.map(newMemo).sum
        newMemo.updated(s, res)
      }
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val inputs = inp.map{case s"$str $len" => (str, len.split(",").map(_.toInt).toList)}
    inputs.map{case (str,lens) =>
      val initState = State(0, 0, lens)
      val m = findCombinations(str, initState, Map.empty)
      m(initState)
    }.sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val inputs = inp.map{case s"$str $len" => (str, len.split(",").map(_.toInt).toList)}
    val newInputs = inputs.map{case (str,len) => (s"$str?$str?$str?$str?$str", List.fill(5)(len).flatten)}

    newInputs.map{case (str,lens) =>
      val initState = State(0, 0, lens)
      val m = findCombinations(str, initState, Map.empty)
      m(initState)
    }.sum
  }
}

object Day12 extends App {
  Solution.solve(new Day12, 12, 2023, 21, 525152)
}