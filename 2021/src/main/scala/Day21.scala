class Day21 extends Solution {

  case class Player(id: Int, score: Int, pos: Int)
  case class State(p1: Player, p2: Player, turn: Int)

  /**
   *
   * @param pAct Active player to have their score updated
   * @param pPas Passive player who will not have their score updated
   * @param dice Next value that the dice will show when rolled
   * @param nRolls Total number of rolls that have been performed so far
   */
  def doGame(pAct: Player, pPas: Player, dice: Int, nRolls: Int): Int = {
    //Track rolling status
    val m1 = dice
    val (m2,m3) = dice match {
      case 100 => (1, 2)
      case 99 => (100, 1)
      case _ => (dice+1, dice+2)
    }
    val move = (m1+m2+m3) % 10
    val newDice = if (dice >= 98) (dice+3)%100 else dice+3

    val p = pAct.pos + move
    //Total move 12 is the same as moving 2
    val pos = if (p > 10) p % 11 + 1 else p
    val score = pAct.score + pos
    if (score >= 1000) {
      pPas.score * (nRolls+3)
    } else {
      //Swap passive and active player on each iteration
      doGame(pPas, Player(pAct.id, score, pos), newDice, nRolls+3)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val p1Pos = inp.head.last.toInt - 48
    val p2Pos = inp(1).last.toInt - 48
    doGame(Player(1, 0, p1Pos), Player(2, 0, p2Pos), 1, 0)
  }

  def doDirac(memo: Map[State, (Long, Long)], state: State): Map[State, (Long,Long)] = {
    /*
    For each roll: Search sub-space where that roll was performed. doDirac on sub-state, then retrieve value from returned map
    Accumulate, then update mapping for this state
     */
    if (memo.contains(state)) { //state already computed
      memo
    } else if (state.p1.score >= 21) { //New winning state for p1
      memo.updated(state, (1,0))
    } else if (state.p2.score >= 21) { //New winning state for p2
      memo.updated(state, (0,1))
    } else { //Not yet computed
      val rolls = 3 to 9
      val factors: Map[Int,Long] = Map(3->1L, 4->3L, 5->6L, 6->7L, 7->6L, 8->3L, 9->1L)
      //Fold over rolls. Use memo as initial value. Then retrieve results afterwards
      val nextStates = rolls.map{roll => if (state.turn == 1) {
          val p = state.p1.pos + roll
          val pos = if (p > 10) (p % 11) + 1 else p
          val score = state.p1.score + pos
          State(Player(1, score, pos), state.p2, 2)
        } else {
          val p = state.p2.pos + roll
          val pos = if (p > 10) (p % 11) + 1 else p
          val score = state.p2.score + pos
          State(state.p1, Player(2, score, pos), 1)
        }
      }
      //Fold over states, multiply by their corresponding roll-factor, save as this state
      //If e.g. newMemo(nextState(roll==4)) return (4,2), we can reach a universe with (p1Win=4, p2Win=2) in 3 different ways
      //This leads to this state having (12,6), as we can reach nextState(roll==4) in 3 different ways
      val newMemo = nextStates.foldLeft(memo)(doDirac)
      val stateRes = (nextStates zip rolls).foldLeft((0L,0L)){case ((p1w,p2w),(ns,r)) =>
        val stateRes = newMemo(ns)
        val factor = factors(r)
        (p1w + stateRes._1 * factor, p2w + stateRes._2 * factor)
      }
      newMemo.updated(state, stateRes)
    }
  }

  override def solvePart2(inp: List[String]): Any = {
    /*
    Totally improbably to perform a full state-space search. Instead, will use memoization to do it super fast
    Memo maps from a given state to number of wins of (p1,p2) in all universes extending from that state
    Since each state has 3 dice rolls, we get a certain distribution for how many ways a given value can be obtained in 3 rolls
    by inspecting all possible next-states (only 7 different possibilities), we can multiply results of next-state with
    how many ways we can reach that universe
    3: 1 outcome : (1,1,1)
    4: 3 outcomes: (2,1,1) (1,1,2), (1,2,1)
    5: 6 outcomes: (1,1,3), (1,3,1), (3,1,1), (1,2,2), (2,1,2), (2,2,1)
    6: 7 outcomes: (1,2,3), (1,3,2), (2,1,3), (2,3,1), (3,1,2), (3,2,1), (2,2,2)
    7: 6 outcomes: (3,3,1), (3,1,3), (1,3,3) (3,2,2), (2,3,2), (2,2,3)
    8: 3 outcomes: (2,3,3), (3,2,3), (3,3,2)
    9: 1 outcome : (3,3,3)
     */
    val p1Pos = inp.head.last.toInt - 48
    val p2Pos = inp(1).last.toInt - 48
    val initState = State(Player(1, 0, p1Pos), Player(2, 0, p2Pos), 1)
    val r = doDirac(Map.empty, initState)
    println(s"Final size of r: ${r.size}")
    Math.max(r(initState)._1, r(initState)._2)
  }
}

object Day21 extends App {
  Solution.solve(new Day21, 21, 739785, 444356092776315L)
}