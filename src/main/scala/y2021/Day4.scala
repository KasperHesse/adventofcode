package y2021

import common.Solution

class Day4 extends Solution {
  //Board representation: Either a 2x2 array, or just a list where we can easily compute h,v offsets
  //Each board should have the numbers, as well as a way of tracking whether that number has been drawn
  //Store (Int, Boolean) tuples, each 25 long?
  case class Board(nums: List[Int], drawn: List[Boolean])

  /**
   * Get all boards in the input.
   * @param inp Input for the problem. Must be trimmed such that the first line of the given input
   *            is the first line of the first board
   * @return
   */
  def getBoards(inp: List[String]): List[Board] = {
    def buildBoard(inp: List[String]): Board = {
      Board(inp.take(5).flatMap(_.split("\\s+").filter(_.nonEmpty)).map(_.toInt), List.fill(25)(false))
    }
    inp.grouped(6).map(buildBoard).toList
  }

  /**
   * Get the Bingo numbers in the order they are drawn
   * @param inp First line of the input, with all numbers being drawn
   * @return
   */
  def getNumbers(inp: String): List[Int] = {
    inp.split(",").map(_.toInt).toList
  }

  /**
   * Checks if a board has won the game. This is the case if a horizontal or vertical row has all numbers drawn
   * @param b
   * @return
   */
  def boardIsWinner(b: Board): Boolean = {
    def extract(b: Board, idxs: Range): Seq[Boolean] = {
      idxs.map(idx => b.drawn(idx))
    }
    //Board is winner if [1:5], [6:10], ... are all true, or if [1,6,11,16,21], [2,7,12,17,22] etc are all true
    //                All horizontal ranges                                  All vertical ranges
    val ranges = List(List.tabulate(5)(n => Range(n*5, (n+1)*5)), List.tabulate(5)(n => Range(n, 25, 5))).flatten
    ranges.exists(r => extract(b, r).forall(x => x)) //checks whether there exists a range which satisfies the win condition
  }

  /**
   * Score of a bingo board
   * @param b
   * @return
   */
  def boardScore(b: Board): Int = {
    //Could also do filter->map->sum, but this is just one traversal
    (b.nums zip b.drawn).foldLeft(0)((acc, tup) => if (tup._2) acc else tup._1 + acc)
  }

  /**
   * Play Bingo, stopping when the first bingo card wins. Returns the score of that bingo card as given in the rules
   * @param nums The numbers remaning to be drawn
   * @param boards The boards in play
   * @return
   */
  def playBingo(nums: List[Int], boards: List[Board]): Int = {
    val n = nums.head
    //Go through all boards. If n exists in the list, update the associated bool to true
    val boards2 = boards.map(b => if (b.nums.contains(n))
      Board(b.nums, b.drawn.updated(b.nums.indexOf(n), true))
    else b)
    if (boards2.exists(boardIsWinner)) {
      val winner = boards2.find(boardIsWinner).get
      boardScore(winner) * n
    } else {
      playBingo(nums.tail, boards2)
    }
  }

  /**
   * Ranks all bingo cards according to their finishing order in the specified game
   * @param nums The numbers remaining in the game
   * @param boards The boards remanining in the game that still haven't won
   * @param acc Accumulating parameter. Keeps a list of (Board, win idx, score)
   */
  def rankBingoBoards(nums: List[Int], boards: List[Board], acc: List[(Board, Int, Int)]): List[(Board, Int, Int)] = {
    if (boards.isEmpty) {
      acc
    } else {
      val n = nums.head
      val boards2 = boards.map(b => if (b.nums.contains(n))
        Board(b.nums, b.drawn.updated(b.nums.indexOf(n), true))
      else b)
      //Take all boards that won this round and push onto the acc list
      val (won, notWon) = boards2.partition(boardIsWinner)
      val acc2 = won.foldLeft(acc)((a, b) => (b, a.length + 1, boardScore(b) * n) :: a)
      rankBingoBoards(nums.tail, notWon, acc2)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val nums = getNumbers(inp.head)
    val boards = getBoards(inp.tail.tail)
//    playBingo(nums, boards)
    rankBingoBoards(nums, boards, List.empty).find(_._2 == 1).get._3
  }

  override def solvePart2(inp: List[String]): Any = {
    val nums = getNumbers(inp.head)
    val boards = getBoards(inp.tail.tail)
    val ranked = rankBingoBoards(nums, boards, List.empty[(Board, Int, Int)])
    ranked.find(_._2 == ranked.length).get._3
  }
}

object Day4 extends App {
  Solution.solve(new Day4, 4, 4512, 1924)
}