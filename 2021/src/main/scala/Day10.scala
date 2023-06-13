class Day10 extends Solution {

  //Instead of just returning bad char, also return exp list
  def parseChunk(l: String, exp: List[Char]): (Char, List[Char]) = {
    val expMap = Map.from(Seq(('(',')'),('{','}'),('[',']'),('<','>')))
    //if open: Grab it, parse next
    //if called and immediately see close: return without modification
    if (l.isEmpty) {
      (0, exp)
    } else {
      if (Seq('(','[','{','<').contains(l.head)) {
        parseChunk(l.tail, expMap(l.head)::exp)
      } else {
        if (l.head == exp.head) {
          parseChunk(l.tail, exp.tail)
        } else {
          (l.head, exp)
        }
      }
    }
  }

  def charScore(c: Char): Int = c match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
    case _   => 0
  }

  def charScoreV2(c: Char): Long = c match {
    case ')' => 1L
    case ']' => 2L
    case '}' => 3L
    case '>' => 4L
    case _   => 0L
  }
  override def solvePart1(inp: List[String]): Any = {
    inp.map(s => parseChunk(s, List.empty)._1)
      .map(charScore)
      .sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val incomplete = inp.filter(s => parseChunk(s, List.empty)._1 == 0)
    val rems = incomplete.map(s => parseChunk(s, List.empty)._2)
    val scores = rems.map(r => r.foldLeft(0L)((acc, c) => acc*5L+charScoreV2(c))).sorted
    val winner = scores(scores.length/2)
    winner
  }
}

object Day10 extends App {
  Solution.solve(new Day10, 10, 26397, 288957)
}