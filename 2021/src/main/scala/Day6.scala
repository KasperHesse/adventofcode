class Day6 extends Solution {
  def evolve(fish: Map[Int,Long], remIter: Int): Map[Int,Long] = {
    if (remIter > 0) {
      val zeros = fish.getOrElse(0, 0L)
      //Update fish counters. Decrement unless 0, in that case set to 6.
      val fishDec = fish.foldLeft(Map.empty[Int,Long])(
        (fd, tup) => fd.updatedWith(if (tup._1 == 0) 6 else tup._1 - 1)(cnt => if (cnt.isEmpty) Some(tup._2) else Some(cnt.get + tup._2)))
      val fishNew = fishDec.updated(8, zeros)
      evolve(fishNew, remIter-1)
    } else {
      fish
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    //Convert to list of int. Then recurse over that list for some number of iterations
    val start = inp.head.split(",")
      .map(_.toInt)
      .toList
    //Convert starting fish list to mapping from (days left) => (num fish)
    val m = start.foldLeft(Map.empty[Int,Long])((m,f) => m.updatedWith(f)(cnt => if (cnt.isEmpty) Some(1) else Some(cnt.get + 1)))
    evolve(m, 80).values.sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val start = inp.head.split(",")
      .map(_.toInt)
      .toList
    val m = start.foldLeft(Map.empty[Int,Long])((m,f) => m.updatedWith(f)(cnt => if (cnt.isEmpty) Some(1) else Some(cnt.get + 1)))
    evolve(m, 256).values.sum
  }
}

object Day6 extends App {
  Solution.solve(new Day6, 6, 5934, 26984457539L)
}