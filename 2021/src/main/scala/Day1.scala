class Day1 extends Solution {

  override def solvePart1(inp: List[String]): Any = {
    super.solvePart1(inp)

    //Part 1: How many times does the depth measurement increase?
    val asInt = inp.map(_.toInt)
    asInt.zip(asInt.tail)
      .foldLeft(0)((cnt, tup) => if (tup._2 > tup._1) cnt+1 else cnt)
  }

  override def solvePart2(inp: List[String]): Any = {
    super.solvePart2(inp)

    //Part 2: How many times does it increase (sliding window of size 3)
    val asInt = inp.map(_.toInt)
    val triplesMapped = asInt.lazyZip(asInt.tail).lazyZip(asInt.tail.tail).map((a,b,c) => a+b+c)
    triplesMapped.zip(triplesMapped.tail)
      .foldLeft(0)((cnt,tup) => if (tup._2 > tup._1) cnt+1 else cnt)
  }
}

object Day1 extends App {
  Solution.solve(new Day1,1,  7, 5)
}