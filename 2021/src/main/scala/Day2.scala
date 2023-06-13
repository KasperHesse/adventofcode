class Day2 extends Solution {
  override def solvePart1(inp: List[String]): Any = {
    //Compute final (hor,depth) position
    val z = inp.map(_.split(" ")).map(x => (x(0), x(1).toInt))
      .fold((0,0)){case ((h: Int, d: Int),(cmd,i)) => cmd match {
        case "forward" => (h, d+i)
        case "down" => (h+i, d)
        case "up" => (h-i, d)
      }}
    z._1.asInstanceOf[Int] * z._2
  }

  override def solvePart2(inp: List[String]): Any = {
    //Compute (hor,depth,aim)
    val z = inp.map(_.split(" ")).map(x => (x(0), x(1).toInt))
      .fold((0,0,0))({case ((h: Int, d: Int, a: Int),(cmd,x: Int)) => cmd match {
        case "forward" => (h+x, d+a*x, a)
        case "down" => (h, d, a+x)
        case "up" => (h, d, a-x)
      }})
    z.productElement(0).asInstanceOf[Int] * z.productElement(1).asInstanceOf[Int]
  }
}

object Day2 extends App {
  Solution.solve(new Day2, 2, 150, 900)
}