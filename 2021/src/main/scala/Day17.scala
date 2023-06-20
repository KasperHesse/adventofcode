class Day17 extends Solution {
  override def solvePart1(inp: List[String]): Any = {
    val Regex = "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r

    val Regex(xMin,xMax,yMax,yMin) = inp.head

    //When bullet falls down, it has velocity -Y at y=0 given that it start with velocity +Y
    //Next, it falls with velocity (-Y-1) and thus falls (-Y-1) steps
    //To just reach the bottom of the target zone, initial y-velocity should be (abs(yMin)-1)
    val Y = yMax.toInt.abs

    //Height reached is 1+2+3+...Y-1
    (Y-1)*Y/2
  }

  def projectileHitsTarget(dx: Int, dy: Int, x: Int, y: Int,
                           xMin: Int,
                           xMax: Int,
                           yMin: Int,
                           yMax: Int): Boolean = {
    if (x > xMax || y < yMax) {
      false
    } else if (xMin <= x && y <= yMin) { //No need to check xMax, yMax
      true
    } else {
      val dx2 = if (dx == 0) 0 else dx-1
      val dy2 = dy - 1
      val x2 = x + dx
      val y2 = y + dy
      projectileHitsTarget(dx2, dy2, x2, y2, xMin, xMax, yMin, yMax)
    }
  }

  override def solvePart2(inp: List[String]): Any = {
    val Regex = "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
    val Regex(xmin, xmax, ymax, ymin) = inp.head

    val (xMin, xMax, yMin, yMax) = (xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)

    //Brute-force solution: Generate all possible initial dx,dy values that could potentially hit target,
    //then try them all
    //All possible x-velocities
    val xs = (1 to xMax)
    //All possible y-velocities
    val ys = (yMax to yMax.abs)

    val cands = for (dx <- xs; dy <- ys) yield (dx, dy)
    println(cands.length)
    cands.count { case (dx, dy) => projectileHitsTarget(dx, dy, 0, 0, xMin, xMax, yMin, yMax) }
  }
}

object Day17 extends App {
  Solution.solve(new Day17, 17, 45, 112)
}
