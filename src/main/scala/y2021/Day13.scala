package y2021

import common.Solution

class Day13 extends Solution {

  //Parse the dots, returning a map of all coordinates with dots set
  def parseDots(inp: List[String]): Set[(Int,Int)] = {
    inp.map(_.split(","))
      .foldLeft(Set.empty[(Int,Int)]){case (s,a) => s.incl((a(0).toInt, a(1).toInt))}
  }

  //Parse the folds, returning a list of (axis, line) along which to fold
  def parseFolds(inp: List[String]): List[(String, Int)] = {
    inp.map(_.split("=")).map(a => (a(0).takeRight(1), a(1).toInt))
  }

  def doFold(m: Set[(Int,Int)], dir: String, v: Int): Set[(Int,Int)] = {
    if (dir == "y") {
      m.map{case (x,y) => if (y>v) (x, v-(y-v)) else (x,y)}
    } else { //dir == x
      m.map{case (x,y) => if (x>v) (v-(x-v), y) else (x,y)}
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val splitPoint = inp.indexOf("")
    val (first, last) = inp.splitAt(splitPoint)
    val m = parseDots(first)
    val folds = parseFolds(last.tail) //we don't want the one that is an empty line

    val m2 = doFold(m, folds.head._1, folds.head._2)
    m2.size
    //given map, perform folds
    //on y-fold: all dots with y>y-fold have their y-coordinate set to (y-y_fold), x is the same
    //on x-fold: all dot
  }

  override def solvePart2(inp: List[String]): Any = {
    val splitPoint = inp.indexOf("")
    val (first, last) = inp.splitAt(splitPoint)
    val m = parseDots(first)
    val folds = parseFolds(last.tail) //we don't want the one that is an empty line

    val res = folds.foldLeft(m)((s,f) => doFold(s, f._1, f._2))

    //Extract x-coordinates set on each line. Example only uses line 0-4, but result uses line 0-5, so we filter out empty lines for example
    val xForLine = Seq(0,1,2,3,4,5).map(y => res.collect{case (x,yy) if y==yy => x}).filterNot(_.equals(Set.empty))
    xForLine.foreach{ xfl =>
      //Generate string of that length with spaces, then set '#' where necessary
      val z = xfl.foldLeft(" " * (xfl.max+1))((str,x) => str.updated(x, '#'))
      println(z)
    }

    1
  }
}

object Day13 extends App {
  Solution.solve(new Day13, 13, 17, 1)
}