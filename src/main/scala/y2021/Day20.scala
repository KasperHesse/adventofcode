package y2021

import common.Solution

class Day20 extends Solution {

  case class Bounds(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  def parseAlgorithm(inp: String): Array[Boolean] = {
    inp.map { case '#' => true; case '.' => false }.toArray
  }

  def parseImage(inp: List[String]): Set[(Int, Int)] = {
    Solution.parseAsMap(inp).filter{ case (_, v) => v == '#'}.keys.toSet
  }

  def getBounds(img: Set[(Int,Int)]): Bounds = {
    val xmin = img.minBy(_._1)._1
    val xmax = img.maxBy(_._1)._1
    val ymin = img.minBy(_._2)._2
    val ymax = img.maxBy(_._2)._2
    Bounds(xmin, xmax, ymin, ymax)
  }

  def inBounds(pixel: (Int,Int), bounds: Bounds): Boolean = {
    val (x,y) = pixel
    bounds.xMin <= x && x <= bounds.xMax &&
    bounds.yMin <= y && y <= bounds.yMax
  }

  def getPixelMapping(img: Set[(Int,Int)], pixel: (Int,Int), bounds: Bounds, rem: Int, oob: Seq[Boolean]): Int = {
    //Generate neighbourhood
    val delta = List((-1,-1), (0,-1), (1,-1), (-1,0), (0,0), (1,0), (-1,1), (0,1), (1,1))
    val nb = delta.map(d => (d._1 + pixel._1, d._2 + pixel._2))
    //Get bin string from neighbours, then parse to int to index into algorithm
    val bin = nb.map {xy =>
      val v = if (inBounds(xy, bounds)) img.contains(xy) else oob(rem % 2)
      if (v) "1" else "0"
    }.mkString("","","")
    if (pixel._1 == -1 && pixel._2 == -1) {
    }
    Integer.parseInt(bin, 2)
  }

  def enhance(img: Set[(Int,Int)], algo: Array[Boolean], rem: Int, oob: Seq[Boolean]): Set[(Int,Int)] = {
    if (rem == 0) {
      return img
    }
    val b = getBounds(img)
    val xs = b.xMin-1 to b.xMax+1
    val ys = b.yMin-1 to b.yMax+1
    val xys = for (x <- xs; y <- ys) yield (x,y)

    val newImg = xys.foldLeft(Set.empty[(Int,Int)]){case (s,pixel) =>
      val mapping = getPixelMapping(img, pixel, b, rem, oob)
      if (algo(mapping)) s.incl(pixel) else s
    }
    enhance(newImg, algo, rem-1, oob)
  }

  def enhanceIterations(img: Set[(Int,Int)], algo: Array[Boolean], iter: Int): Set[(Int,Int)] = {
    //For example problem, OOB checking does not make sense as [000;000;000] is mapped to zero and [111;111;111] is mapped to 1
    //For real problem, [0's] maps to 1 and [1's] maps to 0
    val oob = if (algo.head && !algo.last) Seq(false,true) else Seq(false,false) //Value to use for out-of-bounds elements on (even,odd) rounds
    enhance(img, algo, iter, oob)
  }

  override def solvePart1(inp: List[String]): Any = {
    val algo = parseAlgorithm(inp.head)
    val img = parseImage(inp.drop(2))
    enhanceIterations(img, algo, 2).size
  }

  override def solvePart2(inp: List[String]): Any = {
    val algo = parseAlgorithm(inp.head)
    val img = parseImage(inp.drop(2))
    enhanceIterations(img, algo, 50).size
  }
}

object Day20 extends App {
  Solution.solve2021(new Day20, 20, 35, 3351)
}