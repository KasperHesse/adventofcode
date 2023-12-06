package y2023

import common.Solution
import common._

class Day5 extends Solution {

  case class Map(src: Range, dest: Range)

  /**
   * Parse the input lines and return a (src,dest) Map for each line
   * @param inp
   * @return
   */
  def parseMap(inp: List[String]): List[Map] = {
    inp.map{x =>
      val s = x.split(" ").map(_.toLong)
      val src = Range(s(1), s(1) + s(2) - 1, s(2))
      val dest = Range(s(0), s(0) + s(2) - 1, s(2))
      Map(src, dest)
    }
  }

  /**
   * Convert the ranges from one mapping to another
   * @param ins The input ranges
   * @param maps The maps to use
   * @param outs The output ranges generated
   * @return
   */
  def convertRanges(ins: List[Range], maps: List[Map], outs: List[Range]): List[Range] = {
    /**
     * Convert a single range
     * @param seed The input range
     * @param maps The maps to use
     * @return (Converted range, optional list of leftover ranges that need to be converted)
     */
    def convertRange(seed: Range, maps: List[Map]): (Range, Option[List[Range]]) = {
      val mOpt = maps.find{m =>
        seed.intersects(m.src)
      }
      mOpt match {
        //If no mapping is found, we just transfer the values in the range directly to the next in line
        case None => (seed, None)
        case Some(m) => {
          //If a mapping is found, we convert some (or all) or the Range into new coordinates
          val (r0, r1, r2) = seed.partition(m.src)
          val r1Mapped = Range(
            m.dest.start + (r1.get.start - m.src.start),
            m.dest.start + (r1.get.start - m.src.start) + r1.get.len - 1,
            r1.get.len
          )
          val extra = List(r0, r2).flatten
          (r1Mapped, if (extra.nonEmpty) Some(extra) else None)
        }
      }
    }

    ins match {
      case Nil => outs
      case in::tail => val (out, extra) = convertRange(in, maps)
                       if (extra.isDefined) { //There were some leftover ranges
                         convertRanges(List.concat(extra.get, tail), maps, out::outs)
                       } else {
                         convertRanges(tail, maps, out::outs)
                       }
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val s = Solution.splitOnBlankLines(inp)
    //Seeds are on first line, starting after first space
    val seeds = s(0)(0).substring(s(0)(0).indexOf(' ') + 1).split(" ").map(_.toLong).toList
    val maps = s.tail.map(x => parseMap(x.tail))

    //Parse seed values as seed ranges instead. First problem, ranges are length 1
    val newSeeds = seeds.map(x => Range(x, x, 1))

    //Pass through all maps, then find min-value of ranges. This is min-location
    maps.foldLeft(newSeeds){(s,m) => convertRanges(s, m, List.empty)}
      .map(_.start)
      .min
  }

  override def solvePart2(inp: List[String]): Any = {
    val s = Solution.splitOnBlankLines(inp)
    //Seeds are on first line, starting after first space
    val seeds = s(0)(0).substring(s(0)(0).indexOf(' ') + 1).split(" ").map(_.toLong).toList
    val maps = s.tail.map(x => parseMap(x.tail))

    //Parse seed values as seed ranges
    val newSeeds = seeds.grouped(2).map(l => Range(l.head, l.head + l.last - 1, l.last)).toList

    //Pass through all maps, then find min-value of ranges. This is min-location
    maps.foldLeft(newSeeds){(s,m) => convertRanges(s, m, List.empty)}
      .map(_.start)
      .min
  }
}

object Day5 extends App {
  Solution.solve(new Day5, 5, 2023, 35, 46)
}