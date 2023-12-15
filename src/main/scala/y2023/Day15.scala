package y2023

import common._

import scala.annotation.tailrec

class Day15 extends Solution {

  case class Lens(label: String, focalLength: Int)

  def HASH(in: String): Int = {
    in.foldLeft(0){case (acc, c) => ((acc + c)*17) % 256}
  }

  @tailrec
  final def HASHMAP(ops: List[String], m: Map[Int, List[Lens]]): Map[Int, List[Lens]] = {
    ops match {
      case Nil => m
      case op::tail =>
        val newMap = if (op.last == '-') { //Remove-operation
          val lbl = op.dropRight(1)
          val hash = HASH(lbl)
          m.updatedWith(hash){case Some(v) => Some(v.filter(_.label != lbl))}
        } else { //Insert operation
          val lbl = op.dropRight(2)
          val hash = HASH(lbl)
          val fl = op.last.asDigit
          m.updatedWith(hash){ case Some(v) =>
            val idx = v.indexWhere(_.label == lbl)
            if (idx >= 0) { //Already mapped. Update that mapping
              Some(v.updated(idx, Lens(lbl, fl)))
            } else { //Not mapped, add to list
              Some(Lens(lbl, fl)::v)
            }
          }
        }
        HASHMAP(tail, newMap)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val blocks = inp.head.split(",").toList
    blocks.map(HASH).sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val ops = inp.head.split(",").toList
    val initMap = Seq.tabulate(256)(n => (n,List.empty[Lens])).toMap
    //Go through each entry and reverse the list such that newest items are in the back instead of in front
    val hm = HASHMAP(ops, initMap).map{case (i,v) => (i, v.reverse)}
    hm.foldLeft(0){case (acc, (box,l)) =>
      l.zipWithIndex.foldLeft(acc){case (acc, (Lens(_,fl),idx)) => acc + (1 + box) * (1 + idx) * fl}
    }
  }
}

object Day15 extends App {
  Solution.solve(new Day15, 15, 2023, 1320, 145)
}
