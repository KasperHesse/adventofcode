package y2023

import common._

import scala.annotation.tailrec

class Day22 extends Solution {
  case class Cuboid(x: Range, y: Range, z: Range) {
    def intersects(that: Cuboid): Boolean = this.x.intersects(that.x) && this.y.intersects(that.y) && this.z.intersects(that.z)
    lazy val layerAbove: Cuboid = Cuboid(this.x, this.y, Range(this.z.end + 1, this.z.end + 1))
    lazy val layerBelow: Cuboid = Cuboid(this.x, this.y, Range(this.z.start - 1, this.z.start - 1))
  }

  def canMoveDown(cuboid: Cuboid, cuboids: List[Cuboid]): Boolean = {
    val oneDown = Cuboid(cuboid.x, cuboid.y, Range(cuboid.z.start - 1, cuboid.z.end-1))
    oneDown.z.start > 0 && !cuboids.exists(c => c.intersects(oneDown))
  }

  @tailrec //TODO Consider doing binary search of movedown-values to speed it up
  final def moveDown(cuboids: List[Cuboid], cuboid: Cuboid): List[Cuboid] = {
    if (!canMoveDown(cuboid, cuboids)) {
      cuboid::cuboids
    } else {
      moveDown(cuboids, Cuboid(cuboid.x, cuboid.y, Range(cuboid.z.start - 1, cuboid.z.end-1)))
    }
  }

  def parseCuboid(line: String): Cuboid = line match {
    case s"$x1,$y1,$z1~$x2,$y2,$z2" => Cuboid(Range(x1.toLong,x2.toLong), Range(y1.toLong,y2.toLong), Range(z1.toLong,z2.toLong))
  }

  override def solvePart1(inp: List[String]): Any = {
    val init = inp.map(parseCuboid).sortBy(_.z.start) //sort them by starting z-coordinate
    val settled = init.foldLeft(List.empty[Cuboid])(moveDown)
    val cannotBeDestroyed = settled.foldLeft(Set.empty[Cuboid]){case (set, c) =>
      val supports = settled.filter(c2 => c2.intersects(c.layerBelow))
      if (supports.size == 1) { //cuboid c is only supported by one other cuboid. That support cannot be destroyed
        set.incl(supports.head)
      } else {
        set
      }
    }
    settled.size - cannotBeDestroyed.size

  }

  //Find the set of all bricks above any other brick
  def findBricksAbove(c: Cuboid, memo: Map[Cuboid, Set[Cuboid]], cuboids: List[Cuboid]): Map[Cuboid, Set[Cuboid]] = {
    if (memo.contains(c)) { //Already processed, don't parse again
      memo
    } else {
      //if all supports have been removed: Return removal-count
      //if all supports have not been removed: Return 0
      //Process graph to get memo, then use memo afterwards
      val supporting = cuboids.filter(c2 => c2.intersects(c.layerAbove)).toSet
      //Process all nodes above
      val memo2 = supporting.foldLeft(memo){case (m,c) => findBricksAbove(c, m, cuboids)}
      val memo3 = memo2.updated(c, supporting.union(supporting.flatMap(s => memo2(s))))
      memo3
    }
  }

  //Use some kind of memo as well?
  @tailrec
  final def findNumBricksDropped(queue: Queue[Cuboid], dropped: Set[Cuboid], cuboids: List[Cuboid]): Int = {
    val (c,q) = queue.deq()
    c match {
      case None => dropped.size - 1 //Subtracting 1 to not drop the original brick we destroyed
      case Some(c) =>
        val supportedBy = cuboids.filter(c2 => c2.intersects(c.layerBelow)).toSet
        if (supportedBy.diff(dropped).isEmpty) { //All supports have been removed, this one will drop as well
          val supporting = cuboids.filter(c2 => c2.intersects(c.layerAbove))
          val q2 = q.enqAll(supporting)
          findNumBricksDropped(q2, dropped.incl(c), cuboids)
        } else { //Not all supports have dropped. Empty queue before returning
          findNumBricksDropped(q, dropped, cuboids)
        }
    }
  }

  override def solvePart2(inp: List[String]): Any = {
    val init = inp.map(parseCuboid).sortBy(_.z.start) //sort them by starting z-coordinate
    val settled = init.foldLeft(List.empty[Cuboid])(moveDown)
    val bc = settled.foldLeft(Map.empty[Cuboid,Set[Cuboid]]){case (m,c) => findBricksAbove(c, m, settled)}

    val loneSupports = settled.foldLeft(Set.empty[Cuboid]){ case (s, c) =>
      val supportedBy = settled.filter(c2 => c2.intersects(c.layerBelow)).toSet
      if (supportedBy.size == 1) {
        s.union(supportedBy)
      } else {
        s
      }
    }

    val droppedByLoneSupports = loneSupports.map{ls =>
      val supporting = settled.filter(c2 => c2.intersects(ls.layerAbove))
      val q = Queue(supporting.head).enqAll(supporting.tail)
      val dropped = Set(ls)
      findNumBricksDropped(q, dropped, settled)
    }

    val bot = settled.filter(_.z.start == 1)
    loneSupports.foreach{b =>
      println(s"lone support $b, supporting ${bc(b).size} nodes")
    }
    println(loneSupports.map(b => bc(b).size).sum)
    droppedByLoneSupports.sum

//    loneSupports.map(ls => bc(ls).size).sum

    //1167 is too low, 89024 is too high, 65941 is not correct either
  }
}

object Day22 extends App {
  Solution.solve(new Day22, 22, 2023, 5, 7)
}
