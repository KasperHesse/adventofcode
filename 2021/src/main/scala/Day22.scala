import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec

class Day22 extends Solution {

  case class Cuboid(on: Boolean, xMin: Long, xMax: Long, yMin: Long, yMax: Long, zMin: Long, zMax: Long) {
    override def equals(obj: Any): Boolean = obj match { //Equality does not depend on value of "on"
      case that: Cuboid =>
        this.xMin == that.xMin &&
          this.xMax == that.xMax &&
          this.yMin == that.yMin &&
          this.yMax == that.yMax &&
          this.zMin == that.zMin &&
          this.zMax == that.zMax
      case _ => false
    }
  }

  object Cuboid {
    val re = "(\\w+) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r
    def apply(s: String): Cuboid = {
      s match {
        case re(onoff, xmin, xmax, ymin, ymax, zmin, zmax) => {
          val on = onoff == "on"
          val xMin = xmin.toLong
          val xMax = xmax.toLong
          val yMin = ymin.toLong
          val yMax = ymax.toLong
          val zMin = zmin.toLong
          val zMax = zmax.toLong
          Cuboid(on, xMin, xMax, yMin, yMax, zMin, zMax)
        }
      }
    }
  }

  /**
   * Gets the cuboid at the intersection of cuboid cb1 and cb2 if any such exists
   * @param cb1
   * @param cb2
   * @return
   */
  def getIntersectingCuboid(cb1: Cuboid, cb2: Cuboid): Option[Cuboid] = {
    val xMin = Math.max(cb1.xMin, cb2.xMin)
    val xMax = Math.min(cb1.xMax, cb2.xMax)
    val yMin = Math.max(cb1.yMin, cb2.yMin)
    val yMax = Math.min(cb1.yMax, cb2.yMax)
    val zMin = Math.max(cb1.zMin, cb2.zMin)
    val zMax = Math.min(cb1.zMax, cb2.zMax)
    if (xMax >= xMin && yMax >= yMin && zMax >= zMin) {
      Some(Cuboid(cb2.on, xMin, xMax, yMin, yMax, zMin, zMax))
    } else {
      None
    }
  }

  def cuboidsOverlap(cb1: Cuboid, cb2: Cuboid): Boolean = {
    getIntersectingCuboid(cb1,cb2).isDefined
  }

  @tailrec
  final def combineCuboids(cbs: Set[Cuboid]): Set[Cuboid] = {
    //For each cuboid in set: Find a cuboid which shares two coord-sets, combine into a single cuboid
    def canBeCombined(cb1: Cuboid, cb2: Cuboid): Boolean = {
      (cb1.xMin == cb2.xMin && cb1.xMax == cb2.xMax && cb1.yMin == cb2.yMin && cb1.yMax == cb2.yMax && ((cb1.zMin == cb2.zMax + 1) || (cb1.zMax == cb2.zMin - 1))) || //Match on XY-plane
        (cb1.xMin == cb2.xMin && cb1.xMax == cb2.xMax && cb1.zMin == cb2.zMin && cb1.zMax == cb2.zMax && ((cb1.yMin == cb2.yMax + 1) || (cb1.yMax == cb2.yMin - 1))) || //Match on XZ-plane
        (cb1.yMin == cb2.yMin && cb1.yMax == cb2.yMax && cb1.zMin == cb2.zMin && cb1.zMax == cb2.zMax && ((cb1.xMin == cb2.xMax + 1) || (cb1.xMax == cb2.xMin - 1))) //Match on YZ-plane
    }

    //Find a cuboid which can be merged with something
    val cb = cbs.find{cb => cbs.excl(cb).exists(cb2 => canBeCombined(cb,cb2))}
    if (cb.isDefined) {
      val cb1 = cb.get
      //and get that cuboid it could be merged with
      val cb2 = cbs.excl(cb1).find{cb => canBeCombined(cb1, cb) }.get
      val newCuboid = Cuboid(true, cb1.xMin.min(cb2.xMin), cb1.xMax.max(cb2.xMax), cb1.yMin.min(cb2.yMin), cb1.yMax.max(cb2.yMax), cb1.zMin.min(cb2.zMin), cb1.zMax.max(cb2.zMax))
      combineCuboids(cbs.diff(Set(cb1,cb2)).incl(newCuboid))
    } else {
      cbs
    }
  }

  def getSplitCuboids(cb1: Cuboid, cb2: Cuboid): List[Cuboid] = {
    //Filter to only keep tuples where min <= max
    val xs = Seq((cb1.xMin, cb2.xMin-1), (cb2.xMin, cb2.xMax), (cb2.xMax+1, cb1.xMax)).filter(tp => tp._1 <= tp._2)
    val ys = Seq((cb1.yMin, cb2.yMin-1), (cb2.yMin, cb2.yMax), (cb2.yMax+1, cb1.yMax)).filter(tp => tp._1 <= tp._2)
    val zs = Seq((cb1.zMin, cb2.zMin-1), (cb2.zMin, cb2.zMax), (cb2.zMax+1, cb1.zMax)).filter(tp => tp._1 <= tp._2)

    //All legal cuboids are given by crossing these three ranges
    val cuboids = (for (x <- xs; y <- ys; z <- zs) yield Cuboid(true, x._1, x._2, y._1, y._2, z._1, z._2)).toSet
    //Remove the one that matches cb2 before returning
    combineCuboids(cuboids.excl(cb2)).toList
  }

  def volume(cb: Cuboid): Long = {
    (cb.xMax - cb.xMin + 1) * (cb.yMax - cb.yMin + 1) * (cb.zMax - cb.zMin + 1)
  }

  @tailrec
  final def processCuboids(todo: List[Cuboid], domain: Set[Cuboid]): Set[Cuboid] = {
    if (todo.isEmpty) {
      domain
    } else {
      //Check if cuboid to parse matches any cuboid already in domain
      val cb1 = todo.head
      //Could also just use collect to find *all* overlapping cuboids at once
      //Then fold over them and domain
      val cb2 = domain.collectFirst{case cb if cuboidsOverlap(cb1, cb) => cb}
      if (cb2.isDefined) { //Some overlap, must perform some processing
        val is = getIntersectingCuboid(cb1, cb2.get).get
        if (cb1.on) { //ON-rules
          //cb1 is the new cuboid, cb2 is old cuboid
          //if is == cb1 && volume(cb2) > volume(cb1), discard cb1 as it is already completely contained in domain. Process remainder of todo-list
          //if is == cb2 && volume(cb1) > volume(cb2), discard cb2 (contained in cb1) and repeat processing of cb1 to determine further overlaps
          //if is != 0 (always true here), discard cb1 and add all split cuboids to queue
          if (is == cb1 && volume(cb2.get) > volume(cb1)) {
            processCuboids(todo.tail, domain)
          } else if (is == cb2 && volume(cb1) > volume(cb2.get)) {
            processCuboids(todo, domain.excl(cb2.get))
          } else { //At this point, is cannot be non-zero
            //Split the existing cuboid into new cuboids
            val split = getSplitCuboids(cb1, is)
            processCuboids(split ++ todo.tail, domain)
          }
        } else { //OFF-rules
          //cb1 is the new cuboid, cb2 is old cuboid
          //Since we have an intersection, we can delete the part of cb2 that intersects with cb1
          //Remove original cb2, add sub-cuboids from splitting cb2 to domain as they do not intersect with cb1
          val split = getSplitCuboids(cb2.get, is)
          processCuboids(todo, domain.excl(cb2.get).union(split.toSet))
        }
      } else { //No overlap
        if (cb1.on) { //turn ON, add to domain
          processCuboids(todo.tail, domain.incl(cb1))
        } else { //Turn OFF, no more to turn off, just ignore it
          processCuboids(todo.tail, domain)
        }
      }
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val cbs = inp.map(s => Cuboid(s))
    val cbss = cbs.flatMap{cb => //Filter out any indices outside range [-50;50]. using flatMap and option to remove those that are totally outside range
      val cbb = Cuboid(cb.on,
        cb.xMin.max(-50),
        cb.xMax.min(50),
        cb.yMin.max(-50),
        cb.yMax.min(50),
        cb.zMin.max(-50),
        cb.zMax.min(50))
      if (cbb.xMax < cbb.xMin || cbb.yMax < cbb.yMin || cbb.zMax < cbb.zMin) {
        None
      } else {
        Some(cbb)
      }
    }
    val domain = processCuboids(cbss, Set.empty)
    domain.toList.map(volume).sum //Must convert to list before mapping to volume to avoid non-unique volumes being globbed by set-characteristics

  }

  override def solvePart2(inp: List[String]): Any = {
    val cbs = inp.map(s => Cuboid(s))
    val domain = processCuboids(cbs, Set.empty)
    domain.toList.map(volume).sum
  }
}

object Day22 extends App {
  Solution.solve(new Day22, 22, 474140, 2758514936282235L)
}