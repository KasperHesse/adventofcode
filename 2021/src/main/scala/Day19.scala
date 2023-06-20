class Day19 extends Solution {

  case class Point3D(x: Int, y: Int, z: Int) {
    def add(that: Point3D): Point3D = Point3D(this.x + that.x, this.y + that.y, this.z + that.z)
    def neg: Point3D = Point3D(-this.x, -this.y, -this.z)
  }//Beacons have x,y,z coords

  /**
   *
   * @param pos Position of the scaner
   * @param beacons All beacons detected by the scanner, in the scanners coordinate system
   * @param dists Distance from each beacon is the scanners range to all other beacons in range
   */
  case class Scanner(id: Int, pos: Point3D, beacons: List[Point3D], dists: Map[(Point3D, Point3D), Manhattan])

  def manhattan(b0: Point3D, b1: Point3D): Int = {
    (b0.x - b1.x).abs + (b0.y - b1.y).abs + (b0.z - b1.z).abs
  }
  case class Manhattan(d1: Int, d2: Int, d3: Int, tot: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case that: Manhattan => {
        val M1 = Manhattan(this.d1.abs, this.d2.abs, this.d3.abs, this.tot)
        val M2 = Manhattan(that.d1.abs, that.d2.abs, that.d3.abs, that.tot)
        M1.tot == M2.tot && (
          (M1.d1 == M2.d1 && M1.d2 == M2.d2 && M1.d3 == M2.d3) || //abc == abc
            (M1.d1 == M2.d1 && M1.d2 == M2.d3 && M1.d3 == M2.d2) || //abc == acb
            (M1.d1 == M2.d2 && M1.d2 == M2.d1 && M1.d3 == M2.d3) || //abc == bac
            (M1.d1 == M2.d2 && M1.d2 == M2.d3 && M1.d3 == M2.d1) || //abc == bca
            (M1.d1 == M2.d3 && M1.d2 == M2.d2 && M1.d3 == M2.d1) || //abc == cba
            (M1.d1 == M2.d3 && M1.d2 == M2.d1 && M1.d3 == M2.d2) //abc == cab
          )
      }
      case _ => false
    }
  }
  object Manhattan {
    def apply(b0: Point3D, b1: Point3D): Manhattan = Manhattan(b1.x - b0.x, b1.y - b0.y, b1.z - b0.z, manhattan(b0,b1))
  }


  /**
   * Create a mapping between all beacons visible to a scanner
   * @param beacons List of beacon coordinates
   * @return
   */
  def getDistMap(beacons: List[Point3D]): Map[(Point3D,Point3D), Manhattan] = {
    beacons.allPairs(beacons).map { case (b0, b1) => ((b0, b1), Manhattan(b0, b1)) }.toMap
  }

  def parseInput(inp: List[String]): List[Scanner] = {
    def group(inp: List[String], acc: List[List[String]]): List[List[String]] = {
      inp match {
        case Nil => acc.reverse
        case head::tail if head.startsWith("---") => group(tail, acc) //scanner heading, skip
        case head::tail if head == "" => group(tail, List.empty::acc) //empty line, new scanner
        case head::tail => group(tail, (head::acc.head)::acc.tail)
      }
    }

    def toScanner(bcs: List[String], id: Int): Scanner = {
      val beacons = bcs.map(_.split(",").map(_.toInt)).map(a => Point3D(a(0), a(1), a(2)))
      val dists = (for (b0 <- beacons; b1 <- beacons) yield {(b0,b1)}).map{case (b0,b1) => ((b0,b1), Manhattan(b0, b1))}.toMap
      Scanner(id, Point3D(0,0,0), beacons, dists)
    }
    val grouped = group(inp, List(List.empty))
    grouped.zipWithIndex.map(g => toScanner(g._1, g._2))
  }

  def findCommonDistances(scan0: Scanner, scan1: Scanner, b0: Point3D, b1: Point3D): Iterable[Manhattan] = {
    val b0Dists = scan0.dists.collect{case (k,v) if k._1 == b0 => (k,v)}
    val b1Dists = scan1.dists.collect{case (k,v) if k._1 == b1 => (k,v)}

    b0Dists.values.filter(x => b1Dists.values.exists(y => y==x))
  }


  /**
   * Given two manhattan distances that represent the same vector, find the coordinate transformation
   * matrix between their coordinate systems
   * @param mh0 Vector in first coordinate system
   * @param mh1 Vector in second coordinate system
   * @return 2D array representing the transformation matrix
   */
  def findCoordinateTransform(mh0: Manhattan, mh1: Manhattan): Array[Array[Int]] = {
    //Need the same manhattan distance between both nodes
    val X = List(mh0.d1, mh0.d2, mh0.d3)
    val Y = List(mh1.d1, mh1.d2, mh1.d3)
    val T = Array.fill(3)(Array.fill(3)(0))
    for(y <- 0 to 2; x <- 0 to 2) {
      val r = X(x).toFloat/Y(y).toFloat //Must compare as float to avoid accidentally collapsing division down to 1 or -1
      T(x)(y) = if (r == 1.0 || r == -1.0) r.toInt else 0
    }
    T
  }

  /**
   * Attempts to find overlapping beacons between two scanners. If more than 12 overlapping beacons are found,
   * returns those two beacons in s0 and s1-coordinates, as well as a transformation matrix to go from s1-coordinates
   * to s1-coordinates
   * @param s0
   * @param s1
   * @return
   */
  def getOverlappingBeaconsAndDists(s0: Scanner, s1: Scanner): Option[(Point3D, Point3D, Array[Array[Int]])] = {
    for ((b0,b1) <- s0.beacons.allPairs(s1.beacons)) {
      val intersect = findCommonDistances(s0, s1, b0, b1)
      if (intersect.size >= 12) {
        //mh is some manhattan distance that is shared in the intersection set.
        //Used to find the corresponding manhattan distances in both beacons lists, which is used to generate the transform matrix
        val mh = if (intersect.head.tot != 0) intersect.head else intersect.drop(1).head //To avoid division by zero
        val X = s0.dists.collectFirst{case (k,v) if k._1 == b0 && v.tot == mh.tot => v}.get
        val Y = s1.dists.collectFirst{case (k,v) if k._1 == b1 && v.tot == mh.tot => v}.get
        return Some((b0, b1, findCoordinateTransform(X,Y)))
      }
    }
    None
  }
  /**
   * Apply the coordnate transform matrix T to a set of points/vectors
   * @param T Coordinate transform matrix
   * @param points The points to transform using the matrix
   * @return Mapping from original coordinates to new coordinate system
   */
  def applyCoordTransform(T: Array[Array[Int]], points: List[Point3D]): List[Point3D] = {
    points.map{p =>
      Point3D(
        p.x * T(0)(0) + p.y * T(0)(1) + p.z * T(0)(2),
        p.x * T(1)(0) + p.y * T(1)(1) + p.z * T(1)(2),
        p.x * T(2)(0) + p.y * T(2)(1) + p.z * T(2)(2)
      )
    }
  }

  def alignScanners(scanners: List[Scanner]): List[Scanner] = {
    def processScanners(tested: Set[Scanner], untested: Set[Scanner], unmapped: Set[Scanner]): List[Scanner] = {
      if (unmapped.isEmpty) {
        tested.union(untested).toList
      } else {
        println(s"${unmapped.size} remaining")
        val s0 = untested.head
        //Find all scanners in range
        val allInRange = unmapped.collect {
          case um => getOverlappingBeaconsAndDists(s0, um).map { x => (um, x) }
        }.flatten //Using .flatten should remove all None's
        val remapped = allInRange.map{case (scan, (b0,b1,trans)) =>
          val ptr = applyCoordTransform(trans, List(b1)).head.neg
          val newLoc = b0.add(ptr)
          val beaconsMapped = applyCoordTransform(trans, scan.beacons).map(_.add(newLoc))
          val newDists = getDistMap(beaconsMapped)
          Scanner(scan.id, newLoc, beaconsMapped, newDists)
        }
        processScanners(tested.incl(s0), untested.excl(s0).union(remapped), unmapped.diff(allInRange.map(_._1)))
      }
    }
    val mapped = Set(scanners.head)
    val unmapped = scanners.tail.toSet
    processScanners(Set.empty, mapped, unmapped)
  }

  override def solvePart1(inp: List[String]): Any = {
    val scanners = parseInput(inp)
    val postMapping = alignScanners(scanners)
    val reducedBeacons = postMapping.flatMap{s => s.beacons}.toSet
    reducedBeacons.size
  }

  override def solvePart2(inp: List[String]): Any = {
    val scanners = parseInput(inp)
    val postMapping = alignScanners(scanners)

    val poss = postMapping.map(_.pos)
    val z = poss.allPairs(poss).map(p => manhattan(p._1, p._2)).max
    z
  }
}

object Day19 extends App {
  Solution.solve(new Day19, 19, 79, 3621)
}