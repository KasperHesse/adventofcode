package y2023

import common._

import scala.annotation.tailrec

class Day16 extends Solution {
  case class Beam(loc: Vec2D, dir: Vec2D)

  @tailrec
  final def transmitBeam(beams: Queue[Beam], visited: Set[Beam], map: Map[Vec2D, Char], bounds: (Int,Int,Int,Int)): Set[Beam] = {
    val (b, q) = beams.deq()
    b match {
      case None => visited
      case Some(beam) if visited.contains(beam) => transmitBeam(q, visited, map, bounds)
      case Some(beam) =>
      //Propagate in the direction
        val newBeams = ((map(beam.loc), beam.dir) match {
          //Continue undisturbed
          case ('.',_) | ('-', Vec2D(_,0)) | ('|', Vec2D(0,_)) => List(Beam(beam.dir + beam.loc, beam.dir))
          //Going left/right, split up/down
          case ('|',_) => List(Beam(beam.loc + Vec2D(0,-1), Vec2D(0,-1)), Beam(beam.loc + Vec2D(0, 1), Vec2D(0, 1)))
          //going up/down, split left/right
          case ('-',_) => List(Beam(beam.loc + Vec2D(-1,0), Vec2D(-1,0)), Beam(beam.loc + Vec2D(1, 0), Vec2D(1, 0)))
          //Going left/right and hitting '\' or up/down and hitting '/', rotate right
          case ('\\', Vec2D(_,0)) | ('/', Vec2D(0,_)) => List(Beam(beam.loc + beam.dir.rotRight, beam.dir.rotRight))
          //Going left/right and hitting '/' or up/down and hitting '\', rotate left
          case ('/', Vec2D(_,0)) | ('\\', Vec2D(0,_)) => List(Beam(beam.loc + beam.dir.rotLeft, beam.dir.rotLeft))
        }).filter{case Beam(Vec2D(x,y),_) => (bounds._1 <= x && x <= bounds._2) && (bounds._3 <= y && y <= bounds._4)}
        //Enqueue all new beams,
        val q2 = newBeams.foldLeft(q){case (queue, beam) => queue.enq(beam)}
        transmitBeam(q2, visited.incl(beam), map, bounds)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    //Need memoization of beam location,directions. If already encountered, can stop there. Otherwise will hit infinite loops
    //For Vec2D points to work correctly, must "mirror" everything in X-axis
    //Hence, must flip all '/' and '\' symbols to work correctly. Probably more trouble than it's worth
    val m = Solution.parseAsMap(inp)
    val map = m.map{
      case ((x,y),'/') => (Vec2D(x,y), '\\')
      case ((x,y),'\\') => (Vec2D(x,y), '/')
      case ((x,y),c) => (Vec2D(x,y),c)}
    val bounds = Solution.getRectMapBounds(m)
    val queue = Queue(Beam(Vec2D(0,0), Vec2D(1,0)))
    val res = transmitBeam(queue, Set.empty, map, bounds).map(_.loc)
    res.size
  }

  override def solvePart2(inp: List[String]): Any = {
    //Can probably just be brute-forced
    val m = Solution.parseAsMap(inp)
    val map = m.map{
      case ((x,y),'/') => (Vec2D(x,y), '\\')
      case ((x,y),'\\') => (Vec2D(x,y), '/')
      case ((x,y),c) => (Vec2D(x,y),c)}
    val bounds = Solution.getRectMapBounds(m)
    //Create all initial locations
    val initLocs = Seq(
      Seq.tabulate(inp.head.length)(x => Beam(Vec2D(x,0), Vec2D(0,1))),
      Seq.tabulate(inp.head.length)(x => Beam(Vec2D(x, inp.head.length-1), Vec2D(0, -1))),
      Seq.tabulate(inp.length)(y => Beam(Vec2D(0, y), Vec2D(1,0))),
      Seq.tabulate(inp.length)(y => Beam(Vec2D(inp.length - 1, y), Vec2D(-1,0)))
    ).flatten

    //Find the best one
    initLocs.map{l =>
      val queue = Queue(l)
      transmitBeam(queue, Set.empty, map, bounds).map(_.loc).size
    }.max
  }
}

object Day16 extends App {
  Solution.solve(new Day16, 16, 2023, 46, 51)
}
