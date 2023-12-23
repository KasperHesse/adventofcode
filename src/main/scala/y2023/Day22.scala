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

  /**
   * Create a graph of the dropped cuboids. Each node in the graph points to the bricks resting on top of it
   * @param cuboids The remaining cuboids to process
   * @param all List of all cuboids to find the ones located above
   * @param graph Graph being built / accumulating parameter
   * @return Mapping (cuboid -> all cuboids resting on top of it)
   */
  @tailrec
  final def createGraph(cuboids: List[Cuboid], all: List[Cuboid], graph: Map[Cuboid, List[Cuboid]]): Map[Cuboid, List[Cuboid]] = {
    cuboids match {
      case Nil => graph
      case c::tail =>
        val supporting = all.filter(c2 => c2.intersects(c.layerAbove))
        val g2 = graph.updated(c, supporting)
        createGraph(tail, all, g2)
    }
  }

  /**
   * Create a "degree graph", tracking the in-degree of each cuboid. When a cuboid's in-degree is 0, it is no
   * longer supported by any other cuboid
   * @param graph The graph created by [[createGraph]]
   * @return Mapping (cuboid -> number of cuboids supporting it)
   */
  def createDegGraph(graph: Map[Cuboid, List[Cuboid]]): Map[Cuboid, Int] = {
    graph.foldLeft(Map.empty[Cuboid, Int]){case (dg, (c,cs)) =>
      cs.foldLeft(dg){case (dg, c2) => dg.updatedWith(c2){
        case None => Some(1)
        case Some(i) => Some(i+1)
      }}
    }
  }

  /**
   * Destroy a cuboid and track which other cuboids drop as a result
   * @param queue Queue of cuboids to consider for dropping
   * @param dropped Number of dropped cuboids
   * @param graph The graph mapping (cuboid -> cuboids on top of it)
   * @param degGraph Degree graph being updated, mapping (cuboid -> number of cuboids supporting it)
   * @return Number of cuboids dropped in this process
   */
  @tailrec
  final def destroyAndDrop(queue: Queue[Cuboid], dropped: Int, graph: Map[Cuboid, List[Cuboid]], degGraph: Map[Cuboid, Int]): Int = {
    val (c,q) = queue.deq()
    c match {
      case None => dropped - 1
      case Some(c) =>
        //Reduce in-degree of all vertices pointed to by this vertex
        val dg2 = graph(c).foldLeft(degGraph){case (dg,c2) => dg.updatedWith(c2){case Some(i) => Some(i-1)}}
        //Enqueue all we are pointing to that now have in-degree 0
        val q2 = q.enqAll(graph(c).filter(c2 => dg2(c2) == 0))
        destroyAndDrop(q2, dropped + 1, graph, dg2)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val init = inp.map(parseCuboid).sortBy(_.z.start) //sort them by starting z-coordinate to easily drop
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


  override def solvePart2(inp: List[String]): Any = {
    val init = inp.map(parseCuboid).sortBy(_.z.start) //sort them by starting z-coordinate
    val settled = init.foldLeft(List.empty[Cuboid])(moveDown) //drop down
    val graph = createGraph(settled, settled, Map.empty) //create graph
    val degGraph = createDegGraph(graph) //create in-degree graph

    //For each cuboid, sum up the number of other cuboids that will be dropped if it is destroyed
    settled.map(s => destroyAndDrop(Queue(s),0, graph, degGraph)).sum
  }
}

object Day22 extends App {
  Solution.solve(new Day22, 22, 2023, 5, 7)
}
