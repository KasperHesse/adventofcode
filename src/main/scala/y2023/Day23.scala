package y2023

import common._

import scala.annotation.tailrec

class Day23 extends Solution {

  case class NodeState(parents: Set[Vec2D], children: Set[Vec2D])

  /**
   * Convert the input 2D graph to a graph where only legal edges are present
   * @param map The input 2D map
   * @param nbGraph The possible neighbours for each field in the input map
   * @return A mapping for each legal Vec2D to all legal neighbours that it can access
   */
  def convertGraph(map: Map[(Int, Int), Char], nbGraph: Map[(Int, Int), List[(Int,Int)]]): Map[Vec2D, Set[Vec2D]] = {
    map.collect{case ((x,y),v) if v != '#' =>
      val nbs = v match {
        case '.' => nbGraph((x,y)).filter(nb => (nb, map(nb)) match { //When '.', check adjacent neighbours to see if edge is legal
          case ((x2,y2),c) => c == '.' || //if dot, always legal
            ((y == y2 && ((x2 == x + 1 && c == '>') || (x2 == x - 1 && c == '<'))) || //if same y, must be valid arrow right/left
              (x == x2 && ((y2 == y - 1 && c == '^') || (y2 == y + 1 && c == 'v')))) //if same x, must be valid arrow up/down
        }).toSet
        case '>' => Set((x+1,y)) //Arrows are assumed to always point to something valid and not a wall
        case '<' => Set((x-1, y))
        case 'v' => Set((x, y+1))
        case '^' => Set((x, y-1))
      }
      (Vec2D(x,y), nbs.map(xy => Vec2D(xy._1, xy._2)))
    }
  }

  /**
   * Convert the input 2D graph to graph where slopes are not unidirectional
   * @param map
   * @param nbGraph
   * @return
   */
  def convertGraphNoSlopes(map: Map[(Int, Int), Char], nbGraph: Map[(Int, Int), List[(Int,Int)]]): Map[Vec2D, Set[Vec2D]] = {
    map.collect{case ((x,y),v) if v != '#' =>
      (Vec2D(x,y),nbGraph((x,y)).collect{case nb if map(nb) != '#' => Vec2D(nb._1, nb._2)}.toSet)
    }
  }

  /**
   * Condense the input graph, replacing simple edges on non-branching paths with weighted edges.
   * This drastically reduces the number of edges in the graph
   * @param queue Queue of vertices to consider for condensing
   * @param graph The original, uncondensed input graph
   * @param newGraph The new graph being built
   * @return
   */
  def condenseGraph(queue: Queue[Vec2D], graph: Map[Vec2D, Set[Vec2D]], newGraph: Map[Vec2D, Set[(Vec2D, Long)]]): Map[Vec2D, Set[(Vec2D, Long)]] = {
    //Used to condense a path, computing the length of that path
    @tailrec
    def condensePath(v: Vec2D, prev: Vec2D, len: Long): (Vec2D, Long) = {
      if (graph(v).size != 2) {
        (v, len)
      } else {
        val next = graph(v).excl(prev).head //extract the neighbour that is not previous node
        condensePath(next, v, len + 1)
      }
    }
    val (v,q) = queue.deq()
    v match {
      case None => newGraph
      case Some(v) =>
        //Only find neighbours that aren't already present in map
        val nbs = graph(v)
        //Condense all edges, then filter out the ones that already exist in graph
        val edges = nbs.map(nb => condensePath(nb, v, 1)).filter{case (nb, len) => !newGraph.exists(e => e._1 == v && e._2.contains((nb,len)))}
        val ng2 = edges.foldLeft(newGraph){case (ng, (u,len)) => ng.updatedWith(v){
          case Some(s) => Some(s.incl((u, len))) //Add edge (v -> u)
          case None => Some(Set((u, len)))
        }.updatedWith(u){
          case Some(s) => Some(s.incl((v, len))) //Add edge (u -> v)
          case None => Some(Set((v, len)))
        }}
        //Enqueue all endpoints not seen already
        val q2 = q.enqAll(edges.map(_._1).toList)
        condenseGraph(q2, graph, ng2)
    }
  }

  @tailrec
  final def makeFullyDirected(queue: Queue[Vec2D], graph: Map[Vec2D, Set[Vec2D]], dag: Map[Vec2D, NodeState]): Map[Vec2D, NodeState] = {
    val (v,q) = queue.deq()
    v match {
      case None => dag
      case Some(v) =>
        val children = graph(v).diff(dag(v).parents) //Children are all neighbours not set as this parents
        val notVisited = children.filterNot(dag.contains) //Children not yet encountered must be enqueued later
        val newDag = children.foldLeft(dag){case (dag,c) => dag.updatedWith(c){
          case None => Some(NodeState(Set(v), Set.empty)) //If child not yet in DAG, create and add v as only parent
          case Some(x) => Some(NodeState(x.parents.incl(v), x.children)) //If child already in DAG, add v to list of parents
        }}.updatedWith(v){case Some(x) => Some(NodeState(x.parents, children))} //Update mapping for v, adding all children

        makeFullyDirected(q.enqAll(notVisited.toList), graph, newDag)
    }
  }

  @tailrec
  final def constructAllPaths(queue: Queue[List[Vec2D]], valid: Set[List[Vec2D]], graph: Map[Vec2D, Set[Vec2D]], goal: Vec2D): Set[List[Vec2D]] = {
    val (p, q) = queue.deq()
    p match {
      case None => valid
      case Some(path) if path.head == goal => constructAllPaths(q, valid.incl(path), graph, goal)
      case Some(path) =>
        val newPaths = graph(path.head).filterNot(path.contains).map(_::path).toList
        //Somewhat simple pruning: If any neighbour is goal, just add that neighbour, don't check other neighbours
        newPaths.find(_.head == goal) match {
          case None => constructAllPaths(q.enqAll(newPaths), valid, graph, goal)
          case Some(path2) => constructAllPaths(q, valid.incl(path2), graph, goal)
        }
    }
  }


  @tailrec
  final def findLongestPath(queue: Queue[Vec2D], dag: Map[Vec2D, NodeState], longest: Map[Vec2D, Long]): Map[Vec2D, Long] = {
    def getLongest(longest: Map[Vec2D, Long], v: Vec2D): Map[Vec2D, Long] = {
      if (longest.contains(v)) {
        longest
      } else {
        val upd = dag(v).parents.foldLeft(longest)(getLongest)
        upd.updated(v, dag(v).parents.map(upd).max + 1)
      }
    }
    val (v,q) = queue.deq()
    v match {
      case None => longest
      case Some(v) =>
        val l2 = getLongest(longest, v)
        val toVisit = dag(v).children.filterNot(longest.contains).toList
        findLongestPath(q.enqAll(toVisit), dag, l2)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    /*
    Need to parse into graph. match letters
    # => no neighbours
    . => all neighbours that are not #
    <v>^ => only neighbour in that direction

    Once parsed, perform a BFS-like search of the space, memoizing longest distance to any node
    Always update longest distance to a node when encountered, then get distance to node in bottom-left corner
     */
    val initGraph = Solution.parseAsMap(inp)
    val bounds = Solution.getRectMapBounds(initGraph)
    val nbs = Solution.gen2DMap(bounds._1, bounds._2, bounds._3, bounds._4)
    val graph = convertGraph(initGraph, nbs)
    val topLeft = Vec2D(1, 0)
    val botRight = Vec2D(bounds._2 -1, bounds._4) //maxX - 1, maxY

    val dag = makeFullyDirected(Queue(topLeft), graph, Map(topLeft -> NodeState(Set.empty, Set.empty)))
    val longest = findLongestPath(Queue(topLeft), dag, Map(topLeft -> 0))
    longest(botRight)
  }

  override def solvePart2(inp: List[String]): Any = {
    val initGraph = Solution.parseAsMap(inp)
    val bounds = Solution.getRectMapBounds(initGraph)
    val nbs = Solution.gen2DMap(bounds._1, bounds._2, bounds._3, bounds._4)
    val graph = convertGraphNoSlopes(initGraph, nbs)
    val topLeft = Vec2D(1, 0)
    val botRight = Vec2D(bounds._2 -1, bounds._4) //maxX - 1, maxY

    val condensed = condenseGraph(Queue(topLeft), graph, Map.empty)
    val edgeMap = condensed.foldLeft(Map.empty[(Vec2D, Vec2D), Long]){case (m, (v, s)) =>
      s.foldLeft(m){case (m, (u,len)) => m.updated((u,v), len)}
    }
    val paths = constructAllPaths(Queue(List(topLeft)), Set.empty, condensed.map{case (k,v) => (k, v.map(_._1))}, botRight)
    paths.map(_.pairwise.foldLeft(0L){case (acc, uv) => acc + edgeMap(uv)}).max
  }
}

object Day23 extends App {
  Solution.solve(new Day23, 23, 2023, 94, 154)
}
