package common

import scala.annotation.tailrec

object GraphOps {
  /**
   * Contract a graph, removing all intermediate nodes that don't contribute anything to the graph.
   * Replaces sections of intermediate nodes (e.g A->B->C->D) with edges of equal cost (e.g. A->D, length 3)
   * @param graph The graph to contract
   * @tparam T
   * @return The corresponding graph, but with all intermediate edges contracted
   */
  def contract[T](graph: Graph[T]): Graph[T] = {
    /*
    To contract the graph, must find all nodes that are potential branching points.
    If graph is directed, some may not be found (AoC 2023, day 23). To resolve  this, we first
    generate the undirected version of the graph to detect all potential branch points. Then,
    these are used as the starting points for contracting paths in the input graph
     */
    val unDir = UndirectedGraph(graph.E)
    val spots = unDir.E.collect{case (u,nbs) if nbs.size != 2 => u}.toSet

    def contractPath(v: T, prev: T, len: Long): (T, Long) = {
      //Check for size != 1 is required if a backward edge in a pseudo-dag is attempted (AoC 2023 day 23)
      //Doesn't spoil anything
      if(spots.contains(v) || graph.nbs(v).excl(prev).size != 1) {
        (v, len)
      } else {
        val next = graph.nbs(v).excl(prev).head
        contractPath(next, v, graph.cost(v, next) + len)
      }
    }
    //For each spot, contract outgoing edges in the original graph
    spots.foldLeft(Graph[T](Map.empty, Map.empty)){case (g,u) =>
      val newEdges = graph.nbs(u).map(nb => contractPath(nb, u, graph.cost(u, nb)))
        .filter(e => spots.contains(e._1)) //Filter required to remove "fake" backwards edges in directed graphs
      newEdges.foldLeft(g){case (g, (v,len)) => g.addEdge(u, v, len)}
    }
  }

  /**
   * Find the longest path between two nodes in a given graph.
   * Does this with an exhaustive DFS-search of the graph's space
   * @param G The graph to search
   * @param src The source node to search from
   * @param goal The goal node to reach
   * @tparam T
   * @return The length of the longest path between `src` and `goal`
   */
  def longestPath[T](G: Graph[T], src: T, goal: T): Long = {
    @tailrec
    def DFS(queue: Queue[(List[T], Long)], longest: Long): Long = {
      val (p,q) = queue.deq()
      p match {
        case None => longest
        case Some((path, len)) if path.head == goal => DFS(q, longest.max(len))
        case Some((path, len)) =>
          val newPaths = G.nbs(path.head).filterNot(path.contains).map(_ :: path).map(np => (np, G.cost(path.head, np.head) + len))
          //Somewhat simple pruning: If any neighbour is goal, just add that neighbour's path, don't check other neighbours
          newPaths.find(_._1.head == goal) match {
            case Some(p) => DFS(q.enq(p), longest)
            case None => DFS(q.enqAll(newPaths), longest)
          }
      }
    }
    DFS(Queue((List(src),0)), 0)
  }
}
