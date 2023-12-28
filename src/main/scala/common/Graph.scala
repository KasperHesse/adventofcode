package common

/**
 * An abstract class for representing graphs, allowing for just the features we need.
 * Can have directed and undirected paths, weighted and unweighted edges
 * @tparam T
 */
case class Graph[T](private val map: Map[T, Set[T]],
                    private val weights: Map[(T, T), Long]) {

  /**
   * The set of all vertices in the graph
   */
  lazy val V: Set[T] = map.keySet

  /**
   * The adjacency list mapping of the graph
   */
  val E: Map[T, Set[T]] = map

  /**
   * Get all neighbours for a given node in the graph
   * @param v The node to lookup
   * @return The set of all neighbours for that node
   */
  def nbs(v: T): Set[T] = map(v)

  /**
   * Get all neighbours and edge weights for a given node in the graph
   * @param v The node to lookup
   * @return The set of all neighbours for that node, and each cost of the edge to that neighbours
   */
  def nbsW(v: T): Set[(T, Long)] = {
    map(v).map(u => (u, weights.getOrElse((v, u), 1L)))
  }

  /**
   * Gets the cost of the edge (u,v) in the graph
   * @param u The source node of the edge
   * @param v The destination node of the edge
   * @return The cost of the edge, or 1L if no edge weight is present
   */
  def cost(u: T, v: T): Long = {
    weights.getOrElse((u,v), 1L)
  }

  /**
   * Add an edge with unit edge weight to the graph
   * @param u The source node for the edge
   * @param v The destination node for the edge
   * @return A new graph with that edge added
   */
  def addEdge(u: T, v: T): Graph[T] = {
    addEdge(u, v, 1L)
  }

  /**
   * Add an edge with integer edge weight to the graph
   * @param u The source node for the edge
   * @param v The destination node for the edge
   * @param cost The cost of the edge
   * @return A new graph that that edge added
   */
  def addEdge(u: T, v: T, cost: Long): Graph[T] = {
    val newMap = this.map.updatedWith(u){
      case Some(nbs) => Some(nbs.incl(v))
      case None => Some(Set(v))
    }
    val newWeights = this.weights.updated((u,v), cost)
    Graph(newMap, newWeights)
  }

  /**
   * Get a contracted version of this graph, replacing all non-branching paths with single
   * edges, weighted with the total weight of the path. See [[GraphOps.contract]]
   * @return
   */
  def contracted: Graph[T] = GraphOps.contract(this)
}

object UndirectedGraph {
  /**
   * Create an undirected graph with unit edge weights
   * @param map Mapping (node -> neighbouring nodes)
   * @tparam T
   * @return
   */
  def apply[T](map: Map[T, Set[T]]): Graph[T] = {
    apply(map, Map.empty)
  }

  /**
   * Create an undirected graph with integer edge weights
   * @param map Mapping (node -> neighbouring nodes)
   * @param weights Mapping (u,v -> cost of edge (u,v))
   * @tparam T
   * @return
   */
  def apply[T](map: Map[T, Set[T]], weights: Map[(T,T), Long]): Graph[T] = {
    val bidirMap = map.foldLeft(Map.empty[T, Set[T]]){case (m, (v, nbs)) =>
      nbs.foldLeft(m){case (m, nb) => m.updatedWith(v) {
        case Some(nbs) => Some(nbs.incl(nb))
        case None => Some(Set(nb))
      }.updatedWith(nb) {
        case Some(nbs) => Some(nbs.incl(v))
        case None => Some(Set(v))
      }}
    }
    val bidirWeights = weights.flatMap{case ((u,v),len) => Set(((u,v),len), ((v,u),len))}
    Graph(bidirMap, bidirWeights)
  }
}

object DirectedGraph {
  /**
   * Create a directed graph with unit edge weights
   * @param map Mapping (node -> neighbouring nodes)
   * @tparam T
   * @return
   */
  def apply[T](map: Map[T, Set[T]]): Graph[T] = {
    Graph(map, Map.empty)
  }

  /**
   * Create a directed graph with integer edge weights
   * @param map Mapping (node -> neighbouring nodes)
   * @param weights Mapping (u,v -> cost of edge (u,v))
   * @tparam T
   * @return
   */
  def apply[T](map: Map[T, Set[T]], weights: Map[(T,T), Long]): Graph[T] = {
    // Check that all edges in the graph have an associated weight
     val noWeight = map.collect{
       case (u,nbs) => nbs.filterNot(v => weights.contains(u,v)).map(v => (u,v))
     }
    if (noWeight.nonEmpty) {
      println("WARN: Following edges have no associated weight: ")
      println(noWeight)
    }
    Graph(map, weights)
  }
}