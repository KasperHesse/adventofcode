class Day12 extends Solution {

  def parseInput(m: Map[String, List[String]], lines: List[String]): Map[String, List[String]] = {
    if (lines.isEmpty) {
      m
    } else {
      val s = lines.head.split("-")
      val m2 =  m.updatedWith(s(0)){case Some(nbs) => Some(s(1)::nbs); case None => Some(List(s(1)))}
      val m3 = m2.updatedWith(s(1)){case Some(nbs) => Some(s(0)::nbs); case None => Some(List(s(0)))}
      parseInput(m3, lines.tail)
    }
  }

  /**
   * Do the BFS search for all paths
   * @param m Map of nodes in the cave
   * @param visited lowercase places visited on this path
   * @param n Current node
   * @return List of all paths tried. Each path is a list of the nodes in order they were visited
   */
  def doDFSPart1(m: Map[String, List[String]], visited: List[String], n: String): List[List[String]] = {
    if (n == "end") {
      List(List("end"))
    } else {
      //only keep neighbours that haven't already been visited
      val nbs = m(n).filter(nb => !visited.contains(nb))
      //each neighbour should explore all paths and return those
      val visits = if (n.head.isLower) n::visited else visited
      val nbPaths = nbs.map(nb => doDFSPart1(m, visits, nb))
      val retVal = nbPaths.foldLeft(List.empty[List[String]])((acc, lls) => lls.foldLeft(acc)((a,ls) => ls::a))
        .map(ls => n::ls) //prepend this node's name to all paths going through it
      retVal
    }
  }

  def doDFSPart2(m: Map[String, List[String]], visited: List[String], n: String, usedDouble: Boolean): Int = {
    if (n == "end") {
      1
    } else {
      //if some place has already been double-visited: filter out all lowercase neighbours
      //if not, spawn all possible combinations of visiting each of those now, or
      val (nbsVisited, nbsNot) = m(n).partition(nb => visited.contains(nb))
      //nbs visited can be attempted visited again if usedDouble is not set. If set, ignore
      //all those not visited can be visited in the same way as before
      val visits = if (n.head.isLower) n::visited else visited

      val doubleVisits = if (!usedDouble) nbsVisited.map(nb => doDFSPart2(m, visits, nb, true)) else List(0)
      val nbVisits = nbsNot.map(nb => doDFSPart2(m, visits, nb, usedDouble))
      doubleVisits.sum + nbVisits.sum
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val m = parseInput(Map.empty[String, List[String]], inp)
    //Convert to map(String, List[String]). Each entry maps to all others that it is connected to
    //Do a pseudo-BFS type of thing. Enqueue in the same way, but only enqueue if cave name is uppercase and hasn't been visited
    //keep list of visited lowercase caves
    val r = doDFSPart1(m, List.empty, "start")
    r.length

  }

  override def solvePart2(inp: List[String]): Any = {
    val m = parseInput(Map.empty, inp)  //Only add paths FROM start and TO end
      .updatedWith("end")(_ => None) //Removes all outgoing from end
      .map{case (s, ls) => (s, ls.filterNot(_.equals("start")))} //removes all ingoing to start
    val r = doDFSPart2(m, List.empty, "start", false)
    r

  }
}

object Day12 extends App {
  Solution.solve(new Day12, 12, 10, 36)
}