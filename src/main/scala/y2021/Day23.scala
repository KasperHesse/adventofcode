package y2021

import common.{PriorityQueue, Solution}

import scala.annotation.tailrec

sealed trait Amphi
case class A() extends Amphi {
//  override def toString: String = "y2021.A"
}
case class B() extends Amphi {
//  override def toString: String = "y2021.B"
}
case class C() extends Amphi {
//  override def toString: String = "y2021.C"
}
case class D() extends Amphi {
//  override def toString: String = "y2021.D"
}

/**
 * y2021.State representation. Each index in the map corresponds to a position in the corridor or rooms.
 * Rooms are at indexes (2,4,6,8), corridor is indexes (0,1,3,5,7,9,10).
 * This makes it easy to check if an amphi can move from room and into a corridor location, as it only requires checking
 * if the destination is empty, and whether all odd indexes between start and dest are empty
 * @param state Configuration of amphipods in rooms and corridor
 * @param energy Energy required to reach this state from initial configuration
 */
case class State(state: Map[Int, List[Amphi]], energy: Int, maxDepth: Int = 2) {
}

class Day23 extends Solution {
  def stateToString(s: State) = {
    def a(a: Amphi): String = a match {
      case A() => "y2021.A"
      case B() => "y2021.B"
      case C() => "y2021.C"
      case D() => "y2021.D"
    }
    val state = s.state
    val corr = List(0,1,3,5,7,9,10).map(x => if (state(x).isEmpty) "." else a(state(x).head))
    val first = "#" + corr(0) + corr(1) + "." + corr(2) + "." + corr(3) + "." + corr(4) + "." + corr(5) + corr(6) + s"#  ${s.energy}\n"

    val rooms = (0 until s.maxDepth).map{i =>
      (2 to 8 by 2).map(x => if (state(x).length >= s.maxDepth - i) a(state(x)(i - (s.maxDepth - state(x).length))) else ".").mkString(" ##", "#", "## \n")
    }
    val res = rooms.foldLeft("#############\n" + first){case (s,r) => s + r}
    res + "  #########  \n"
  }
  def parseInput(inp: List[String]): State = {
    def charToAmphi(c: Char): Amphi = c match {
      case 'A' => A()
      case 'B' => B()
      case 'C' => C()
      case 'D' => D()
      case _ => throw new IllegalArgumentException(s"Unknown amphipod $c")
    }
    val lines = inp.slice(2, 4) //Only need the lines where initial locations are
    //Indexes 3,5,7,9 are relevant for us
    val first = lines.map(_.charAt(3)).map(charToAmphi)
    val second = lines.map(_.charAt(5)).map(charToAmphi)
    val third = lines.map(_.charAt(7)).map(charToAmphi)
    val fourth = lines.map(_.charAt(9)).map(charToAmphi)
    State(Map(
      0 -> List.empty,
      1 -> List.empty,
      2 -> first,
      3 -> List.empty,
      4 -> second,
      5 -> List.empty,
      6 -> third,
      7 -> List.empty,
      8 -> fourth,
      9 -> List.empty,
      10 -> List.empty
    ), 0)
  }

  def genNextStates(state: State): Set[State] = {
    val stepEnergy: Map[Amphi, Int] = Map(A() -> 1, B() -> 10, C() -> 100, D() -> 1000)
    //Possible next-states:
    //Moving amphipods out of room and into corridor
    //Moving amphipods from corridor and back into room
    //Moving amphipod from one room directly to another. We choose to break it into two
    val corridorPositions = List(0, 1, 3, 5, 7, 9, 10)
    val roomPositions = List(2,4,6,8)
    val homeRoom: Map[Amphi, Int] = Map(A() -> 2, B() -> 4, C() -> 6, D() -> 8)
    /**
     * Checks whether an amphipod in a room can move into the hallway to land at a given position.
     * An amphipod cannot move out if it is in the right room and everyone else in the room
     * is of the same type
     * @param state
     * @param start
     * @param dest
     * @return
     */
    def canMoveOut(state: State, start: Int, dest: Int): Boolean = {
      val oddOnPath = if (dest < start) {
        start-1 until dest by -2
      } else {
        start+1 until dest by 2
      }
      val s = state.state
      s(dest).isEmpty && oddOnPath.forall(idx => s(idx).isEmpty)
        .&& (s(start).exists(a => homeRoom(a) != start))
        //amphipod can only move out if it is not in the correct room, or, if in the right room, someone else in the room is not in the right room
    }

    /**
     * Checks whether an amphipod in a hallway can move into a given room.
     * @param state
     * @param start
     * @param dest
     * @return
     */
    def canMoveInto(state: State, start: Int, dest: Int): Boolean = {
      val oddOnPath = if (dest < start) {
        (if(start == 10) 9 else start-2) until dest by -2
      } else {
        (if(start == 0) 1 else start+2) until dest by 2
      }
      val s = state.state
      //either dest room must be empty, or all inhabitants are of the same type. Must be correct room to move into. All odd items on path must be empty
      (s(dest).isEmpty || s(dest).forall(_ == s(start).head)) && oddOnPath.forall(idx => s(idx).isEmpty) && (dest == homeRoom(s(start).head))
    }

    /**
     * Generate a next state based on the current state, start and destination indices.
     * Assumes that the start,dest indices are valid, does not check this
     * @param init
     * @param start
     * @param dest
     */
    def genNS(init: State, start: Int, dest: Int): State = {
      val nSteps = if (roomPositions.contains(start)) {
        (start-dest).abs + 1 + state.maxDepth - init.state(start).length
      } else {
        (start - dest).abs + state.maxDepth - init.state(dest).length //if already occupied by one amphi, only needs one additional step. Otherwise two steps into room
      }

      val energy = stepEnergy(init.state(start).head) * nSteps
      val amphi = state.state(start).head
      val ns = state.state.updatedWith(start){case Some(v) => Some(v.tail); case None => None}
        .updatedWith(dest){case Some(v) => Some(amphi :: v); case None => None}
      State(ns, init.energy + energy, init.maxDepth)
    }

    //all legal moves from a corridor position into a room
    val corrToRoom = corridorPositions.allPairs(roomPositions).filter{
      case (c,_) => state.state(c).nonEmpty
    }.filter{
      case (start,dest) => canMoveInto(state, start, dest)
    }.toSet

    //If an amphipod can move into a room, we always want to do that
    if (corrToRoom.nonEmpty) {
      corrToRoom.map{case (s,d) => genNS(state, s, d)}
    } else {
      //all legal moves from a room into a corridor position
      val roomToCorr = roomPositions.allPairs(corridorPositions).filter{
        case (r,_) => state.state(r).nonEmpty
      }.filter {
        case (start,dest) => canMoveOut(state, start, dest)
      }.toSet
      roomToCorr.map{case (s,d) => genNS(state, s, d)}
    }
  }

  @tailrec
  final def doDay23(pq: PriorityQueue[State], finish: State, visited: Set[State], cntFilt: Int = 0): State = {
    val s = pq.extract()
    if (s.state == finish.state) {
      println(s"common.Solution found: ${s.energy}")
      println(s"Visited: ${visited.size}")
      println(s"cntFilt: $cntFilt")
      println(s"Remaining in PQ: ${pq.size}")
      s
    } else {
      val ns = genNextStates(s)
      val nsFilt = ns.diff(visited) //No reason to enqueue already visited states
      nsFilt.foreach(s => pq.insert(s, s.energy))
      doDay23(pq, finish, visited.union(nsFilt), cntFilt + ns.size - nsFilt.size)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val initState = parseInput(inp)
    val finishState = parseInput("""#############
                                   |#...........#
                                   |###y2021.A#y2021.B#y2021.C#y2021.D###
                                   |  #y2021.A#y2021.B#y2021.C#y2021.D#
                                   |  #########
                                   |""".stripMargin.split("\n").toList)

    val states = Array(
      /*init*/initState,
      /*y2021.B moves out*/State(Map(0 -> List(), 1 -> List(), 2 -> List(B(), A()), 3 -> List(B()), 4 -> List(C(), D()), 5 -> List(), 6 -> List(C()), 7 -> List(), 8 -> List(D(), A()), 9 -> List(), 10 -> List()), 40),
      /*y2021.C moves up*/State(Map(0 -> List(), 5 -> List(C()), 10 -> List(), 1 -> List(), 6 -> List(C()), 9 -> List(), 2 -> List(B(), A()), 7 -> List(), 3 -> List(B()), 8 -> List(D(), A()), 4 -> List(D())),240),
      /*y2021.C moves into room*/State(Map(0 -> List(), 5 -> List(), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(B(), A()), 7 -> List(), 3 -> List(B()), 8 -> List(D(), A()), 4 -> List(D())),440),
      /*y2021.D moves up*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(B(), A()), 7 -> List(), 3 -> List(B()), 8 -> List(D(), A()), 4 -> List()),3440),
      /*y2021.B moves into room*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(B(), A()), 7 -> List(), 3 -> List(), 8 -> List(D(), A()), 4 -> List(B())),3470),
      /*y2021.B moves up*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(A()), 7 -> List(), 3 -> List(B()), 8 -> List(D(), A()), 4 -> List(B())),3490),
      /*y2021.B moves into room*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(A()), 7 -> List(), 3 -> List(), 8 -> List(D(), A()), 4 -> List(B(), B())),3510),
      /*y2021.D moves up*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(A()), 7 -> List(D()), 3 -> List(), 8 -> List(A()), 4 -> List(B(), B())),5510),
      /*y2021.A moves up*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(A()), 2 -> List(A()), 7 -> List(D()), 3 -> List(), 8 -> List(), 4 -> List(B(), B())),5513),
      /*y2021.D moves down*/State(Map(0 -> List(), 5 -> List(D()), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(A()), 2 -> List(A()), 7 -> List(), 3 -> List(), 8 -> List(D()), 4 -> List(B(), B())),8513),
      /*y2021.D moves down*/State(Map(0 -> List(), 5 -> List(), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(A()), 2 -> List(A()), 7 -> List(), 3 -> List(), 8 -> List(D(), D()), 4 -> List(B(), B())),12513),
      /*y2021.A moves down*/State(Map(0 -> List(), 5 -> List(), 10 -> List(), 1 -> List(), 6 -> List(C(), C()), 9 -> List(), 2 -> List(A(), A()), 7 -> List(), 3 -> List(), 8 -> List(D(), D()), 4 -> List(B(), B())),12521)
    )

//    val s = states(11)
//    val ns = genNextStates(s).toList
//    println("INITIAL STATE")
//    println(stateToString(s))
//    println()
//    ns.sortBy(_.energy).foreach{s =>
//      println(s)
//      println(stateToString(s))
//    }
//    12521

    val pq = new PriorityQueue[State](0)
    pq.insert(initState, 0)
    val res = doDay23(pq, finishState, Set(initState))
    res.energy
  }

  override def solvePart2(inp: List[String]): Any = {
    val preState = parseInput(inp).state
    val s1 = preState.updatedWith(2){case Some(v) => Some(v.head :: D() :: D() :: v.last :: Nil)}
    val s2 = s1.updatedWith(4){case Some(v) => Some(v.head :: C() :: B() :: v.last :: Nil)}
    val s3 = s2.updatedWith(6){case Some(v) => Some(v.head :: B() :: A() :: v.last :: Nil)}
    val s4 = s3.updatedWith(8){case Some(v) => Some(v.head :: A() :: C() :: v.last :: Nil)}
    val initState = State(s4, 0, 4)
    val finishState = State(Map(0 -> List(), 5 -> List(), 10 -> List(), 1 -> List(), 6 -> List(C(), C(), C(), C()), 9 -> List(), 2 -> List(A(), A(), A(), A()), 7 -> List(), 3 -> List(), 8 -> List(D(), D(), D(), D()), 4 -> List(B(), B(), B(), B())),0,4)

    val pq = new PriorityQueue[State](0)
    pq.insert(initState, 0)
    val res = doDay23(pq, finishState, Set(initState))
    res.energy
  }
}

object Day23 extends App {
  Solution.solve(new Day23, 23, 12521, 44169)
}