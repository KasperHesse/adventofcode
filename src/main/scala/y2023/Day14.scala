package y2023

import common._

import scala.annotation.tailrec

class Day14 extends Solution {

  //Checks if a boulder can move up. Only if not on topmost row, is a boulder and field above is empty
  def canMoveUp(xy: (Int,Int), m: Map[(Int,Int), Char]): Boolean = {
    xy._2 > 0 && m(xy) == 'O' && !m.contains((xy._1, xy._2 - 1))
  }

  def canMoveDown(maxY: Int)(xy: (Int, Int), m: Map[(Int,Int), Char]): Boolean = {
    xy._2 < maxY-1 && m(xy) == 'O' && !m.contains((xy._1, xy._2 + 1))
  }

  def canMoveLeft(xy: (Int, Int), m: Map[(Int, Int), Char]): Boolean = {
    xy._1 > 0 && m(xy) == 'O' && !m.contains((xy._1 - 1, xy._2))
  }

  def canMoveRight(maxX: Int)(xy: (Int, Int), m: Map[(Int, Int), Char]): Boolean = {
    xy._1 < maxX - 1 && m(xy) == 'O' && !m.contains((xy._1 + 1, xy._2))
  }

  /**
   * Move the rocks on the board in a given direction
   * @param m The map with the rocks on it
   * @param pred Predicate checking if a rock can move
   * @param dx X-offset to add to moved rocks
   * @param dy Y-offset to add to moved rocks
   * @return Map after all rocks have moved as far in the (dx,dy) direction as possible
   */
  @tailrec
  final def moveRocks(m: Map[(Int,Int), Char],
                      pred: ((Int,Int), Map[(Int,Int),Char]) => Boolean,
                      dx: Int, dy: Int): Map[(Int,Int), Char] = {
    val (canMove, cannotMove) = m.partition(v => pred(v._1, m))
    if (canMove.isEmpty) {
      cannotMove
    } else {
      //cannotMove remains const, we loop over the items in canMove and update the y-value
      val newMap = canMove.keys.foldLeft(cannotMove){case (m,(x,y)) => m.updated((x + dx, y + dy), 'O')}
      moveRocks(newMap, pred, dx, dy)
    }
  }

  /**
   * Do a single iteration of the north/west/south/east cycle
   * @param m The map to move rocks on
   * @param maxX Maximum x-coordinate that rocks can have
   * @param maxY Maximum y-coordinate that rocks can have
   * @return
   */
  def doSingleCycle(m: Map[(Int,Int), Char], maxX: Int, maxY: Int): Map[(Int,Int), Char] = {
    val predDown = canMoveDown(maxY)_
    val predRight = canMoveRight(maxX)_
    val predUp = canMoveUp _
    val predLeft = canMoveLeft _

    val up = moveRocks(m, predUp, 0, -1)
    val left = moveRocks(up, predLeft, -1, 0)
    val down = moveRocks(left, predDown, 0, 1)
    val resMap = moveRocks(down, predRight, 1, 0)

    resMap
  }

  /**
   * Move the rocks many many times, detecting a cycle in the rocks movement and aborting early
   * @param m The map to keep working from
   * @param states Mapping from iteration number to state
   * @param maxX Maximum X-coordinate
   * @param maxY Maximum Y-coordinate
   * @return
   */
  @tailrec
  final def detectCycle(m: Map[(Int,Int), Char],
                        states: Map[Int, Map[(Int,Int), Char]],
                        maxX: Int, maxY: Int): Map[(Int,Int), Char] = {
    val m2 = doSingleCycle(m, maxX, maxY)
    val matched = states.find(_._2 == m2)
    if (matched.nonEmpty) {
      println(s"Found cycle after ${states.size} iterations. Match has index ${matched.get._1}")
      val cycleLength = states.size - matched.get._1 + 1
      val cyclesToLoop = 1000000000 - matched.get._1 + 1
      val rem = cyclesToLoop % cycleLength
      states(matched.get._1 + rem)
    } else {
      detectCycle(m2, states.updated(states.size + 1, m2), maxX, maxY)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val startMap = Solution.parseAsMap(inp).filter{case (k,v) => v != '.'}
    val endMap = moveRocks(startMap, canMoveUp, 0, -1)

    //For each item in endMap that is a boulder, map to 100-y
    endMap.collect{case ((x,y),'O') => inp.length - y}.sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val startMap = Solution.parseAsMap(inp).filter{case (k,v) => v != '.'}

    val cycleRes = detectCycle(startMap, Map(0 -> startMap), inp.head.length, inp.length)

    cycleRes.collect{case ((x,y),'O') => inp.length - y}.sum
  }
}

object Day14 extends App {
  Solution.solve(new Day14, 14, 2023, 136, 64)
}
