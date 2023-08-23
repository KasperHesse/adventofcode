package y2021

import common.Solution

import scala.collection.immutable.Queue

class Day11 extends Solution {

  case class Octopus(energy: Int, hasFlashed: Boolean = false)

  /**
   * Parse the input, converting it to a map from (x,y) coordinate to that octopus
   * @param inp
   * @return
   */
  def parseInput(inp: List[String]): Map[(Int,Int), Octopus] = {
    val z = inp.zipWithIndex.flatMap{case (s,x) => s.zipWithIndex.map{case (c,y) => (c.toInt - 48, x, y)}} //converted to list of (energy, x, y)
      .foldLeft(Map.empty[(Int,Int), Octopus]){case (m, (energy, x, y)) => m.updated((x,y), Octopus(energy))}
    z
  }

  /**
   * Parse a queue of to-be-flashed octopuses, flashing them in turn. If already flashed when popped off queue,
   * an octopus is simply skipped
   * @param m Map of octopuses
   * @param q Queue of octopuses to be flashed
   * @return
   */
  def flashOctopus(m: Map[(Int, Int), Octopus], q: Queue[(Int,Int)]): Map[(Int, Int), Octopus] = {
    val deltas = List.from(Seq((-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1))) //deltas of neighbours
    if (q.isEmpty) {
      m
    } else {
      val (coords, q2) = q.dequeue
      if (!m(coords).hasFlashed) {
        val m2 = m.updated(coords, Octopus(m(coords).energy, true)) //Update this octopus to have flashed
        val nbs = deltas.map(dxy => (coords._1 + dxy._1, coords._2 + dxy._2))

        //Find all neighbour octopi and increase their energy level. updatedWith allows for easily handling of non-existent neighbours
        val m3 = nbs.foldLeft(m2)((m, c) => m.updatedWith(c){
            case Some(octo) => Some(Octopus(octo.energy + 1, octo.hasFlashed))
            case None => None})
        //Find all neighbours with energy > 9 that haven't blinked yet
        val toFlash = nbs.foldLeft(List.empty[(Int,Int)])(
          (acc,nb) => if (m3.contains(nb) && m3(nb).energy > 9 && !m3(nb).hasFlashed) nb::acc else acc
        )
        flashOctopus(m3, q2.enqueueAll(toFlash))
      } else { //If already blinked, just skip and go to next
        flashOctopus(m, q2)
      }
    }
  }

  /**
   * Blink the octopi, progressing the simulation
   * @param m The map of octopus coordinates and energy levels
   * @param blinks Number of blinks that have been performed
   * @param rounds Number of rounds still to perform
   * @return
   */
  def blinkStep(m: Map[(Int, Int), Octopus], blinks: Int, rounds: Int): Int = {
    if (rounds == 0) {
      blinks
    } else {
      //First, raise energy levels. Then, take all octopi with energy 9 and add to a queue of octopi to blink
      //use queue function. It dequeues one, blinks it if not already blinked, sets hasBlinked=true in map and increments all neighbours
      val incMap = m.map { case (coord, octo) => (coord, Octopus(octo.energy + 1)) }
      val toFlash = incMap.filter(_._2.energy > 9).keys
      val flashedMap = flashOctopus(incMap, Queue.from(toFlash))
      val nFlashes = flashedMap.count(_._2.hasFlashed)
      //Reset all flashed to !hasFlashed and 0 energy
      val resMap = flashedMap.map{case (coord, octo) => if (octo.hasFlashed) (coord, Octopus(0)) else (coord, octo)}
      blinkStep(resMap, blinks + nFlashes, rounds-1)
    }
  }

  def calcSyncFlashTime(m: Map[(Int, Int), Octopus], rounds: Int): Int = {
    //First, raise energy levels. Then, take all octopi with energy >9 and add to a queue of octopi to blink
    val incMap = m.map { case (coord, octo) => (coord, Octopus(octo.energy + 1)) }
    val toFlash = incMap.filter(_._2.energy > 9).keys //coords of those to flash
    val flashedMap = flashOctopus(incMap, Queue.from(toFlash))
    val nFlashes = flashedMap.count(_._2.hasFlashed)
    if (nFlashes == 100) {
      rounds + 1
    } else {
      //Reset all flashed to !hasFlashed and 0 energy
      val resMap = flashedMap.map{case (coord, octo) => if (octo.hasFlashed) (coord, Octopus(0)) else (coord, octo)}
      calcSyncFlashTime(resMap, rounds+1)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val m = parseInput(inp)
    val res = blinkStep(m, 0, 100)
    res
    //parse input. Convert to map (int,int)->octopus
  }

  override def solvePart2(inp: List[String]): Any = {
    val m = parseInput(inp)
    calcSyncFlashTime(m, 0)
  }
}

object Day11 extends App {
  Solution.solve(new Day11, 11, 1656, 195)
}