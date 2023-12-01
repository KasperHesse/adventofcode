package y2021

import common.Solution

class Day7 extends Solution {

  /**
   * Compute fuel consumption for moving all crabs with a linear cost
   * @param crabs
   * @param pos
   * @return
   */
  def fuelConsumptionLinear(crabs: Array[Int], pos: Int): Int = {
    //for each value, abs of that value - pos
    crabs.map(c => Math.abs(c-pos)).sum
  }

  /**
   * Compute fuel consumption for moving all crabs with an exponential cost
   * @param crabs
   * @param pos
   * @return
   */
  def fuelConsumptionExp(crabs: Array[Int], pos: Int): Int = {
    crabs.map(c => Math.abs(c-pos)).map(n => n*(n+1)/2).sum
    //Moving 1 => 1
    //Moving 2 => 3 (1+2)
    //Moving 3 => 6 (1+2+3)
    //Moving 4=> 10 (1+2+3+4)
    //moving 4 => 4*(4+1)/2=4*5/2=20/2=10

    //Exponential increase?
  }

  /**
   * Find best position for the crabs to occupy
   * @param crabs original crab positions
   * @param l Left (lower) bound for position under consideration
   * @param r right (upper) bound for position under consideration
   * @return
   */
  def findBestPosition(crabs: Array[Int], l: Int, r: Int, fc: (Array[Int], Int) => Int): Int = {
    val m = (l+r)/2 //Middle of range under consideration
    val prev = fc(crabs, m-1)
    val here = fc(crabs, m)
    val next = fc(crabs, m+1)

    if (here < prev && here < next) { //Find lowest point
      m
    } else if (prev < here) { //prev is lower, must investigate range [l, m-1]
      findBestPosition(crabs, l, m-1, fc)
    } else { //assuming next is lower
      findBestPosition(crabs, m+1, r, fc)
    }
  }
  override def solvePart1(inp: List[String]): Any = {
    val crabs = inp.head.split(",").map(_.toInt)
    //Do a binary peak-finding search on the array
    val l = crabs.min
    val r = crabs.max
    val pos = findBestPosition(crabs, l, r, fuelConsumptionLinear)
    fuelConsumptionLinear(crabs, pos)
  }

  override def solvePart2(inp: List[String]): Any = {
    val crabs = inp.head.split(",").map(_.toInt)
    //Do a binary peak-finding search on the array
    val l = crabs.min
    val r = crabs.max
    val pos = findBestPosition(crabs, l, r, fuelConsumptionExp)
    fuelConsumptionExp(crabs, pos)
  }
}

object Day7 extends App {
  Solution.solve2021(new Day7, 7, 37, 168)
}