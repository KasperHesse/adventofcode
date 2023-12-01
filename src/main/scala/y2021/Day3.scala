package y2021

import common.Solution

import scala.annotation.tailrec

class Day3 extends Solution {
  //Count the number of ones in the input at each position
  def countOnes(z: List[Array[Int]]): Array[Int] = {
    z.fold(Array.fill(z.head.length)(0))( //Array of counter variables, all init to 0
      (c,x) => c.zip(x).map(x => x._1 + x._2) //Add each value in this line of z to counter
    )
  }

  override def solvePart1(inp: List[String]): Any = {
    //Compute gamma and epsilon rate of input
    //Convert to char array, then int array. Then, for each index, count number of 1's
    val z = inp.map(_.toCharArray).map(_.map(_.toInt - 48))
    val res = countOnes(z)
    val gamma = res.map(x => if (x > z.length/2) 1 else 0)
    val epsilon = gamma.map(x => if (x==1) 0 else 1)
    Integer.parseInt(gamma.mkString("","",""), 2) * Integer.parseInt(epsilon.mkString("","",""), 2)
  }

  override def solvePart2(inp: List[String]): Any = {
    /**
     * Find the CO2/Oxygen rating for a list of inputs
     * @param z The list of inputs under consideration
     * @param pos The position we're considering
     * @param selF Selection function to determine which bit value to keep.
     *             First parameter is count of ones in z at position, second is length of z
     * @return     Int value of rating selected by function
     */
    @tailrec
    def findRating(z: List[Array[Int]], pos: Int, selF: (Int, Int) => Int): Int = {
      if (z.length == 1) {
        Integer.parseInt(z.head.mkString("","",""), 2)
      } else {
        val cnts = countOnes(z)
        //with count of ones: Oxygen rating should determine whether to keep all with 1's or 0's
        //Extract at pos, then map to 1 or 0. Then filter based on that value at pos
        val sel = selF(cnts(pos), z.length)
        val zNew = z.filter(x => x(pos) == sel)
        findRating(zNew, pos+1, selF)
      }
    }
    //Convert from string to int arrays
    val z = inp.map(_.toCharArray).map(_.map(_.toInt - 48))
    //Oxygen: Select most common value, tiebreak to 1
    val oxygen = findRating(z, 0, (cnt,len) => if (cnt >= len - cnt) 1 else 0)
    //CO2: Select least common value, tiebreak to 0
    val co2 = findRating(z, 0, (cnt,len) => if (len - cnt <= cnt) 0 else 1)
    oxygen * co2
  }
}

object Day3 extends App {
  Solution.solve2021(new Day3, 3, 198, 230)
}
