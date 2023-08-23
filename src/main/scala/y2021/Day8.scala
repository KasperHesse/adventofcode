package y2021

import common.Solution

class Day8 extends Solution {

  case class SevenSegInfo(inputs: List[String], outputs: List[String])
  object SevenSegInfo {
    def apply(s: String): SevenSegInfo = {
      val split = s.split(" \\| ")
      SevenSegInfo(split(0).split(" ").map(_.sorted).toList, split(1).split(" ").map(_.sorted).toList)
    }
  }
  override def solvePart1(inp: List[String]): Any = {
    //Map each input to a case class. Then map each info to the outputs of correct length, get length of coll
    val ssis = inp.map(x => SevenSegInfo(x))
    val z = ssis.flatMap(ssi => ssi.outputs.filter(s => Seq(2,3,4,7).contains(s.length))).length
    z
  }

  def computeSevenSegValue(ssi: SevenSegInfo): Int = {
    /*
    Order of operations:
    Identify (1): Entry with 2 characters
    Identify (7): Entry with 3 characters. Character not in (1) gives top line
    Identify (6): Entry with 6 characters where only one of those in (1) exists. Gives bottom-right line
                  Once bottom-right is isolated, other character in (1) gives top-right line
    Identify (4): Entry with 4 characters. The two chars not in (1) are middle and top-left
    Identify (0): Entry with 6 characters where only one of the two extra chars in (4) are present
                  Extra char present in (4) and in (0): top-left line
                  Additional char from (4) not in (0): Middle line
    Identify (9): Entry with 6 characters with only one unidentified char. Unidentified char is bottom
                  Final character not yet identified is bottom-left line

    Representation:
      Convert all strings to char sets (or string sets, whatever is easiest)
      Sort each string to make parsing simpler
      Once a string->num combination has been identified, store it in a map
      Once all have been identified, compute results
     */
    val one = ssi.inputs.find(_.length == 2).get
    val four = ssi.inputs.find(_.length == 4).get
    val seven = ssi.inputs.find(_.length == 3).get
    val eight = ssi.inputs.find(_.length == 7).get

    val topLine = seven.diff(one)
    val six = ssi.inputs.find(x => x.length == 6 && one.diff(x).length == 1).get
    val bottomRightLine = six.intersect(one)
    val topRightLine = one.diff(bottomRightLine)

    val fourCands = four.diff(one)
    val zero = ssi.inputs.find(x => x.length == 6 && fourCands.diff(x).length == 1).get
    val topLeftLine = fourCands.intersect(zero)
    val middleLine = fourCands.diff(topLeftLine)

    val almostNine = (topLine + topLeftLine + topRightLine + middleLine + bottomRightLine)
    val nine = ssi.inputs.find(x => x.length == 6 && x.diff(almostNine).length == 1).get
    val bottomLine = nine.diff(almostNine)
    val bottomLeftLine = eight.diff(nine)

    val two = (topLine + topRightLine + middleLine + bottomLeftLine + bottomLine).sorted
    val three = (topLine + topRightLine + middleLine + bottomRightLine + bottomLine).sorted
    val five = (topLine + topLeftLine + middleLine + bottomRightLine + bottomLine).sorted

    val digitMap = Map.from(Seq((zero,0), (one,1), (two,2), (three,3), (four,4), (five,5), (six,6), (seven,7), (eight,8), (nine,9)))

    //Result:
    digitMap(ssi.outputs(0))*1000 + digitMap(ssi.outputs(1))*100 + digitMap(ssi.outputs(2))*10 + digitMap(ssi.outputs(3))
  }

  override def solvePart2(inp: List[String]): Any = {
    inp.map(x => SevenSegInfo(x))
      .map(computeSevenSegValue)
      .sum
  }
}

object Day8 extends App {
  Solution.solve(new Day8, 8, 26, 61229)
}