package y2023

import common._

import scala.annotation.tailrec

class Day19 extends Solution {
  //An object for storing an item's properties
  case class XMAS(x: Int, m: Int, a: Int, s: Int)
  //A rule for checking where an item should go, and the destination it should go to
  case class Rule(pred: XMAS => Boolean, dest: String)
  //A workflow, containing a series of rules
  case class Workflow[T](name: String, rules: List[T])

  def parseRuleP1(in: String): Rule = {
    //Special case: Last rule in a list is always uncondtional.
    // In that case, return function that always return true
    if (!in.contains(':')) {
      Rule(_ => true, in)
    } else {
      val c = in.head
      val op = in.charAt(1) match {
        case '<' => (a: Int, b: Int) => a < b
        case '>' => (a: Int, b: Int) => a > b
      }
      val colonPos = in.indexOf(':')
      val value = in.substring(2, colonPos).toInt
      val dest = in.substring(colonPos+1)

      val pred = c match {
        case 'x' => (xmas: XMAS) => op(xmas.x, value)
        case 'm' => (xmas: XMAS) => op(xmas.m, value)
        case 'a' => (xmas: XMAS) => op(xmas.a, value)
        case 's' => (xmas: XMAS) => op(xmas.s, value)
      }
      Rule(pred, dest)
    }
  }

  @tailrec
  final def processItemP1(bins: Map[String, List[XMAS]], workflows: Map[String, Workflow[Rule]]): Map[String, List[XMAS]] = {
    //When all other bins than 'A' and 'R' are empty, we are finished
    if (bins.forall { case (k, v) => v.isEmpty || ((k == "R") || (k == "A")) }) {
      bins
    } else {
      //Go through each bin, moving all items to their next bin
      val newBins = bins.foldLeft(Map.empty[String, List[XMAS]]) { case (m, (label, xmass)) =>
        xmass.foldLeft(m) { case (m, xmas) =>
          //Find first rule for currently mapped workflow that evaluates true. Move item to that one
          //If item is mapped to A or R, must stay there
          val dest = if (Seq("A", "R").contains(label)) label else workflows(label).rules.find(_.pred(xmas)).get.dest
          m.updatedWith(dest) { case Some(list) => Some(xmas :: list); case None => Some(List(xmas)) }
        }
      }
      processItemP1(newBins, workflows)
    }
  }

  //Object for storing range properties for part 2 of the problem
  case class XMASRange(x: Range, m: Range, a: Range, s: Range) {
    def split(c: Char, tf: (Range, Range)): (XMASRange, XMASRange) = {
      val (t,f) = tf
      c match {
        case 'x' => (XMASRange(t, m, a, s), XMASRange(f, m, a, s))
        case 'm' => (XMASRange(x, t, a, s), XMASRange(x, f, a, s))
        case 'a' => (XMASRange(x, m, t, s), XMASRange(x, m, f, s))
        case 's' => (XMASRange(x, m, a, t), XMASRange(x, m, a, f))
      }
    }
  }
  //Object for storing rules for part 2. Now, a range maps (dest of true, true range, false range)
  case class RuleRange(pred: XMASRange => (String, XMASRange, XMASRange))

  def parseRuleP2(in: String): RuleRange = {
    //Special case: Last rule is always unconditional
    //Return the input range unmodified
    if (!in.contains(":")) {
      RuleRange(x => (in, x, x))
    } else {
      val c = in.head
      val op = in.charAt(1)
      val colonPos = in.indexOf(':')
      val value = in.substring(2, colonPos).toInt
      val dest = in.substring(colonPos+1)

      //Splits a range at 'value' using the less-than condition
      //One of the returned ranges may be empty (0,0,1) if that value is outside range of input
      //First range returned is the (true) condition, second is the (false) condition
      def splitRangeLT(r: Range, value: Int): (Range, Range) = {
        if (r.start < value && value <= r.end) { //Crosses value, split into two ranges
          (Range(r.start, value - 1), Range(value, r.end))
        } else if (r.end < value) {
          (r, Range(0,0))
        } else if (value <= r.start) {
          (Range(0,0), r)
        } else {
          throw new IllegalArgumentException(s"Range $r and value $value did not map to something useful")
        }
      }
      //Splits a range at 'value' using the greater-than condition
      def splitRangeGT(r: Range, value: Int): (Range, Range) = {
        if (r.start <= value && value < r.end) { //Crosses value, split
          (Range(value + 1, r.end), Range(r.start, value))
        } else if (r.end <= value) { //Entire range is <= value, none are greater. Return all in false
          (Range(0,0), r)
        } else if (r.start > value) { //Entire range is > value, return all in true
          (r, Range(0,0))
        } else {
          throw new IllegalArgumentException(s"Range $r and value $value did not map to something useful")
        }
      }
      //This is pretty ugly, but can't think of a better way of doing it
      val mapping = (c, op) match {
        case ('x', '<') => (xmas: XMASRange) => xmas.split(c, splitRangeLT(xmas.x, value))
        case ('x', '>') => (xmas: XMASRange) => xmas.split(c, splitRangeGT(xmas.x, value))
        case ('m', '<') => (xmas: XMASRange) => xmas.split(c, splitRangeLT(xmas.m, value))
        case ('m', '>') => (xmas: XMASRange) => xmas.split(c, splitRangeGT(xmas.m, value))
        case ('a', '<') => (xmas: XMASRange) => xmas.split(c, splitRangeLT(xmas.a, value))
        case ('a', '>') => (xmas: XMASRange) => xmas.split(c, splitRangeGT(xmas.a, value))
        case ('s', '<') => (xmas: XMASRange) => xmas.split(c, splitRangeLT(xmas.s, value))
        case ('s', '>') => (xmas: XMASRange) => xmas.split(c, splitRangeGT(xmas.s, value))
      }
      val pred = (xmas: XMASRange) => {
        val (t,f) = mapping(xmas)
        (dest, t, f)
      }
      RuleRange(pred)
    }
  }

  //Split an XMAS item from part 2:
  //Takes the input item range and splits into two new ranges. The ranges matching the
  //head rule, and the range that not matching. Range not matching head rule is processed by
  //next rule, until no rules remain
  def splitXMASP2(xmas: XMASRange, rules: List[RuleRange]): List[(String, XMASRange)] = {
    @tailrec
    def loop(xmas: XMASRange, rules: List[RuleRange], acc: List[(String, XMASRange)]): List[(String, XMASRange)] = {
      rules match {
        case Nil => acc
        case rule::tail =>
          val (dest, r1, r2) = rule.pred(xmas)
          if (Seq(r2.x.start, r2.m.start, r2.a.start, r2.s.start).contains(0)) {
            //If r2 has any of the subranges empty (start=0), nothing was mapped to that subrange
            //Can terminate early
            (dest,r1)::acc
          } else {
            loop(r2, tail, (dest,r1)::acc)
          }
      }
    }
    loop(xmas, rules, Nil)
  }

  @tailrec
  final def processItemsP2(bins: Map[String, List[XMASRange]], workflows: Map[String, Workflow[RuleRange]]): Map[String, List[XMASRange]] = {
    if (bins.forall{case (k,_) => Seq("R","A").contains(k)}) {
      bins
    } else {
      //Go through each bin and each item in a bin
      val newBins = bins.foldLeft(Map.empty[String, List[XMASRange]]) {case (m, (label, xmass)) =>
        xmass.foldLeft(m) {case (m,xmas) =>
          val split = if (Seq("A","R").contains(label)) { //No further mapping required
            List((label, xmas))
          } else {
            splitXMASP2(xmas, workflows(label).rules)
          }
          split.foldLeft(m){case (m,(dest, xmas)) =>
            m.updatedWith(dest) {case Some(list) => Some(xmas::list); case None => Some(List(xmas))}
          }
        }
      }
      processItemsP2(newBins, workflows)
    }
  }


  override def solvePart1(inp: List[String]): Any = {
    val blocks = Solution.splitOnBlankLines(inp)
    val workflows = blocks.head.map{case s"$bin{$rules}" =>
      (bin, Workflow(bin, rules.split(",").map(parseRuleP1).toList))
    }.toMap
    val items = blocks.last.map{
      case s"{x=$x,m=$m,a=$a,s=$s}" => XMAS(x.toInt,m.toInt,a.toInt,s.toInt)
    }
    val startBins = Map("in" -> items)
    val endBins = processItemP1(startBins, workflows)
    endBins.collect{case (k,v) if k == "A" =>
      v.map(xmas => xmas.x + xmas.m + xmas.a + xmas.s).sum
    }.sum
  }

  override def solvePart2(inp: List[String]): Any = {
    /*
    Must use our range class
    In "in", we input the item with all ranges from [1:4000].
    For each of the rules in the workflow, some sub-range will be accepted,
    another sub-range will be moved on to the next bin.
    Proceeding through all rules of the workflow will map each item in bin into new
    sub-ranges in other bins. Rinse-repeat until finished
     */
    val blocks = Solution.splitOnBlankLines(inp)
    val workflows = blocks.head.map{
      case s"$bin{$rules}" => (bin, Workflow(bin, rules.split(",").map(parseRuleP2).toList))
    }.toMap
    //Starting item covers all ranges
    val items = List(XMASRange(Range(1, 4000), Range(1, 4000), Range(1, 4000), Range(1, 4000)))
    val startBins = Map("in" -> items)
    val endBins = processItemsP2(startBins, workflows)
    endBins.collect{case (k,v) if k == "A" =>
      v.map(xmas => xmas.x.len * xmas.m.len * xmas.a.len * xmas.s.len).sum
    }.sum
  }
}

object Day19 extends App {
  Solution.solve(new Day19, 19, 2023, 19114, 167409079868000L)
}
