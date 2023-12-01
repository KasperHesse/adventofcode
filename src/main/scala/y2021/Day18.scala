package y2021

import common.Solution

class Day18 extends Solution {

  abstract class SnailfishNum
  case class Lit(v: Int) extends SnailfishNum
  case class Pair(l: SnailfishNum, r: SnailfishNum) extends SnailfishNum

  abstract class Dir
  case class Left() extends Dir
  case class Right() extends Dir

  //Parse a string into a snailfish number
  //Assumes that none of the values are in the original number are >9
  def parseSN(s: String): (SnailfishNum, String) = {
    s.head match {
      //It's a pair. Parse as pair, get remaining substring and parse next from that
      case '[' => {
        val (l, sub) = parseSN(s.tail)
        val (r, subb) = parseSN(sub.drop(1))
        (Pair(l, r), subb.drop(1))
      }
      //Must be a lit?
      //After a lit, the next char is useless. Perhaps 2 if it is "],"
      case x => (Lit(x.toInt - 48), s.tail)
    }
  }

  //Adds two snailfish numbers
  def add(l: SnailfishNum, r: SnailfishNum): SnailfishNum = {
    Pair(l, r)
  }

  def explode(sn: SnailfishNum): (SnailfishNum, Boolean) = {
    val (r, v, _) = doExplode(sn, 0)
    (r, v != -1)
  }

  /**
   * Perform the explode operation on a snailfish number: TODO: Intermediate values should not be user-facing
   * @param sn The number to explode
   * @param depth The depth we are currently at. Used to detect when depth 4 is reached
   * @return 3-tuple: (value, dir, sn)
   *         If value is -2, replacement was performed at a lower level, should not be attempted further
   *         If value is -1, replacement was not performed, should be attempted further
   *         If value is >=0, replacement should be performed / propagated
   *
   *         If dir==0 and value>=0, value should be propagated to next literal on the left
   *         If dir==1 and value>=0, value should be propagated to next literal on the right
   *
   *         sn is the resultant SnailfishNum of performing the explode operation
   */
  private def doExplode(sn: SnailfishNum, depth: Int): (SnailfishNum, Int, Dir) = {
    //Propagate a value into a subtree, adding it to a literal once that is encountered
    def propagate(sn: SnailfishNum, v: Int, dir: Dir): SnailfishNum = {
      sn match {
        case Lit(x) => Lit(x+v)
        case Pair(l,r) => dir match {
          case Left() => Pair(propagate(l, v, dir), r)
          case Right() => Pair(l, propagate(r, v, dir))
        }
      }
    }

    sn match {
      case Lit(v) => (sn, -1, Left()) //Cannot explode lits, go back up immediately
      case Pair(l, r) => if (depth == 3) {
        val (l2: SnailfishNum, r2: SnailfishNum, value: Int, dir: Dir) = (l,r) match {
          //if l is pair: Replace with 0, propagate y-value into r (should move left), return x back up for insertion to the left (moving right)
          case (Pair(Lit(x),Lit(y)),_) => (Lit(0), propagate(r, y, Left()), x, Left())
          //if r is pair and l is not: Replace with 0, propagate x-value in l (should move right), return y back up for insertion to the left
          case (_,Pair(Lit(x),Lit(y))) => (propagate(l, x, Right()), Lit(0), y, Right())
          //All other cases, both are lits. Can return immediately without replacements
          case _ => (l, r, -1, Left())
        }
        //Return new pair and value,dir from attempted propagation
        (Pair(l2,r2), value, dir)
      } else { //Depth not yet 3. Attempt explosion first on left, then on right
        val (l2, v, dir) = doExplode(l, depth+1)
        //If v == -2, return immediately because everything has been done below us
        //if v == -1, attempt to explode r
        //If v >= 0, either propagate into r (if dir==Left()), or return back up (if dir==y2021.Right())
        (v, dir) match {
          case (-2, _) => (Pair(l2, r), v, dir) //All done on level below us
          case (x, Left()) if x>=0 => (Pair(l2, r), v, dir) //Propagate value up for insertion in another subtree
          case (x, Right()) if x>=0 => val r2 = propagate(r, v, Left()) //Propagate value into right subtree, as far left as it can go. This should always succeed. Can now return -2
            (Pair(l2, r2), -2, dir)
          case (-1, _) => { //Attempt to explode r
            val (r2, v, dir) = doExplode(r, depth + 1)
            (v, dir) match {
              case (-2, _) => (Pair(l2, r2), v, dir) //All done on level below us
              case (-1, _) => (Pair(l2, r2), v, dir)
              case (x, Left()) => val l3 = propagate(l2, v, Right()) //Propagate into left subtree, as far right as possible
                (Pair(l3, r2), -2, dir)
              case (x, Right()) => (Pair(l2,r2), v, dir)//Propagate up for insertion in another subtree
            }
          }
        }
      }
    }
  }

  //Split a snailfish number:
  // Should only split the leftmost. Try left tree, return SN and bool to indicate whether a split happened in there
  def split(sn: SnailfishNum): (SnailfishNum, Boolean) = {
    sn match {
      case Lit(x) if x>=10 => (Pair(Lit(x/2), Lit(x/2 + x%2)), true)
      case Lit(_) => (sn, false)
      case Pair(l,r) =>
        val (l2, wasSplit) = split(l)
        if (wasSplit) {
          (Pair(l2, r), true)
        } else {
          val (r2, wasSplit) = split(r)
          (Pair(l2, r2), wasSplit)
        }
    }
  }

  def reduce(sn: SnailfishNum): SnailfishNum = {
    //First attempt explode, then split if it didn't explode
    val (sn2, exploded) = explode(sn)
    val (sn3, wasSplit) = if (exploded) (sn2, false) else split(sn2)
    if (exploded || wasSplit) {
      reduce(sn3)
    } else {
      sn3
    }
  }

  def magnitude(sn: SnailfishNum): Long = sn match {
    case Lit(v) => v.toLong
    case Pair(l,r) => magnitude(l)*3 + magnitude(r)*2
  }

  def bestMagnitude(sn: SnailfishNum): Long = sn match {
    case Lit(v) => v.toLong
    case Pair(l,r) => val (magL, magR) = (magnitude(l), magnitude(r))
      val (bestL, bestR) = (bestMagnitude(l), bestMagnitude(r))
      Seq(bestL, bestR, magL*3 + magR*2, magR*3 + magL*2).max
  }

  //Can never be nested more than 4x, since we always perform reduction right after addition


  override def solvePart1(inp: List[String]): Any = {
    //Parse each number on a line
    //y2021.A number can either be a Lit, or it can be a Pair
    val sns = inp.map(x => parseSN(x)._1)
    magnitude(sns.tail.foldLeft(sns.head)((sn1,sn2) => reduce(add(sn1,sn2))))
  }

  override def solvePart2(inp: List[String]): Any = {
    //Create all pairs of snailfish numbers
    val sns = inp.map(x => parseSN(x)._1)
    val snPairs = for(a <- sns;  b <- sns) yield (a,b)
    snPairs.map{case (a,b) => Math.max(magnitude(reduce(add(a,b))), magnitude(reduce(add(b,a))))}.max
  }
}

object Day18 extends App {

  Solution.solve2021(new Day18, 18, 4140, 3993)
}