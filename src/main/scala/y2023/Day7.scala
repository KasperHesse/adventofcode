package y2023

import common._

class Day7 extends Solution {

  class Hand(val cards: List[Int], val rank: Int)
  case class FiveKind(override val cards: List[Int]) extends Hand(cards, 7)  //1
  case class FourKind(override val cards: List[Int]) extends Hand(cards, 6)  //2
  case class FullHouse(override val cards: List[Int]) extends Hand(cards, 5) //2
  case class ThreeKind(override val cards: List[Int]) extends Hand(cards, 4) //3
  case class TwoPairs(override val cards: List[Int]) extends Hand(cards, 3)  //3
  case class OnePair(override val cards: List[Int]) extends Hand(cards, 2)   //4
  case class HighCard(override val cards: List[Int]) extends Hand(cards, 1)  //5
  //END OTHER CODE

  implicit object HandOrdering extends Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      if (x.rank == y.rank) {
        //Hands have same rank: Compare the cards they have
        //Find first index where they differ, then return difference
        val idxDiff = x.cards.zip(y.cards).find(xy => xy._1 != xy._2)
        idxDiff.get._1 - idxDiff.get._2
      } else {
        x.rank - y.rank
      }
    }
  }

  def parseHand(in: String): Hand = {
    val cards = in.map {
      case x if x.isDigit => x.asDigit
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
      case x => throw new IllegalArgumentException(s"Could not parse card $x")
    }.toList

    //Define hand based on cardinality of set
    cards.toSet.size match {
      case 1 => FiveKind(cards)
      case 2 => if (cards.exists(c => cards.count(_==c) == 4)) FourKind(cards) else FullHouse(cards)
      case 3 => if (cards.exists(c => cards.count(_==c) == 3)) ThreeKind(cards) else TwoPairs(cards)
      case 4 => OnePair(cards)
      case 5 => HighCard(cards)
    }
  }

  /**
   * Convert a hand from the hand-types parsed for part 1 to a hand-type for part 2
   * @param h
   * @return
   */
  def convertHand(h: Hand): Hand = {
    val newCards = h.cards.map{case 11 => 1; case x => x}

    val numJokers = h.cards.count(_==11)
    (h, numJokers) match {
      case (_,0) => h
      case (_,5) => FiveKind(newCards)
      case (_: HighCard, 1) => OnePair(newCards)
      case (_: OnePair, 1) => ThreeKind(newCards)
      case (_: TwoPairs, 1) => FullHouse(newCards)
      case (_: ThreeKind, 1) => FourKind(newCards)
      case (_: FourKind, 1) => FiveKind(newCards)
      case (_: OnePair, 2) => ThreeKind(newCards)
      case (_: TwoPairs, 2) => FourKind(newCards)
      case (_: FullHouse, 2) => FiveKind(newCards)
      case (_: ThreeKind, 3) => FourKind(newCards)
      case (_: FullHouse, 3) => FiveKind(newCards)
      case (_: FourKind, 4) => FiveKind(newCards)
      case (_,_) => throw new IllegalArgumentException(s"ERROR: Unknown combination $h, $numJokers")
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    val hands = inp.map(_.split(" ")).map(s => (parseHand(s(0)), s(1).toInt))
    val handsSorted = hands.sortBy(_._1)
    handsSorted.zipWithIndex.map(x => x._1._2 * (x._2 + 1)).sum
  }

  override def solvePart2(inp: List[String]): Any = {
    val hands = inp.map(_.split(" ")).map(s => (parseHand(s(0)), s(1).toInt))
    val newHands = hands.map{case (h,v) => (convertHand(h), v)}
    val newHandsSorted = newHands.sortBy(_._1)
    newHandsSorted.zipWithIndex.map(x => x._1._2 * (x._2 + 1)).sum
  }
}

object Day7 extends App {
  Solution.solve(new Day7, 7, 2023, 6440, 5905)
}
