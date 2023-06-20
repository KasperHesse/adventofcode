class Day16 extends Solution {

  abstract class Packet(val version: Int, val typeID: Int)
  case class LitPacket(override val version: Int, override val typeID: Int, val v: BigInt) extends Packet(version, typeID)
  case class OpPacket(override val version: Int, override val typeID: Int, val pkts: List[Packet]) extends Packet(version, typeID)

  /**
   * Parse a packet from the binary string
   * @param s The binary string. First char should be first entry in a version number
   * @return The parsed packet, and remaining string after parsing
   */
  def parsePacket(s: String): (Packet, String) = {
    s.substring(3,6) match {
      case "100" => parseLitPacket(s)
      case _     => parseOpPacket(s)
    }
  }
  /**
   * Parse a literal packet
   * @param s The string containing the packet. The first character should be the first char of the version
   * @return The packet, and the number of characters parsed
   */
  def parseLitPacket(s: String): (LitPacket, String) = {
    /**
     * Parse the literal value stored in a lit packet
     * @param idx Index in string that contains the next 5 bits
     * @param acc Accumulating value that has been parsed so far
     * @param len Length of parsed string (num bits parsed)
     * @return
     */
    def parseLitVal(idx: Int, acc: BigInt, len: Int): (BigInt, Int) = {
      val end = s.charAt(idx) == '0'
      val byte = Integer.parseInt(s.substring(idx+1, idx+5), 2)
      val v = (acc << 4) | byte
      if (end) {
        (v, len+5)
      } else {
        parseLitVal(idx+5, v, len+5)
      }
    }
    val version = Integer.parseInt(s.substring(0, 3), 2)
    val typeID = Integer.parseInt(s.substring(3, 6), 2)
    val (v, len) = parseLitVal(6, 0, 0)

    (LitPacket(version, typeID, v), s.substring(len+6))
  }

  def parseOpPacket(s: String): (OpPacket, String) = {
    /**
     * Parse the next N bits as the subpackets of an operation packet
     * @param s Remaining string to parse
     * @param rem Number of bits (chars) remaining
     * @param acc Accumulating parameter, packets parsed so far
     * @return All packets parsed, remaining substring after parsing
     */
    def parseNextNbits(s: String, rem: Int, acc: List[Packet]): (List[Packet], String) = {
      val (pkt, sub) = parsePacket(s)
      val newRem = rem - (s.length - sub.length)
      if (newRem == 0) {
        (pkt::acc, sub)
      } else {
        parseNextNbits(sub, newRem, pkt::acc)
      }
    }

    /**
     * Parse the remaining string until N subpackets of an operation packet have been parsed
     * @param s Remaining string to parse
     * @param rem Number of packets remaining
     * @param acc Accumulating parameter, packets parsed so far
     * @return All packets parsed, remaining substring after parsing
     */
    def parseNextNpkts(s: String, rem: Int, acc: List[Packet]): (List[Packet], String) = {
      val (pkt, sub) = parsePacket(s)
      if (rem-1 == 0) {
        (pkt::acc, sub)
      } else {
        parseNextNpkts(sub, rem-1, pkt::acc)
      }
    }
    val version = Integer.parseInt(s.substring(0, 3), 2)
    val typeID = Integer.parseInt(s.substring(3, 6), 2)
    val (pkts, sub) = if (s.charAt(6) == '0') { //Next 15 bits encode length of nested packets
      val lenPkts = Integer.parseInt(s.substring(7, 7+15),2)
      parseNextNbits(s.substring(7+15), lenPkts, List.empty)
    } else { //Next 11 bits encode number of nested packets
      val numPkts = Integer.parseInt(s.substring(7, 7+11), 2)
      parseNextNpkts(s.substring(7+11), numPkts, List.empty)
    }
    (OpPacket(version, typeID, pkts.reverse), sub) //Must reverse packets such that >, < and == OP packets work correctly
  }

  def evalPacket(pkt: Packet): BigInt = {
    pkt match {
      case LitPacket(_,_,v) => v
      case OpPacket(_,id,pkts) => val evald = pkts.map(evalPacket)
                                  id match {
                                    case 0 => evald.sum
                                    case 1 => evald.product
                                    case 2 => evald.min
                                    case 3 => evald.max
                                    case 5 => if (evald.head > evald.last) 1 else 0
                                    case 6 => if (evald.head < evald.last) 1 else 0
                                    case 7 => if (evald.head == evald.last) 1 else 0
                                  }
    }
  }

  def BITSToBin(s: String): Packet = {
    val bi = BigInt(s, 16).toString(2)
    val remLen = s.length*4 - bi.length
    val fnl = ("0" * remLen) ++ bi
    parsePacket(fnl)._1
  }

  override def solvePart1(inp: List[String]): Any = {
    def computeVersionSum(pkts: List[Packet], sum: Int): Int = {
      pkts match {
        case LitPacket(v,_,_)::tail => computeVersionSum(tail, sum+v)
        case OpPacket(v,_,pkts)::tail => val s = computeVersionSum(pkts, 0) + v
                                         computeVersionSum(tail, sum + s)
        case Nil => sum
      }
    }
    //Map each test packet to its version sum
    val pkts = inp.map(BITSToBin)
    pkts.map(pkt => computeVersionSum(List(pkt), 0))
  }

  override def solvePart2(inp: List[String]): Any = {
    val pkts = inp.map(BITSToBin)
    pkts.map(evalPacket)
  }
}

object Day16 extends App {
  Solution.solve(new Day16, 16,
    List(16, 12, 23, 31, 14, 8, 15, 11, 13, 19, 16, 20), //first 4: Version nums from 4 examples for part 1. Last: Computed version nums for examples from part 2
    List(15, 46, 46, 54, 3, 54, 7, 9, 1, 0, 0, 1)) //first 4: Computed values for examples from part 1: Last: Given results for examples from part 2
}