case class ALU(regs: Map[Reg, Long])

sealed trait Reg
case class W() extends Reg
case class X() extends Reg
case class Y() extends Reg
case class Z() extends Reg

sealed trait Instr
case class Inp(r1: Reg) extends Instr
case class AddRR(r1: Reg, r2: Reg) extends Instr
case class AddRI(r1: Reg, imm: Long) extends Instr
case class MulRR(r1: Reg, r2: Reg) extends Instr
case class MulRI(r1: Reg, imm: Long) extends Instr
case class DivRR(r1: Reg, r2: Reg) extends Instr
case class DivRI(r1: Reg, imm: Long) extends Instr
case class ModRR(r1: Reg, r2: Reg) extends Instr
case class ModRI(r1: Reg, imm: Long) extends Instr
case class EqlRR(r1: Reg, r2: Reg) extends Instr
case class EqlRI(r1: Reg, imm: Long) extends Instr

/**
 * Notes for this solution
 * The program consists of 14 instruction bundles, each of which is 18 instructions long.
 * The bundles have two types. Those where the 5'th instruction is "div z 1" and those where it is "div z 26"
 * On the bundles where we have "div z 1", the final z-value will be based on the previous z value (z') multiplied by 26,
 * added by the input and some offset encoded in the 16'th instruction "add y IMM". Hence, the z-value after a "div z 1"-bundle is
 * Z = 26*z' + inp + delta
 *
 * In the "div z 26" bundles, these multiplications are reversed. On each reversal, a term in the value of z is "popped off", giving us
 * a correspondence between the 7 "div z 1" bundles and the 7 "div z 26" bundles.
 * The "div z 26" bundles must not modify the value of z when "add z y" is performed,
 * which means y must be 0. For this to be true, x must be set to zero. For this to be true, the value of x and w must be the same after adding a delta-value.
 * On each "div z 1"-bundle, the value of z is multiplied by 26 and (input + delta) is added. This means that z%26 = (input + delta), which is exactly x in a "div z 26"-bundle before another delta is added.
 * Hence, the x-value in a "div z 26"-bundle must be set such that the following equation is satisfied for the "div z 1"-bundle it is paired to
 * (input of d1-instruction) + v1 + v2 = (input of d26-instruction), where (v1) is delta of "div z 1"-bundle and (v2) is delta of "div z 26"-bundle

 */
class Day24 extends Solution {
  val strToReg: Map[String, Reg] = Map("x" -> X(), "y" -> Y(), "z" -> Z(), "w" -> W())

  def parseInstruction(instr: String): Instr = {
    val t = instr.split(" ")
    t(0) match {
      case "inp" => Inp(strToReg(t(1)))
      case "add" => if (strToReg.contains(t(2))) AddRR(strToReg(t(1)), strToReg(t(2))) else AddRI(strToReg(t(1)), t(2).toLong)
      case "mul" => if (strToReg.contains(t(2))) MulRR(strToReg(t(1)), strToReg(t(2))) else MulRI(strToReg(t(1)), t(2).toLong)
      case "div" => if (strToReg.contains(t(2))) DivRR(strToReg(t(1)), strToReg(t(2))) else DivRI(strToReg(t(1)), t(2).toLong)
      case "mod" => if (strToReg.contains(t(2))) ModRR(strToReg(t(1)), strToReg(t(2))) else ModRI(strToReg(t(1)), t(2).toLong)
      case "eql" => if (strToReg.contains(t(2))) EqlRR(strToReg(t(1)), strToReg(t(2))) else EqlRI(strToReg(t(1)), t(2).toLong)
      case _ => throw new IllegalArgumentException(s"Instruction $instr could not be parsed")
    }
  }

  def doInstr(alu: ALU, instr: Instr, inputs: List[Long]): (ALU, List[Long]) = {
    val r = alu.regs
    val res: Map[Reg, Long] = instr match {
      case Inp(r1)        => r.updated(r1, inputs.head)
      case AddRR(r1, r2)  => r.updated(r1, r(r1) + r(r2))
      case AddRI(r1, imm) => r.updated(r1, r(r1) + imm)
      case MulRR(r1, r2)  => r.updated(r1, r(r1) * r(r2))
      case MulRI(r1, imm) => r.updated(r1, r(r1) * imm)
      case DivRR(r1, r2)  => r.updated(r1, r(r1) / r(r2))
      case DivRI(r1, imm) => r.updated(r1, r(r1) / imm)
      case ModRR(r1, r2)  => r.updated(r1, r(r1) % r(r2))
      case ModRI(r1, imm) => r.updated(r1, r(r1) % imm)
      case EqlRR(r1, r2)  => r.updated(r1, if (r(r1) == r(r2)) 1 else 0)
      case EqlRI(r1, imm) => r.updated(r1, if (r(r1) == imm) 1 else 0)
    }
    if (instr.isInstanceOf[Inp]) (ALU(res), inputs.tail) else (ALU(res), inputs)
  }

  def process(alu: ALU, instrs: List[Instr], inputs: List[Long]): ALU = {
    instrs.foldLeft((alu, inputs)){case ((alu, inputs), instr) => doInstr(alu, instr, inputs)}._1
  }

  def pairBundles(bundles: List[List[Instr]], div1: List[(List[Instr], Int)], paired: List[((List[Instr], Int), (List[Instr], Int))], cnt: Int = 0): List[((List[Instr], Int), (List[Instr], Int))] = {
    bundles match {
      case head::tail => head(4) match {
        case DivRI(_,1) => pairBundles(tail, (head, cnt)::div1, paired, cnt+1)
        case DivRI(_,26) => pairBundles(tail, div1.tail, (div1.head, (head, cnt))::paired, cnt+1)
        case _ => throw new IllegalArgumentException(s"Instruction at (4) in bundles was not a DivRI. Instr=${head(4)}, bundle=$head")
      }
      case Nil => paired
    }
  }

  def getInstructionDiffs(instrs: List[Instr]): List[(Int, Int, Long)] = {
    val bundles = instrs.grouped(18).toList //Each instruction bundle is 18 instructions long
    //Pair the bundles into those where we perform a "divide by 1" and those where we do "divide by 26".
    //Always the 5th instruction in a bundle
    val paired = pairBundles(bundles, List(), List())
    //For the div1-instructions, we're interested in the literal value of the "add y IMM" instruction at position 16
    //For the div26-instructions, we're interested in the literal value of the "add x IMM" instruction at position 5
    //Finally, we're interested in the value that occurs when they are added together, as we wish to satisfy the equation
    // (input of d1-instruction) + v1 + v2 = (input of d26-instruction)
    paired.map{case (d1,d26) =>
      val v1 = d1._1(15).asInstanceOf[AddRI].imm;
      val v2 = d26._1(5).asInstanceOf[AddRI].imm
      (d1._2, d26._2, v1+v2)
    }
  }

  override def solvePart1(inp: List[String]): Any = {
    if (inp.length == 1) { //No test case
      return 1
    }
    val alu = ALU(Map(W() -> 0L, X() -> 0L, Y() -> 0L, Z() -> 0L))
    val instrs = inp.map(parseInstruction)
    val diffs = getInstructionDiffs(instrs)
    val digits: List[(Int, Long)] = diffs.flatMap{case (o1,o2,v) =>
      //If diff is negative, d1-value must be 9 and 26-value must be smaller
      // If diff is positive, d26-value is 9 and d1-value must be smaller
      if (v < 0) List((o1, 9), (o2, 9+v)) else List((o1, 9-v), (o2, 9))
    }
    val inputs = digits.sortBy(_._1).map(_._2)
    val res = process(alu, instrs, inputs)
    println(res.regs)
    inputs.mkString("","","")
  }

  override def solvePart2(inp: List[String]): Any = {
    if (inp.length == 1) { //No test case
      return 1
    }
    val alu = ALU(Map(W() -> 0L, X() -> 0L, Y() -> 0L, Z() -> 0L))
    val instrs = inp.map(parseInstruction)
    val diffs = getInstructionDiffs(instrs)
    diffs.foreach(println)
    val digits: List[(Int, Long)] = diffs.flatMap{case (o1,o2,v) =>
      //If diff is negative, d26-value must be 1 and d1-value must be abs(diff)+1
      //If diff is positive, d1-value must be 1 and d26-value must be abs(diff)+1
      if (v < 0) List((o1, v.abs+1), (o2, 1)) else List((o1, 1), (o2, v+1))
    }
    val inputs = digits.sortBy(_._1).map(_._2)
    val res = process(alu, instrs, inputs)
    println(res.regs)
    inputs.mkString("","","")
  }
}

object Day24 extends App {
  Solution.solve(new Day24, 24, 1, 1)
}