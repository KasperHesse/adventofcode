package y2021

import common.Solution

class Day14 extends Solution {

  def parseRules(inp: List[String]): Map[(Char, Char), Char] = {
    inp.map(_.split(" -> "))
      .foldLeft(Map.empty[(Char,Char), Char]){case (m,a) => m.updated((a(0)(0), a(0)(1)), a(1)(0))}
  }

  /**
   * Apply the rules in the polymer template, generating a new string as a result
   * @param rules
   * @param str
   * @return
   */
  def doPolymerTemplate(rules: Map[(Char,Char), Char], str: String): String = {
    (str zip str.tail).map(x => s"${x._1}${rules(x)}").mkString("","","") + str.takeRight(1) //Append final character
  }

  override def solvePart1(inp: List[String]): Any = {
    val rules = parseRules(inp.drop(2))
    val counts = getCharCounts(inp.head, rules, 10)
    counts.values.max - counts.values.min
    //Original string-manipulating solution

    //    val res = Range(0,10).foldLeft(inp.head)((s,_) => doPolymerTemplate(rules, s))
    //    //Get all distinct letters, then count each of them
    //    val letters = res.distinct
    //    val counts = letters.map(l => res.count(_==l))
  }

  def countPairs(rules: Map[(Char,Char),Char], charCounts: Map[Char,Long], pairCounts: Map[(Char,Char), Long]): (Map[Char, Long], Map[(Char, Char), Long]) = {
    //for each entry in counts
      //decrement count of that entry with the number currently set
      //increment all others. Increment in new map, read from original map
    pairCounts.foldLeft((charCounts, pairCounts)){case ((cc,pc), ((a,b),c)) =>
      val x = rules((a,b))
      val pc2 = pc.updated((a,b), pc((a,b))-c)
        .updatedWith((a,x)){case Some(v) => Some(v+c); case None => Some(c)}
        .updatedWith((x,b)){case Some(v) => Some(v+c); case None => Some(c)}
      val cc2 = cc.updatedWith(x){case Some(v) => Some(v+c); case None => Some(c)}
      (cc2, pc2)
    }
  }

  def getCharCounts(template: String, rules: Map[(Char,Char), Char], rounds: Int): Map[Char, Long] = {
    //Initialize char-counts
    val charCount = template.distinct.foldLeft(Map.empty[Char,Long])((m,c) => m.updated(c, template.count(_==c)))
    val pairCount = (template zip template.tail).foldLeft(Map.empty[(Char,Char),Long])(
      (m, p) => m.updatedWith(p){case Some(v) => Some(v+1); case None => Some(1)}
    )
    val res = Range(0,rounds).foldLeft((charCount, pairCount)){case ((cc,pc),_) => countPairs(rules, cc, pc)}
    res._1
  }

  override def solvePart2(inp: List[String]): Any = {
    //String will be way too long. We need a way of reducing/compressing the string, and then de-compressing it at runtime
    //Observation: Each pair destructs itself and generates two new pairs + inserts one character into the string
    //On each iteration, track how many of each pair exist. Remove those pairs, create new pairs and update char counters
    val rules = parseRules(inp.drop(2))
    val template = inp.head
    val counts = getCharCounts(template, rules, 40)
    counts.values.max - counts.values.min
  }
}

object Day14 extends App {
  Solution.solve(new Day14, 14, 1588, 2188189693529L)
}