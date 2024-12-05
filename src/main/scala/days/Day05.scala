package days

import utility.*

class Day05 extends IDay {
  private type Update = List[Int]
  private type PageOrdering = Ordering[Int]

  override def execute(input: String): (Any, Any) = {
    val (orderingRulesString, updatesString) = input.split("\n\n").toList match {
      case List(a, b) => (a, b)
      case _ => throw new Exception("Invalid input")
    }

    def parseOrderingRule(s: String): (Int, Int) = s.split("\\|").toList match {
      case List(a, b) => (a.toInt, b.toInt)
      case _ => throw new Exception(s"Invalid input for ordering rule: $s")
    }

    def parseUpdate(s: String): Update = s.split(",").toList.map(_.toInt)

    val orderingRules = Helper.readLines(orderingRulesString, parseOrderingRule).toList
    val ordering = orderingRuleOrdering(orderingRulesToMap(orderingRules))
    val updates = Helper.readLines(updatesString, parseUpdate).toList
    (part1(ordering, updates), part2(ordering, updates))
  }

  private def part1(ordering: PageOrdering, updates: List[Update]): Int = {
    updates
      .filter(isOrdered(ordering))
      .map(getMiddleElement)
      .sum
  }

  private def part2(ordering: PageOrdering, updates: List[Update]): Int = {
    updates
      .filterNot(isOrdered(ordering))
      .map(_.sorted(ordering))
      .map(getMiddleElement)
      .sum
  }

  private def getMiddleElement(update: Update): Int = update((update.length - 1) / 2)

  private def isOrdered(ordering: PageOrdering)(update: Update): Boolean = update.sorted(ordering) == update

  private def orderingRulesToMap(orderingRules: List[(Int, Int)]): Map[Int, Set[Int]] = {
    orderingRules.sorted.map(_._1).toSet.map(l =>
      l -> orderingRules.filter(_._1 == l).map(_._2).toSet
    ).toMap
  }

  private def orderingRuleOrdering(orderingRuleMap: Map[Int, Set[Int]]): PageOrdering = Ordering[Int] { (x, y) =>
    if (orderingRuleMap.getOrElse(x, Set()).contains(y)) -1
    else if (orderingRuleMap.getOrElse(y, Set()).contains(x)) 1
    else 0
  }
}
