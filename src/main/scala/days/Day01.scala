package days

import utility.{Helper, IDay}

class Day01 extends IDay {

  override def execute(input: String): (Any, Any) = {
    val pairs: List[(Int, Int)] = Helper.readLines(input, s => Helper.splitPair(s, "\\s+"))
      .map(p => (p._1.strip.toInt, p._2.strip.toInt))
      .toList
    val list1: List[Int] = pairs.map(_._1)
    val list2: List[Int] = pairs.map(_._2)
    (part1(list1, list2), part2(list1, list2))
  }

  private def part1(list1: List[Int], list2: List[Int]): Int =
    list1.sorted.zip(list2.sorted).map(p => Math.abs(p._2 - p._1)).sum

  private def part2(list1: List[Int], list2: List[Int]): Int =
    list1.map(x => (x, list2.count(_ == x))).map(p => p._1 * p._2).sum
}
