package days

import utility.*

import scala.collection.parallel.CollectionConverters.*

class Day22 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val initialSecrets = Helper.readLines(input, _.toLong)
    val monkeys = initialSecrets.map(MonkeyBuyer)
    (part1(monkeys), part2(monkeys))
  }

  private def part1(monkeys: Iterable[MonkeyBuyer]): Long = {
    monkeys.map(_.sequence.last).sum
  }

  private def part2(monkeys: Iterable[MonkeyBuyer]): Long = {
    val range = Range.inclusive(-9, 9)
    val changes = range.flatMap(a => range.flatMap(b => range.flatMap(c => range.map(d => (a, b, c, d)))))
    changes.par.map(change => monkeys.flatMap(_.buy(change)).sum).max
  }

  private def evolveSecret(a: Long): Long = {
    val b = prune(mix(a * 64, a))
    val c = prune(mix(b / 32, b))
    val d = prune(mix(c * 2048, c))
    d
  }

  private def mix(a: Long, b: Long): Long = a ^ b

  private def prune(a: Long): Long = a & 0xFFFFFF

  private case class MonkeyBuyer(initialSecret: Long) {
    val sequence: List[Long] = LazyList
      .iterate(initialSecret)(evolveSecret)
      .take(2001)
      .toList

    private val buys: Array[Array[Array[Array[Option[Long]]]]] = {
      val buys = Array.fill(19, 19, 19, 19)(None: Option[Long])
      sequence
        .sliding(5)
        .foreach(s => {
          val ones = s.map(_ % 10)
          val changes = ones.zip(ones.tail).map { case (a, b) => (b - a).toInt }
          val k = (changes(0), changes(1), changes(2), changes(3))
          if (buys(k._1 + 9)(k._2 + 9)(k._3 + 9)(k._4 + 9).isEmpty) {
            buys(k._1 + 9)(k._2 + 9)(k._3 + 9)(k._4 + 9) = Some(ones.last)
          }
        }
        )
      buys
    }

    def buy(s: (Int, Int, Int, Int)): Option[Long] = buys(s._1 + 9)(s._2 + 9)(s._3 + 9)(s._4 + 9)
  }
}
