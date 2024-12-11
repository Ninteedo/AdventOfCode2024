package days

import utility.*

class Day11 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val stones = StoneLine.fromString(input)
    (part1(stones), part2(stones))
  }

  private def part1(stones: StoneLine): Long = stones.countAfter(25)

  private def part2(stones: StoneLine): Long = stones.countAfter(75)

  private case class StoneLine(stones: List[Stone]) {
    def countAfter(n: Int): Long = {
      stones.map(_.countAfter(n)).sum
    }
  }

  private object StoneLine {
    def fromString(s: String): StoneLine = StoneLine(s.trim.split(" ").map(v => Stone.create(v.toLong)).toList)
  }

  private case class Stone(value: Long) {
    private lazy val evolve: List[Long] = {
      if value == 0L then List(1L)
      else {
        val s = value.toString
        if (s.length % 2 == 0) List(s.take(s.length / 2).toLong, s.drop(s.length / 2).toLong)
        else List(value * 2024L)
      }
    }

    /** tuple of (time to split, (split-left, split-right)), memoized */
    private lazy val toSplit: (Int, (Stone, Stone)) = {
      if (evolve.length == 2) {
        (1, (Stone.create(evolve.head), Stone.create(evolve.last)))
      } else {
        val child = Stone.create(evolve.head)
        (1 + child.toSplit._1, child.toSplit._2)
      }
    }

    def countAfter(n: Int): Long = countAfterMemo.getOrElseUpdate(n, {
      val t = toSplit._1
      if n < t then 1
      else toSplit._2._1.countAfter(n - t) + toSplit._2._2.countAfter(n - t)
    })

    private val countAfterMemo: collection.mutable.Map[Int, Long] = collection.mutable.Map.empty
  }

  private object Stone {
    def create(v: Long): Stone = stoneMap.getOrElseUpdate(v, Stone(v))

    private val stoneMap: collection.mutable.Map[Long, Stone] = collection.mutable.Map.empty
  }
}
