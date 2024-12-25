package days

import utility.*

class Day25 extends IDay {
  private val MAX_PIN_HEIGHT = 5

  override def execute(input: String): (Any, Any) = {
    val schematics = input.split("\n\n")
    val locks = schematics.filter(isLock).map(Lock.fromString).toList
    val keys = schematics.filterNot(isLock).map(Key.fromString).toList
    (part1(locks, keys), part2())
  }

  private def part1(locks: List[Lock], keys: List[Key]): Int = {
    locks.map(lock => keys.count(_.canUnlock(lock))).sum
  }

  private def part2() = "Merry Christmas!"

  private case class Lock(pinHeights: List[Int])

  private object Lock {
    def fromString(s: String): Lock = Lock(readPins(s))
  }

  private case class Key(pinHeights: List[Int]) {
    def canUnlock(lock: Lock): Boolean = pinHeights.zip(lock.pinHeights).forall {
      case (keyHeight, lockHeight) => keyHeight + lockHeight <= MAX_PIN_HEIGHT
    }
  }

  private object Key {
    def fromString(s: String): Key = Key(readPins(s))
  }

  private def readPins(s: String): List[Int] = {
    val grid = Grid2D.from2DCharArray(s, _ == '#')
    grid.entriesByColumn.map(_.count(identity) - 1).toList
  }

  private def isLock(s: String): Boolean = s.split("\n").head.forall(_ == '#')
}
