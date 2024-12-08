package days

import utility.*

class Day08 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val antennaGrid = Grid2D.from2DCharArray(input, identity)
    val antennas = antennaGrid.entries
      .zipWithIndex
      .flatMap((row, y) => row
        .zipWithIndex
        .filter(_._1 != '.')
        .map((c, x) => Antenna(c, Point2D(x, y)))
      )
      .toList
    val boundX = antennaGrid.entries.head.length
    val boundY = antennaGrid.entries.length
    (part1(antennas, boundX, boundY), part2(antennas, boundX, boundY))
  }

  private def part1(antennas: List[Antenna], boundX: Int, boundY: Int): Int = {
    countAntinodes(antennas, boundX, boundY, (a, b) => a.antinodesWithPart1(b))
  }

  private def part2(antennas: List[Antenna], boundX: Int, boundY: Int) = {
    countAntinodes(antennas, boundX, boundY, (a, b) => a.antinodesWithPart2(b, boundX, boundY))
  }

  private def countAntinodes(antennas: List[Antenna], boundX: Int, boundY: Int, f: (Antenna, Antenna) => Set[Point2D]): Int = {
    antennas
      .combinations(2)
      .flatMap(pair => f(pair(0), pair(1)))
      .filter(checkBounds(boundX, boundY))
      .toSet
      .size
  }

  private def checkBounds(boundX: Int, boundY: Int)(pos: Point2D): Boolean = {
    pos.x >= 0 && pos.x < boundX && pos.y >= 0 && pos.y < boundY
  }

  private case class Antenna(frequency: Char, pos: Point2D) {
    def antinodesWithPart1(other: Antenna): Set[Point2D] = {
      if (this.frequency != other.frequency) {
        Set.empty
      } else {
        val diff = other.pos - this.pos
        Set(
          this.pos - diff,
          other.pos + diff
        )
      }
    }

    def antinodesWithPart2(other: Antenna, boundX: Int, boundY: Int): Set[Point2D] = {
      if (this.frequency != other.frequency) {
        Set.empty
      } else {
        val diff = other.pos - this.pos
        val decreasing = LazyList.from(0).map(i => this.pos - (diff * i)).takeWhile(checkBounds(boundX, boundY))
        val increasing = LazyList.from(0).map(i => other.pos + (diff * i)).takeWhile(checkBounds(boundX, boundY))
        decreasing.toSet ++ increasing.toSet
      }
    }
  }
}
