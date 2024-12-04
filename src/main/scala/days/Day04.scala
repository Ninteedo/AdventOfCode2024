package days

import utility.*

class Day04 extends IDay {
  private type WordSearch = Grid2D[Char]

  private val WORD: String = "MAS"

  override def execute(input: String): (Any, Any) = {
    val wordSearch = Grid2D.from2DCharArray(input, identity)
    (part1(wordSearch), part2(wordSearch))
  }

  private def part1(wordSearch: WordSearch): Int = {
    val directions = Range(-1, 2).flatMap(dy => Range(-1, 2).map(dx => Point2D(dx, dy))).filter(_ != Point2D(0, 0))

    def checkPos(pos: Point2D): Int = wordSearch.at(pos) match {
      case 'X' => directions.count(checkInDirection(wordSearch, pos, +1))
      case _ => 0
    }

    wordSearch.indices.map(checkPos).sum
  }

  private def part2(wordSearch: WordSearch): Int = {
    val directions = List(1, -1).flatMap(dy => List(1, -1).map(dx => Point2D(dx, dy)))

    def checkPos(pos: Point2D): Boolean =
      wordSearch.at(pos) == 'A' && directions.count(checkInDirection(wordSearch, pos, -1)) == 2

    wordSearch.indices.count(checkPos)
  }

  private def checkInDirection(wordSearch: WordSearch, pos: Point2D, posOffset: Int)(dir: Point2D): Boolean = {
    WORD.indices.forall { i =>
      val newPos = pos + dir * (i + posOffset)
      wordSearch.contains(newPos) && wordSearch.at(newPos) == WORD(i)
    }
  }
}
