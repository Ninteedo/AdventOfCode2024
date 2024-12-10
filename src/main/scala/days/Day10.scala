package days

import utility.*

class Day10 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val grid = Grid2D.from2DCharArray(input, _.toString.toInt)
    (part1(grid), part2(grid))
  }

  private def part1(grid: Grid2D[Int]): Int = {
    def reachablePeaks(start: Point2D): Int = {
      def rec(curr: Point2D): Set[Point2D] = {
        if (grid.at(curr) == 9) {
          Set(curr)
        } else {
          Direction.values.toSet.flatMap(dir => {
            val next = curr + dir
            if (grid.contains(next) && grid.at(curr) == grid.at(next) - 1) {
              rec(next)
            } else {
              Set.empty
            }
          })
        }
      }

      rec(start).size
    }

    grid.indicesWhere(_ == 0).map(reachablePeaks).sum
  }

  private def part2(grid: Grid2D[Int]): Int = {
    def reachablePeaks(start: Point2D): Int = {
      def rec(curr: Point2D): Int = {
        if (grid.at(curr) == 9) {
          1
        } else {
          Direction.values.map(dir => {
            val next = curr + dir
            if (grid.contains(next) && grid.at(curr) == grid.at(next) - 1) {
              rec(next)
            } else {
              0
            }
          }).sum
        }
      }

      rec(start)
    }

    grid.indicesWhere(_ == 0).map(reachablePeaks).sum
  }
}
