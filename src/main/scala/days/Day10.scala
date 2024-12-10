package days

import utility.*

import scala.reflect.ClassTag

class Day10 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val grid = Grid2D.from2DCharArray(input, _.toString.toInt)
    (part1(grid), part2(grid))
  }

  private def part1(grid: Grid2D[Int]): Int = starts(grid).map(reachablePeaks(grid)).map(_.size).sum

  private def part2(grid: Grid2D[Int]): Int = starts(grid).map(distinctTrails(grid)).sum

  private def reachablePeaks(grid: Grid2D[Int]): Point2D => Set[Point2D] = {
    findTrailheads(grid, peak = x => Set(x), deadEnd = Set.empty, collect = {_.flatten.toSet})
  }

  private def distinctTrails(grid: Grid2D[Int]): Point2D => Int = {
    findTrailheads(grid, peak = _ => 1, deadEnd = 0, collect = {_.sum})
  }

  private def starts(grid: Grid2D[Int]): Iterable[Point2D] = grid.indicesWhere(_ == 0)

  private def findTrailheads[T: ClassTag](grid: Grid2D[Int], peak: Point2D => T, deadEnd: => T, collect: Array[T] => T)(start: Point2D): T = {
    def rec(curr: Point2D): T = {
      if (grid.at(curr) == 9) {
        peak(curr)
      } else {
        collect(Direction.values.map(dir => {
          val next = curr + dir
          if (grid.contains(next) && grid.at(curr) == grid.at(next) - 1) {
            rec(next)
          } else {
            deadEnd
          }
        }))
      }
    }

    rec(start)
  }
}
