package days

import utility.{Helper, IDay}

class Day02 extends IDay {
  private type Report = List[Int]

  override def execute(input: String): (Any, Any) = {
    val reports = Helper.readLines(input, s => s.split(" ").map(_.toInt).toList).toList
    (part1(reports), part2(reports))
  }

  private def part1(reports: List[Report]): Int = reports.count(isSafe)

  private def part2(reports: List[Report]): Int = reports.count(isSafeDampened)

  private def isSafe(report: Report): Boolean = {
    val directions = List(1, -1)
    directions.exists(direction =>
      report.zip(report.tail)
        .forall((a, b) =>
          ((direction < 0 && a > b) || (direction > 0 && a < b)) &&
          (Math.abs(a - b) <= 3)
      )
    )
  }

  private def isSafeDampened(report: Report): Boolean =
    isSafe(report) || report.indices.exists(i => isSafe(report.take(i) ++ report.drop(i + 1)))
}
