package days

import utility.{Helper, IDay}

class Day02 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val reports: List[List[Int]] = Helper.readLines(input, s => s.split(" ").map(_.toInt).toList).toList
    (part1(reports), part2(reports))
  }

  private def part1(reports: List[List[Int]]): Int = {
    reports.count(report => isSafe(report, 1) || isSafe(report, -1))
  }

  private def part2(reports: List[List[Int]]): Int = {
    def isSafeDampened(report: List[Int], direction: Int): Boolean = report.indices.exists(i => {
      val reportExceptI = report.take(i) ++ report.drop(i + 1)
      isSafe(reportExceptI, direction)
    })

    reports.count(report => isSafeDampened(report, 1) || isSafeDampened(report, -1))
  }

  private def isSafe(report: List[Int], direction: Int): Boolean = report.zip(report.tail).forall(p => {
    val (a, b) = (p._1, p._2)
    ((direction < 0 && a > b) || (direction > 0 && a < b)) && (Math.abs(a - b) <= 3)
  })
}
