package days

import utility.*

class Day07 extends IDay {
  private type Equation = (Long, List[Long])

  override def execute(input: String): (Any, Any) = {
    val equations = Helper.readLines(input, parseEquation).toList
    (part1(equations), part2(equations))
  }

  private def part1(equations: List[Equation]) = calibrationResult(equations, enableConcat = false)

  private def part2(equations: List[Equation]) = calibrationResult(equations, enableConcat = true)

  private def parseEquation(line: String): Equation = {
    val parts = line.split(":")
    (parts(0).toLong, parts(1).trim.split(" ").map(_.toLong).toList)
  }

  private def calibrationResult(equations: List[Equation], enableConcat: Boolean): Long = {
    equations.filter(testEquation(enableConcat)).map(_._1).sum
  }

  private def testEquation(enableConcat: Boolean)(equation: Equation): Boolean = {
    val target = equation._1

    def rec(current: Long, remaining: List[Long]): Boolean = {
      if (remaining.isEmpty) {
        current == target
      } else {
        rec(current + remaining.head, remaining.tail) ||
          rec(current * remaining.head, remaining.tail) ||
          (enableConcat && rec(concatNums(current, remaining.head), remaining.tail))
      }
    }

    rec(equation._2.head, equation._2.tail)
  }

  private def concatNums(a: Long, b: Long): Long = {
    val bLength = b.toString.length
    a * Math.pow(10, bLength).toLong + b
  }
}
