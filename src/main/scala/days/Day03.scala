package days

import utility.*

import scala.util.matching.Regex.Match

class Day03 extends IDay {
  override def execute(input: String): (Any, Any) = {
    (part1(input), part2(input))
  }

  private def part1(input: String): Int = {
    val pattern = "mul\\((\\d+),(\\d+)\\)".r
    val matches = pattern.findAllMatchIn(input)
    matches.map(mul).sum
  }

  private def part2(input: String): Int = {
    val pattern = "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)".r
    val matches = pattern.findAllMatchIn(input)
    matches.foldLeft((false, 0)) { case ((isDisabled, sum), m) =>
      (isDisabled, m.group(0)) match {
        case (_, "do()")    => (false, sum)
        case (_, "don't()") => (true,  sum)
        case (true, _)      => (true,  sum)
        case (false, _)     => (false, sum + mul(m))
      }
    }._2
  }

  private def mul(m: Match): Int = m.group(1).toInt * m.group(2).toInt
}
