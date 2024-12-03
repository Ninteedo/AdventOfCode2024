package days

import utility.*

class Day03 extends IDay {
  override def execute(input: String): (Any, Any) = {
    (part1(input), part2(input))
  }

  private def part1(input: String): Int = {
    val pattern = "mul\\((\\d+),(\\d+)\\)".r
    val matches = pattern.findAllMatchIn(input).toList
    matches.map(m => m.group(1).toInt * m.group(2).toInt).sum
  }

  private def part2(input: String): Int = {
    val pattern = "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)".r
    val matches = pattern.findAllMatchIn(input).toList
    matches.foldLeft((false, 0)) { case ((isDisabled, sum), m) =>
      (isDisabled, m.group(0)) match {
        case (_, "do()") => (false, sum)
        case (_, "don't()") => (true, sum)
        case (false, _) => (false, sum + m.group(1).toInt * m.group(2).toInt)
        case _ => (isDisabled, sum)
      }
    }._2
  }
}
