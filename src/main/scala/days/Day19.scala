package days

import utility.*

class Day19 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val patterns: List[List[Colour]] = input
      .takeWhile(_ != '\n')
      .split(", ")
      .map(_.toCharArray.map(Colour.fromChar).toList)
      .toList
    val designs: List[List[Colour]] = input
      .dropWhile(_ != '\n')
      .trim
      .split("\n")
      .map(_.toCharArray.map(Colour.fromChar).toList)
      .toList
    (part1(patterns, designs), part2(patterns, designs))
  }

  private def part1(patterns: List[List[Colour]], designs: List[List[Colour]]): Int = {
    def designPossible(design: List[Colour]): Boolean = {
      val visitedLengths = collection.mutable.Set.empty[Int]

      def rec(n: Int): Boolean = {
        !visitedLengths.contains(n) && {
          visitedLengths.add(n)
          (n == design.length) || patterns.exists(pattern =>
            design.drop(n).startsWith(pattern) && rec(n + pattern.length)
          )
        }
      }

      rec(0)
    }

    designs.count(designPossible)
  }

  private def part2(patterns: List[List[Colour]], designs: List[List[Colour]]): Long = {
    def designPossibilities(design: List[Colour]): Long = {
      val possibilitiesAtLength = collection.mutable.Map.empty[Int, Long]

      def rec(n: Int): Long = {
        possibilitiesAtLength.getOrElseUpdate(n, if (n == design.length) 1 else {
          patterns.map(pattern =>
            if (design.drop(n).startsWith(pattern)) rec(n + pattern.length) else 0
          ).sum
        })
      }

      rec(0)
    }

    designs.map(designPossibilities).sum
  }

  private enum Colour {
    private case White, Blue, Black, Red, Green
  }

  private object Colour {
    def fromChar: Char => Colour = {
      case 'w' => White
      case 'u' => Blue
      case 'b' => Black
      case 'r' => Red
      case 'g' => Green
    }
  }
}
