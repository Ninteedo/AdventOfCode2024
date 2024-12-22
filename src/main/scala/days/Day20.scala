package days

import utility.*

class Day20 extends IDay {
  private val MIN_CHEAT_SAVE = 100

  override def execute(input: String): (Any, Any) = {
    val raceTrack = Grid2D.from2DCharArray(input, MapTile.fromChar)
    val path = findPath(raceTrack)
    (part1(path), part2(path.toArray))
  }

  private def part1(path: List[Point2D]): Int = {
    var savings = 0
    val pathMap = path.zipWithIndex.toMap

    for (i <- 0 until path.length - MIN_CHEAT_SAVE - 2) {
      for (dir <- Direction.values) {
        val cutEnd = path(i) + dir + dir
        val j = pathMap.get(cutEnd)
        if (j.isDefined && j.get - i - 2 >= MIN_CHEAT_SAVE) {
          savings += 1
        }
      }
    }

    savings
  }

  private def part2(path: Array[Point2D]): Int = {
    var savings = 0

    for (i <- 0 until path.length - MIN_CHEAT_SAVE) {
      for (j <- i + MIN_CHEAT_SAVE until path.length) {
        val baseDist = j - i
        val cheatDist = path(i).mannDist(path(j))
        if (baseDist - cheatDist >= MIN_CHEAT_SAVE && cheatDist <= 20) {
          savings += 1
        }
      }
    }

    savings
  }

  private def findPath(raceTrack: Grid2D[MapTile]): List[Point2D] = {
    val start = raceTrack.indexWhere(_ == MapTile.Start).get
    val end = raceTrack.indexWhere(_ == MapTile.End).get

    val visited = collection.mutable.Set.empty[Point2D]
    val queue = collection.mutable.Queue((start, List(start)))

    while (queue.nonEmpty) {
      val (curr, path) = queue.dequeue()
      if (curr == end) return path
      visited.add(curr)
      for (dir <- Direction.values) {
        val next = curr + dir
        if (!visited.contains(next) && raceTrack.at(next) != MapTile.Wall) {
          queue.enqueue((next, next :: path))
        }
      }
    }

    throw new Exception("No path found")
  }

  private enum MapTile {
    case Wall, Empty, Start, End
  }

  private object MapTile {
    def fromChar: Char => MapTile = {
      case '#' => Wall
      case '.' => Empty
      case 'S' => Start
      case 'E' => End
    }
  }
}
