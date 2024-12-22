package days

import utility.*

class Day18 extends IDay {
  private val GRID_SIZE = 70
  private val START = 1024

  override def execute(input: String): (Any, Any) = {
    val bytePositions = Helper.readLines(input, line => {
      val splits = line.trim.split(",")
      Point2D(splits(0).toInt, splits(1).toInt)
    }).toList
    (part1(bytePositions), part2(bytePositions))
  }

  private def part1(blockers: List[Point2D]): Int = {
    val fallenBytes = blockers.take(START)
    findPath(fallenBytes).get.size
  }

  private def part2(blockers: List[Point2D]): String = {
    var i = START
    var lastPath = findPath(blockers.take(START)).get
    while (i < blockers.length) {
      if (lastPath.contains(blockers(i))) {
        val path = findPath(blockers.take(i + 1))
        if (path.isEmpty) return f"${blockers(i).x},${blockers(i).y}"
        lastPath = path.get
      }
      i += 1
    }

    throw new Exception("No solution found")
  }

  private def findPath(blockers: List[Point2D]): Option[Set[Point2D]] = {
    val blockersSet = blockers.toSet
    val goal = Point2D(GRID_SIZE, GRID_SIZE)

    val frontier = collection.mutable.PriorityQueue((0, Point2D(0, 0), List.empty[Point2D]))(Ordering.by(
      (s: Int, p: Point2D, _) => p.mannDist(goal) - s))
    val visited = collection.mutable.Set.empty[Point2D]

    while (frontier.nonEmpty) {
      val (steps, current, path) = frontier.dequeue()
      if (current == goal) return Some(path.toSet)

      if (!visited.contains(current)) {
        visited.add(current)

        for (dir <- Direction.values) {
          val next = current + dir
          if (next.x >= 0 && next.x <= GRID_SIZE && next.y >= 0 && next.y <= GRID_SIZE
            && !blockersSet.contains(next) && !visited.contains(next)) {
            frontier.enqueue((steps + 1, next, path :+ next))
          }
        }
      }
    }

    None
  }
}

// not 9,10
