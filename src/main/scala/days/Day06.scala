package days

import utility.*

class Day06 extends IDay {
  private type GuardMap = Grid2D[MapSpace]

  override def execute(input: String): (Any, Any) = {
    val guardMap = Grid2D.from2DCharArray(input, getMapSpace)
    (part1(guardMap), part2(guardMap))
  }

  private def part1(guardMap: GuardMap) = {
    getGuardRoute(guardMap)._1.size
  }

  private def part2(guardMap: GuardMap) = {
    val routePositions = getGuardRoute(guardMap)._1
    val startPos = guardMap.indexWhere(_ == MapSpace.Start).get
    routePositions.excl(startPos).count(pos => {
      val newMap = Grid2D(guardMap.entries.updated(pos.y, guardMap.entries(pos.y).updated(pos.x, MapSpace.Wall)))
      !getGuardRoute(newMap)._2
    })
  }

  private enum MapSpace:
    case Empty, Wall, Start

  private def getMapSpace(c: Char): MapSpace = c match {
    case '.' => MapSpace.Empty
    case '#' => MapSpace.Wall
    case '^' => MapSpace.Start
  }

  private def getGuardRoute(guardMap: GuardMap): (Set[Point2D], Boolean) = {
    val visited = collection.mutable.Set.empty[(Point2D, Direction)]
    var currentDir = Direction.North
    var pos = guardMap.indexWhere(_ == MapSpace.Start).get

    while (guardMap.contains(pos)) {
      if (visited.contains((pos, currentDir))) {
        return (visited.map(_._1).toSet, false)
      }

      visited += ((pos, currentDir))
      val nextPos = pos + currentDir.toPoint
      if (guardMap.contains(nextPos) && guardMap.at(nextPos) == MapSpace.Wall) {
        currentDir = currentDir.rotateClockwise
      } else {
        pos = nextPos
      }
    }

    (visited.map(_._1).toSet, true)
  }
}
