package days

import utility.*
import utility.Direction.North

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.{ParIterable, ParSet}

class Day06 extends IDay {
  private type GuardMap = Grid2D[MapSpace]

  override def execute(input: String): (Any, Any) = {
    val guardMap = Grid2D.from2DCharArray(input, getMapSpace)
    (part1(guardMap), part2(guardMap))
  }

  private def part1(guardMap: GuardMap): Int = {
    getGuardRoute(guardMap, getStartPos(guardMap))._1.map(_._1).size
  }

  private def part2(guardMap: GuardMap): Int = {
    val startPos = getStartPos(guardMap)
    val routePositions = getGuardRoute(guardMap, startPos)._1

    val posCounts = collection.mutable.Map.empty[Point2D, Int].withDefaultValue(0)
    routePositions.foreach((pos, _) => posCounts.update(pos, posCounts(pos) + 1))
    posCounts.update(startPos, 0) // ignore start position

    // positions that are only passed once, route check starts from the position before the new wall
    val routePositionsSingle = ParIterable.fromSpecific(routePositions.filter((pos, _) => posCounts.getOrElse(pos, 0) == 1))
    val singleCount = routePositionsSingle.count((pos, dir) =>
      !getGuardRoute(addWall(guardMap, pos), pos - dir.toPoint, dir)._2)

    // positions that are passed multiple times, route check starts from the original start position
    val routePositionsMulti = ParSet.fromSpecific(routePositions.filter((pos, _) => posCounts.getOrElse(pos, 0) > 1).map(_._1))
    val multiCount = routePositionsMulti.count(pos => !getGuardRoute(addWall(guardMap, pos), startPos)._2)

    singleCount + multiCount
  }

  private def getGuardRoute(
    guardMap: GuardMap,
    startPos: Point2D,
    startDir: Direction = North
  ): (Iterable[(Point2D, Direction)], Boolean) = {
    val visited = collection.mutable.Set.empty[(Point2D, Direction)]
    var currentDir = startDir
    var pos = startPos

    while (guardMap.contains(pos) && !visited.contains((pos, currentDir))) {
      visited.add((pos, currentDir))
      val nextPos = pos + currentDir.toPoint
      if (guardMap.atOption(nextPos).contains(MapSpace.Wall)) {
        currentDir = currentDir.rotateClockwise
      } else {
        pos = nextPos
      }
    }

    (visited, !visited.contains((pos, currentDir)))
  }

  private def getStartPos(guardMap: GuardMap): Point2D = {
    guardMap.indexWhere(_ == MapSpace.Start).get
  }

  private def addWall(guardMap: GuardMap, pos: Point2D): GuardMap = {
    val newEntries = guardMap.entries.updated(pos.y, guardMap.entries(pos.y).updated(pos.x, MapSpace.Wall))
    new GuardMap(newEntries)
  }

  private def getMapSpace(c: Char): MapSpace = c match {
    case '.' => MapSpace.Empty
    case '#' => MapSpace.Wall
    case '^' => MapSpace.Start
  }

  private enum MapSpace:
    case Empty, Wall, Start
}
