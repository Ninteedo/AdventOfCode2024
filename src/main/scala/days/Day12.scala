package days

import utility.*

class Day12 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val gardenMap = GardenMap(Grid2D.from2DCharArray(input, identity))
    (part1(gardenMap), part2(gardenMap))
  }

  private def part1(gardenMap: GardenMap): Int = {
    def perimeter(region: Set[Point2D]): Int = {
      region.toList.map(pos => Direction.values.count(dir => !region.contains(pos + dir))).sum
    }

    gardenMap.regions.map(region => region.size * perimeter(region)).sum
  }

  private def part2(gardenMap: GardenMap): Int = {
    def perimeterSides(region: Set[Point2D]): Int = {
      val perimeters = Direction.values.map(dir => collection.mutable.Set.from(
        region
          .filter(pos => !region.contains(pos + dir))
          .map(pos => Perimeter(List(pos), dir)))
      )

      perimeters.foreach(group => {
        var changed = true
        while (changed) {
          changed = false
          group
            .toList
            .combinations(2)
            .map(ps => (ps, ps.head.join(ps.last)))
            .find(_._2.length == 1)
            .foreach((orig, updated)=> {
              orig.foreach(p => group.remove(p))
              group.add(updated.head)
              changed = true
            })
        }
      })

      perimeters.map(_.size).sum
    }

    gardenMap.regions.map(region => region.size * perimeterSides(region)).sum
  }

  private case class GardenMap(plots: Grid2D[Char]) {
    def regions: List[Set[Point2D]] = {
      val consumed = collection.mutable.Set.empty[Point2D]

      def findAllInRegion(start: Point2D): Set[Point2D] = {
        val frontier = collection.mutable.Set(start)
        val result = collection.mutable.Set.empty[Point2D]
        val c = plots.at(start)

        while (frontier.nonEmpty) {
          val pos = frontier.head
          frontier.remove(pos)
          result.add(pos)

          for (dir <- Direction.values) {
            val next = pos + dir
            if (plots.contains(next) && !result.contains(next) && plots.at(next) == c) {
              frontier.add(next)
            }
          }
        }

        result.toSet
      }

      plots.indices.flatMap(pos => {
        if (consumed.contains(pos)) None
        else {
          val regionPositions = findAllInRegion(pos)
          consumed.addAll(regionPositions)
          Some(regionPositions)
        }
      })
    }
  }

  private case class Perimeter(from: List[Point2D], dir: Direction) {
    def join(other: Perimeter): List[Perimeter] = {
      if (dir == other.dir) {
        if (from.head - dir.rotateClockwise == other.from.last) {
          List(Perimeter(other.from ++ from, dir))
        } else if (from.last + dir.rotateClockwise == other.from.head) {
          List(Perimeter(from ++ other.from, dir))
        } else {
          List(this, other)
        }
      } else List(this, other)
    }
  }
}
