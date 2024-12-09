package days

import utility.*

class Day09 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val diskMap = DiskMapGranular.fromEntries(input.trim.toCharArray.map(_.toString.toInt))
    (part1(diskMap), part2(diskMap))
  }

  private def part1(diskMap: DiskMapGranular): Long = diskMap.optimisedFragmented.checksum

  private def part2(diskMap: DiskMapGranular): Long = diskMap.optimisedBlocks.checksum

  private class DiskMapGranular(val bytes: Array[Option[Int]], originalEntries: Array[Int]) {
    def optimisedFragmented: DiskMapGranular = {
      val res = bytes.clone()
      var l = 0
      var r = bytes.length - 1

      while (l < r) {
        if (res(r).isDefined) {
          while (res(l).isDefined) {
            l += 1
          }
          res.update(l, res(r))
          res.update(r, None)
        }
        r -= 1
      }

      DiskMapGranular(res, originalEntries)
    }

    def optimisedBlocks: DiskMapGranular = {
      val res = bytes.clone()

      def swap(left: Int, right: Int, offset: Int, fileIndex: Int): Unit = {
        res.update(left + offset, Some(fileIndex))
        res.update(right + offset, None)
      }

      var r = bytes.length - 1
      var l = 0
      for (i <- Range(res.last.get, 0, -1)) {
        val len = originalEntries(i * 2)

        var spaces = 0
        var l2 = l
        var noSpaces = true
        while (spaces < len && l2 < r) {
          if (res(l2).isEmpty) {
            spaces += 1
            noSpaces = false
          } else {
            spaces = 0
            if (noSpaces) {
              l += 1
            }
          }
          l2 += 1
        }
        if (spaces == len) {
          for (j <- Range(0, len)) {
            swap(l2 - len, r - len + 1, j, i)
          }
        }
        r -= len + originalEntries(i * 2 - 1)
      }

      DiskMapGranular(res, originalEntries)
    }

    def checksum: Long = {
      bytes.zipWithIndex.collect {
        case (Some(x), i) => x * i.toLong
      }.sum
    }
  }

  private object DiskMapGranular {
    def fromEntries(entries: Array[Int]): DiskMapGranular = {
      DiskMapGranular(
        entries.zipWithIndex.flatMap((e, i) => List.fill(e)(if (i % 2 == 0) Some(i / 2) else None)),
        entries
      )
    }
  }
}
