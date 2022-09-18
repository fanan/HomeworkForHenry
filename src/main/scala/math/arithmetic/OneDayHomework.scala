package math.arithmetic

import utils.WithClose
import utils.pdf.{Document, PDRect, Page}

case class OneDayHomework(
                           day: String,
                           puzzles: Array[Puzzle],
                           cols: Int = 2,
                           showDate: Boolean = true
                         ) {
  def generatePage(document: Document): Unit = {
    require(puzzles.length % cols == 0)
    val rows = puzzles.length / cols
    WithClose(Page(document)) {
      page =>
        val r = scala.util.Random
        val turnedPuzzles = puzzles.map {
          case p@PlusPuzzle(a, b) => if (r.nextBoolean()) {
            p.shuffle
          } else {
            p
          }
          case p@MinusPuzzle(a, b) => p
        }

        val finalPuzzles = r.shuffle(turnedPuzzles.iterator).map(_.text).toArray
        val box = page.box
        val columnWidth = box.w / cols
        val displayRows = rows + 2
        val rowHeight = box.h / displayRows
        0.until(cols).foreach(columnIndex => {
          0.until(rows).foreach(rowIndex => {
            val puzzleIndex = columnIndex * rows + rowIndex
            val puzzle = finalPuzzles(puzzleIndex)
            val rect = PDRect(
              x = columnWidth * columnIndex,
              y = (rows - rowIndex) * rowHeight,
              w = columnWidth,
              h = rowHeight
            )
            page.cell(puzzle, rect)
          })
        })
        1.until(cols).foreach(index => {
          page.verticalSplit(columnWidth * index, rowHeight * 1.5f, box.h - rowHeight * 1.5f)
        })
        if (showDate) {
          val footerRect = PDRect(x = 0, y = 0, w = box.w, h = rowHeight)
          page.cell(day, footerRect)
        }
    }
  }
}
