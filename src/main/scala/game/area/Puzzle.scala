package game.area

import utils.WithClose
import utils.pdf.{Document, PDRect, Page}

case class Puzzle(
                   rows: Int,
                   cols: Int,
                   groups: Int,
                   values: Array[(RowCol, Int)]
                 ) {
  def pdf(doc: Document): Unit = {
    WithClose(Page(doc)) {
      page =>
        val box = page.box
        val scale = 0.8f
        val gridSize = scala.math.min(scale * box.w / cols, scale * box.h / rows)
        val startX = (box.w - gridSize * cols) / 2f
        val startY = (box.h - gridSize * rows) / 2f
        val endX = startX + gridSize * cols
        val endY = startY + gridSize * rows
        0 to rows foreach { rowIndex =>
          page.horizontalSplit(startY + rowIndex * gridSize, startX, endX)
        }
        0 to cols foreach { colIndex =>
          page.verticalSplit(startX + colIndex * gridSize, startY, endY)
        }
        values.foreach {
          case (rc, v) =>
            page.cell(v.toString, PDRect(startX + rc.col * gridSize, startY + (rows - 1 - rc.row) * gridSize, gridSize,
              gridSize))
        }
    }
  }
}
