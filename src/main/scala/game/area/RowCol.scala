package game.area

case class RowCol(row: Int, col: Int) {
  def +(other: RowCol): RowCol = {
    RowCol(row + other.row, col + other.col)
  }

  def index(grids: RowCol): Int = {
    col * grids.row + row
  }

  def maybeIndex(grids: RowCol): Option[Int] = {
    if (isValid(grids)) {
      Some(index(grids))
    } else {
      None
    }
  }

  def isValid(grids: RowCol): Boolean = {
    row >= 0 && row < grids.row && col >= 0 && col < grids.col
  }
}

object RowCol {
  def make(grids: RowCol, index: Int): RowCol = {
    val col = index / grids.row
    val row = index % grids.row
    RowCol(row, col)
  }

  def maybeMake(grids: RowCol, index: Int): Option[RowCol] = {
    if (grids.row > 0 && grids.col > 0 && index >= 0 && index < grids.row * grids.col) {
      Some(make(grids, index))
    } else {
      None
    }
  }
}
