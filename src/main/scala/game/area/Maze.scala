package game.area

import game.area.Maze.randomChoose
import utils.WithClose
import utils.pdf.Document

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case class Maze(rows: Int, cols: Int) {
  private val data = Array.fill(rows * cols)(Maze.unfilled)
  private val grids = RowCol(rows, cols)

  @tailrec
  final def init(group: Int = 0): Unit = {
    if (group < rows * cols) {
      getNextEmpty match {
        case Some(index) =>
          generateArea(index, group)
          init(group + 1)
        case None =>
      }
    }
  }

  def toPuzzle: Puzzle = {
    val groups = data.max + 1
    val values = (0 until groups map { g =>
      val indexes = data.indices.filter(idx => data(idx) == g)
      val index = randomChoose(indexes).get
      val rowCol = RowCol.make(grids, index)
      (rowCol, indexes.length)
    }).toArray
    Puzzle(
      rows, cols, groups, values
    )
  }

  override def toString: String = {
    (for {
      row <- 0 until rows
    } yield {
      0 until cols map { col => data(RowCol(row, col).index(grids)) } mkString " "
    }).mkString("\n")
  }

  private def getNextEmpty: Option[Int] = {
    data.indices.find(data(_) == Maze.unfilled)
  }

  private def generateArea(index: Int, group: Int): Unit = {
    val rowCol = RowCol.make(grids, index)

    def isValid(dr: Int, dc: Int, debug: Boolean = false): Boolean = {
      (dr * dc * 3 <= rows * cols) &&
        (for {
          r <- 0 until dr
          c <- 0 until dc
        } yield {
          val rc = rowCol + RowCol(r, c)
          val index = rc.index(grids)
          data(index)
        }).forall(_ == Maze.unfilled)
    }

    val possibleRows = rows - rowCol.row
    val possibleCols = cols - rowCol.col
    val candidates = for {
      deltaRow <- 1 to possibleRows
      deltaCol <- 1 to possibleCols if isValid(deltaRow, deltaCol, group == 1)
    } yield (deltaRow, deltaCol)

    val removeOneOne = if (candidates.length > 1) {
      candidates.drop(1)
    } else {
      candidates
    }

    randomChoose(removeOneOne) foreach {
      case (dr, dc) =>
        for {
          r <- 0 until dr
          c <- 0 until dc
        } {
          val idx = (rowCol + RowCol(r, c)).index(grids)
          data(idx) = group
        }
    }
  }
}

object Maze {
  def unfilled: Int = -1

  def randomChoose[A](as: Seq[A]): Option[A] = {
    if (as.isEmpty) {
      None
    } else {
      val n = as.length
      val idx = Random.between(0, n)
      Some(as(idx))
    }
  }

  def main(args: Array[String]): Unit = {
    val puzzleTexts = mutable.Set.empty[String]
    val puzzles = mutable.ArrayBuffer.empty[Puzzle]
    for {
      grid <- 6 to 10
    } {
      val n = grid * grid * 3
      0.until(n) foreach (_ => {
        val maze = Maze(grid, grid)
        maze.init()
        val puzzle = maze.toPuzzle
        val puzzleText = puzzle.toString
        if (!puzzleTexts.contains(puzzleText)) {
          puzzles.append(puzzle)
          puzzleTexts.add(puzzleText)
        }
      })
    }
    WithClose(Document("AreaPuzzle.pdf", fontSize = 32)) { doc =>
      puzzles.foreach(_.pdf(doc))
    }
  }
}
