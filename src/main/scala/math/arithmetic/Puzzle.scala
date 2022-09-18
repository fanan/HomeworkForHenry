package math.arithmetic

import scala.collection.mutable

sealed trait Puzzle {
  def level: Int

  def text: String

  def shuffle: Puzzle

  def result: Int
}

case class PlusPuzzle(a: Int, b: Int) extends Puzzle {

  override def level: Int = {
    var flag = false
    val (aList, bList) = Puzzle.Padding(a, b)
    aList.zip(bList).foldLeft(0)((acc, p) => {
      val next = if (flag) {
        p._1 + p._2 + 1
      } else {
        p._1 + p._2
      }
      flag = next >= 10
      if (flag) {
        acc + 1
      } else {
        acc
      }
    })
  }

  override def text: String = s"""$a + $b = """

  override def shuffle: Puzzle = PlusPuzzle(b, a)

  override def result: Int = a + b
}

case class MinusPuzzle(a: Int, b: Int) extends Puzzle {
  override def level: Int = {
    var flag = false
    val (aList, bList) = Puzzle.Padding(a, b)
    aList.zip(bList).foldLeft(0)((acc, p) => {
      flag = if (flag) {
        p._1 < p._2 + 1
      } else {
        p._1 < p._2
      }
      if (flag) {
        acc + 1
      } else {
        acc
      }
    })

  }

  override def text: String = s"""$a - $b = """

  override def shuffle: Puzzle = this

  override def result: Int = a - b
}


object Puzzle {
  private def IntToArrayBuilder(a: Int): mutable.ArrayBuilder[Int] = {
    val ab = mutable.ArrayBuilder.make[Int]
    var b = a
    if (a == 0) {
      ab.addOne(0)
      return ab
    }
    while (b > 0) {
      ab.addOne(b % 10)
      b /= 10
    }
    ab
  }

  def IntToList(a: Int): Array[Int] = {
    IntToArrayBuilder(a).result()
  }

  def Padding(a: Int, b: Int): (Array[Int], Array[Int]) = {
    val aBuilder = IntToArrayBuilder(a)
    val bBuilder = IntToArrayBuilder(b)
    val n = scala.math.max(aBuilder.length, bBuilder.length)
    if (aBuilder.length < n) {
      aBuilder.addAll(Iterator.fill(n - aBuilder.length)(0))
    }
    if (bBuilder.length < n) {
      bBuilder.addAll(Iterator.fill(n - bBuilder.length)(0))
    }
    (aBuilder.result(), bBuilder.result())
  }

  def apply(a: Int, b: Int, symbol: String): Puzzle = {
    symbol match {
      case "+" => PlusPuzzle(a, b)
      case "-" => if (a >= b) {
        MinusPuzzle(a, b)
      } else {
        MinusPuzzle(b, a)
      }
    }
  }

  def generate(min: Int, max: Int): Seq[Puzzle] = {
    min.to(max).flatMap(a => {
      a.to(max).flatMap(b => {
        Seq(
          Puzzle(a, b, "+"),
          Puzzle(b, a, "-")
        )
      })
    })
  }
}