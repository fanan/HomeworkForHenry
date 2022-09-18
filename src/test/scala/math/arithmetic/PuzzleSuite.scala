package math.arithmetic

import org.scalatest.funsuite.AnyFunSuite

class PuzzleSuite extends AnyFunSuite {
  test("int to list") {
    assert(Puzzle.IntToList(0) sameElements Array(0))
    assert(Puzzle.IntToList(10) sameElements Array(0, 1))
    assert(Puzzle.IntToList(21) sameElements Array(1, 2))
  }

  test("PlusPuzzle level") {
    assert(PlusPuzzle(4, 1).level == 0)
    assert(PlusPuzzle(4, 6).level == 1)
    assert(PlusPuzzle(33, 77).level == 2)
    assert(PlusPuzzle(100, 1).level == 0)
    assert(PlusPuzzle(99, 2).level == 2)
  }

  test("MinusPuzzle level") {
    assert(MinusPuzzle(10, 2).level == 1)
    assert(MinusPuzzle(20, 17).level == 1)
    assert(MinusPuzzle(99, 17).level == 0)
    assert(MinusPuzzle(101, 3).level == 2)
  }

  test("generate") {
    val puzzles = Puzzle.generate(10, 50)
    assert(puzzles.contains(PlusPuzzle(20, 30)))
    assert(puzzles.contains(MinusPuzzle(30, 20)))
    assert(puzzles.contains(PlusPuzzle(10, 50)))
    assert(puzzles.contains(PlusPuzzle(10, 10)))
    assert(puzzles.contains(PlusPuzzle(50, 50)))
    assert(!puzzles.contains(PlusPuzzle(9,50)))
    assert(!puzzles.contains(MinusPuzzle(20,30)))
  }
}
