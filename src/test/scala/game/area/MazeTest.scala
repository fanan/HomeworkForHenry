package game.area

import org.scalatest.funsuite.AnyFunSuite

class MazeTest extends AnyFunSuite {
  test("init") {
    val maze = Maze(8,8)
    maze.init()
    println(maze)
  }
}