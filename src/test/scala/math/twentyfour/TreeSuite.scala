package math.twentyfour

import org.scalatest.funsuite.AnyFunSuite

class TreeSuite extends AnyFunSuite {
  test("print tree") {
    val trees = Tree.genTree(4)
    trees.foreach(tree => {
      println("=======")
      Tree.printTree(tree)
    })
  }
}
