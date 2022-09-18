package math.twentyfour

sealed trait Tree {
  def left: Tree

  def right: Tree

}

case object Leaf extends Tree {

  override def left: Tree = null

  override def right: Tree = null
}

case class OpNode(left: Tree, right: Tree) extends Tree {

}

object Tree {
  def genTree(n: Int): Seq[Tree] = {
    require(n > 0)
    val nOp = n - 1
    if (nOp == 0) {
      Seq(Leaf)
    } else {
      1.until(n).flatMap(leftNumber => {
        val rightNumber = n - leftNumber
        val leftTrees = genTree(leftNumber)
        val rightTrees = genTree(rightNumber)
        for {
          leftTree <- leftTrees
          rightTree <- rightTrees
        } yield {
          OpNode(leftTree, rightTree)
        }
      })
    }
  }

  def printTree(tree: Tree): Unit = {
    printTreeImpl(tree, 0)
  }

  private def printTreeImpl(tree: Tree, id: Int): Unit = {
    tree match {
      case Leaf => println(s"leaf[$id]")
      case OpNode(left, right) =>
        println(s"op[$id]")
        printTreeImpl(left, id + 1)
        printTreeImpl(right, id + 1)
    }
  }
}
