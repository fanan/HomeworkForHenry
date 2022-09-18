package math.twentyfour

object HomeWork {
  private val target = Rational(24)

  def main(args: Array[String]): Unit = {
    val candidates = for {
      a <- 1.to(10)
      b <- a.to(10)
      c <- b.to(10)
      d <- c.to(10)
    } yield Array[Int](a, b, c, d)

    candidates.foreach(arr => {
      new Permutation[Rational](arr.map(Rational(_))).exists(xs => {
        val expressions = Expression.generateExpressions(xs)
        val expr = expressions.find(x => x.valid && x.value == target)
        if (expr.nonEmpty) {
          println(s"${expr.get} = $target")
        }
        expr.nonEmpty
      })
    })
  }
}
