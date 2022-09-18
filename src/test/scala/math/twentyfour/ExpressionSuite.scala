package math.twentyfour

import org.scalatest.funsuite.AnyFunSuite

class ExpressionSuite extends AnyFunSuite {
  test("toString") {
    val data = Array(1, 2, 3, 4).map(x => Rational(x))
    val expressions = Expression.generateExpressions(data)
    println(expressions.length)
    expressions.foreach(e => {
      println(e)
    })
  }
}

class PermutationSuite extends AnyFunSuite {
  private def checkValue(a: Array[Integer], b: Array[Integer]): Boolean = {
    if (a.length != b.length) {
      return false
    }
    a.zip(b).forall(p => p._1 == p._2)
  }

  test("1 2 3") {
    val p = new Permutation[Integer](Array(1, 2, 3))
    assert(p.hasNext)
    var x = p.next()
    assert(checkValue(x, Array[Integer](1, 2, 3)))
    assert(p.hasNext)
    x = p.next()
    assert(checkValue(x, Array[Integer](1, 3, 2)))
    assert(p.hasNext)
    x = p.next()
    assert(checkValue(x, Array[Integer](2, 1, 3)))
    assert(p.hasNext)
    x = p.next()
    assert(checkValue(x, Array[Integer](2, 3, 1)))
    assert(p.hasNext)
    x = p.next()
    assert(checkValue(x, Array[Integer](3, 1, 2)))
    assert(p.hasNext)
    x = p.next()
    assert(checkValue(x, Array[Integer](3, 2, 1)))
    assert(!p.hasNext)
  }

  test("1 2 3 4") {
    val p = new Permutation[Integer](Array(1, 2, 3, 4))
    assert(p.length == 24)
  }
}
