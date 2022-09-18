package math.twentyfour

import math.twentyfour.Rational.gcd

import scala.annotation.tailrec

case class Rational(
                     up: Int,
                     down: Int = 1
                   ) extends Ordered[Rational] {
  require(down != 0, "down could not be zero")

  def +(other: Rational): Rational = {
    Rational(up * other.down + down * other.up, down * other.down).normalize()
  }

  def -(other: Rational): Rational = {
    Rational(up * other.down - down * other.up, down * other.down).normalize()
  }

  def *(other: Rational): Rational = {
    Rational(up * other.up, down * other.down).normalize()
  }

  def /(other: Rational): Rational = {
    require(!other.isZero, "can not / zero")
    Rational(up * other.down, down * other.up).normalize()
  }

  def isZero: Boolean = {
    up == 0
  }

  def isNotZero: Boolean = !isZero

  def normalize(): Rational = {
    val r = gcd(up, down)
    Rational(up / r, down / r)
  }

  def ==(other: Rational): Boolean = {
    val a = normalize()
    val b = other.normalize()
    a.up == b.up && a.down == b.down
  }

  override def compare(that: Rational): Int = {
    val result = (this - that).normalize()
    (result.up * result.down).compareTo(0)
  }

  override def toString: String = {
    val x = normalize()
    if (x.down == 1) {
      s"${x.up}"
    } else {
      s"${x.up}/${x.down}"
    }
  }
}

object Rational {
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) {
      return a
    }
    gcd(b, a % b)
  }
}

sealed trait Expression {
  def value: Rational
  def valid: Boolean
}

case class Direct(a: Rational) extends Expression {
  override def value: Rational = a

  override def toString: String = a.toString

  override def valid: Boolean = true
}


case class Plus(a: Expression, b: Expression) extends Expression {
  override def value: Rational = {
    a.value + b.value
  }

  override def valid: Boolean = a.valid && b.valid

  override def toString: String = s"($a + $b)"
}

case class Sub(a: Expression, b: Expression) extends Expression {
  override def value: Rational = a.value - b.value

  override def valid: Boolean = {
    a.valid && b.valid && a.value >= b.value
  }

  override def toString: String = s"($a - $b)"
}

case class Mul(a: Expression, b: Expression) extends Expression {
  override def value: Rational = a.value * b.value

  override def toString: String = s"($a * $b)"

  override def valid: Boolean = a.valid && b.valid
}

case class Div(a: Expression, b: Expression) extends Expression {
  override def value: Rational = {
    a.value / b.value
  }

  override def valid: Boolean = {
    b.valid && a.valid && b.value != Rational(0)
  }

  override def toString: String = s"($a / $b)"
}

object Expression {
  def generateExpressions(data: Array[Rational]): Seq[Expression] = {
    require(data.nonEmpty)
    if (data.length == 1) {
      Seq(Direct(data(0)))
    } else {
      val n = data.length
      1.until(n).flatMap(leftNumber => {
        val leftArray = data.slice(0, leftNumber)
        val rightArray = data.slice(leftNumber, n)
        generateExpressions(leftArray).flatMap(l => {
          generateExpressions(rightArray).flatMap(r => {
            Seq(
              Plus(l, r),
              Sub(l, r),
              Mul(l, r)
            )
          })
        })
      })
    }

  }

}