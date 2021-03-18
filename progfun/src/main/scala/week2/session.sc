import scala.annotation.tailrec

def sum1(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum1(f, a + 1, b)
}

sum1(x => x, 1, 10)

def sum(f: Int => Int, a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x, 1, 10)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x)(1, 5)

def factorial(n: Int): Int = product(x => x)(1, n)

factorial(5)

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
  if(a > b) unit
  else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))
}

val squares: Int => Int = x => x * x

def product1(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
def product1OfSquares: (Int, Int) => Int = product1(squares)
product1OfSquares(1, 3)

def product2(f: Int => Int): (Int, Int) => Int = mapReduce(f, (x, y) => x * y, 1)(_, _)
val product2OfSquares = product2(squares)
product2OfSquares(1, 4)

val product3: (Int => Int) => (Int, Int) => Int = mapReduce(_, (x, y) => x * y, 1)
val product3OfSquares = product3(squares)
product3OfSquares(1, 3)

def sumSqrt: (Int, Int) => Int = mapReduce(x => x*x, (x, y) => x + y, 0)(_, _)
sumSqrt(1, 6)

def fact(n: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, n)
fact(3)

def avgDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def avgDampSqrt(x: Double): Double => Double = avgDamp(y => x / y)

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ //

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must not be zero!")

  def this(x: Int) = this(x, 1)

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numerator = x
  val denominator = y

  def +(that: Rational): Rational = {
    new Rational(
      this.numerator * that.denominator + that.numerator * this.denominator,
      this.denominator + that.denominator
    )
  }

  def -(that: Rational): Rational = {
    this + -that
  }

  def unary_- : Rational = {
    new Rational(this.numerator * -1, this.denominator)
  }

  def <(that: Rational): Boolean = {
    this.numerator * that.denominator < that.numerator * this.denominator
  }

  def max(that: Rational): Rational = {
    if (this < that ) {
      that
    } else {
      this
    }
  }

  override def toString: String = {
    val g = gcd(this.numerator, this.denominator)
    s"${numerator/g}/${denominator/g}"
  }

}

val x = new Rational(1, 2)
val y = new Rational(1, 2)
x.numerator
x.denominator
x + y
x - y