import scala.annotation.tailrec

def abs(x:Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def isGoodEnough(guess: Double, x: Double) = abs((guess * guess) - x) / x < 0.001

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  sqrtIter(1.0, x)
}

sqrt(2)
sqrt(0.01)
sqrt(0.1e-20)
sqrt(1e20)
sqrt(1e50)

def sqrt_clean(x: Double) = {
  def isGoodEnough(guess: Double) = abs((guess * guess) - x) / x < 0.001

  def improve(guess: Double) = (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt_clean(2)

val foo = 1
+ 2

val bar = 1 +
  3

def factorial(n: Int): Int = {
  if (n == 0) 1 else n * factorial(n - 1)
}

def f(n: Int): Int = {
  @tailrec
  def loop(acc: Int, curr: Int): Int = {
    if (curr == 0) acc
    else loop(acc * curr, curr - 1)
  }
  loop(1, n)
}

f(5)

