import week3._

import scala.annotation.tailrec

def singleton[T](element: T) = new Cons[T](element, new Nil[T])

singleton(1)
singleton(true)

@tailrec
def nth[T](n: Int, xs: List[T]): T = {
  if (xs.isEmpty) {
    throw new IndexOutOfBoundsException
  } else if (n == 0) {
    xs.head
  } else {
    nth(n - 1, xs.tail)
  }
}

val l = new Cons(1, new Cons(2, new Nil))
println(nth(0, l))
println(nth(1, l))
// println(nth(2, l))

println(nth(-1, l))

