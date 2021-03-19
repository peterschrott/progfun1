import scala.annotation.tailrec

import java.util.NoSuchElementException

trait List[T] {

  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

  def head: T

  def tail: List[T]
}

class Cons[T](override val head: T, override val tail: List[T]) extends List[T] {

  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

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

