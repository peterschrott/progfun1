package week3

import java.util.NoSuchElementException

trait List[+T] {

  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

  def head: T

  def tail: List[T]

  def prepend[U >: T](e: U): List[U] = new Cons[U](e, this)
}

case class Cons[T](override val head: T, override val tail: List[T]) extends List[T] {

  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
