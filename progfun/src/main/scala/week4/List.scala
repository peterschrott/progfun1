package week4

import week3.{Cons, List, Nil}

object List {

  private def singleton[T](element: T) = new Cons[T](element, Nil)
  private def list[T](e: T, l: List[T]) = new Cons[T](e, l)

  def apply[T](): List[T] = {
    Nil
  }

  def apply[T](x: T): List[T] = {
    singleton(x)
  }

  def apply[T](x1: T, x2: T): List[T] = {
    list(x2, singleton(x1))
  }

  def apply[T](x1: T, x2: T, x3: T): List[T] = {
    list(x3, list(x2, singleton(x1)))
  }
}
