package week3

abstract class IntSet {

  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

case class NonEmpty(e: Int, l: IntSet, r: IntSet) extends IntSet {

  override def contains(x: Int): Boolean = {
    if (x < e) l contains x
    else if (x > e) r contains x
    else true
  }

  override def incl(x: Int): IntSet = {
    if (x < e) NonEmpty(e, l incl x, r)
    else if (x > e) NonEmpty(e, l, r incl x)
    else this
  }

  override def union(other: IntSet): IntSet = {
    ((l union r) union other) incl e
  }

  override  def toString: String = s"{$l $e $r}"
}

case object Empty extends IntSet {

  override def contains(x: Int) = false

  override def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)

  override def union(other: IntSet): IntSet = other

  override def toString: String = "."
}