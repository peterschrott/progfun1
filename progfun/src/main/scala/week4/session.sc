import week3.{Empty, IntSet, NonEmpty, List => MyList}
import week4._

// ########################### 4.1 ###########################

val y: MyBoolean = True
val n: MyBoolean = False

val result: MyBoolean = y < n

println(result.toString)

val z = Zero
val one = z.successor // new Succ(Zero)

val two = one + one
// new Succ(Zero) + new Succ(Zero)
// new Succ(Zero + new Succ(Zero))
// new Succ(new Succ(Zero))

val three = two + one
// new Succ(new Succ(Zero)) + new Succ(Zero)
// new Succ(new Succ(Zero) + new Succ(Zero))
// new Succ(new Succ(Zero + new Succ(Zero)))
// new Succ(new Succ(new Succ(Zero)))

val three2 = one + two
// new Succ(Zero) + new Succ(new Succ(Zero))
// new Succ(Zero + new Succ(new Succ(Zero))
// new Succ(new Succ(new Succ(Zero))

// ########################### 4.2 ###########################

val f: Int => Int = (x: Int) => x * x

class AnonFun extends Function1[Int, Int] {
  override def apply(v1: Int): Int = v1 * v1
}
val f2 = new AnonFun
println(f2.apply(17))

new Function1[Int, Int] {
  def apply(v1: Int): Int = v1 * v1
}.apply(17)

// ########################### 4.3 ###########################

val nonEmpty: NonEmpty = new NonEmpty(1, Empty, Empty)
val intSet: IntSet = Empty

def assertAllPositive_1(s: IntSet): IntSet = {
  s
}

// ########################### 4.4 ###########################

def assertAllPositive_2[T <: IntSet](s: T): T = {
  s
}

def assertAllPositive_3(s: NonEmpty): IntSet = {
  s
}

assertAllPositive_1(nonEmpty)
assertAllPositive_1(Empty)
assertAllPositive_1(intSet)

assertAllPositive_2(nonEmpty)
assertAllPositive_2(Empty)
assertAllPositive_2(intSet)

assertAllPositive_3(nonEmpty)
// assertAllPositive_3(Empty)
// assertAllPositive_3(intSet)

val a: Array[NonEmpty] = Array(nonEmpty)
// val b: Array[IntSet] = a <-- Arrays are NOT covariant in scala!


def f1(x: NonEmpty): IntSet = ???
def f2(x: IntSet): NonEmpty = ???

// val r11: IntSet = f1(nonEmpty)
// val r12: IntSet = f2(nonEmpty)

// val r21: NonEmpty = f2(intSet)
// val r22: NonEmpty = f1(intSet)

def f44(xs: MyList[NonEmpty], x: Empty.type): MyList[IntSet] = {
  xs.prepend(x)
}

// ########################### 4.5 ###########################

println(Sum(Num(1), Num(2)).eval)

// ########################### 4.6 ###########################

val N = 12

2 match {
  case N => println(N)
  case 2 => println(2)
  case x => println(x)
}

println(Sum(Num(1), Num(2)).show)

println(Sum(Product(2, Var("x")), Var("y")).show)
println(Product(Sum(2, Var("x")), Var("y")).show)

// ########################### 4.7 ###########################

val nums: List[Int] = scala.List(1, 2, 3, 4)
val nums2: List[Int] = 1 :: (2 :: (3 :: (4 :: Nil)))

case class Foo(var bar: Int) {
  def baz_: (x: Int) = {
    println(x)
  }
}

1 baz_: Foo(2)
