import week4._

// ######### 4.1 #########

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

// ######### 4.2 #########

val f: Int => Int = (x: Int) => x * x

class AnonFun extends Function1[Int, Int] {
  override def apply(v1: Int): Int = v1 * v1
}
val f2 = new AnonFun
println(f2.apply(17))

new Function1[Int, Int] {
  def apply(v1: Int): Int = v1 * v1
}.apply(17)

