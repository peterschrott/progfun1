package week4

trait Expr {

  def eval: Int
  def show: String = {
    this match {
      case Num(n) => s"$n"
      case Sum(left, right) => s"${left.show} + ${right.show}"

      case Product(left: Sum, right: Expr) => s"(${left.show}) * ${right.show}"
      case Product(left: Expr, right: Sum) => s"${left.show} * (${right.show})"
      case Product(left: Sum, right: Sum) => s"(${left.show}) * (${right.show})"
      case Product(left, right) => s"${left.show} * ${right.show}"

      case Var(s) => s
    }
  }
}

case class Num(n: Int) extends Expr {

  override def eval: Int = n
}

case class Sum(left: Expr, right: Expr) extends Expr {

  override def eval: Int = left.eval + right.eval
}

case object Sum {
  def apply(left: Expr, right: Int): Sum = {
    Sum(left, Num(right))
  }
  def apply(left: Int, right: Expr): Sum = {
    Sum(Num(left), right)
  }
  def apply(left: Int, right: Int): Sum = {
    Sum(Num(right), Num(right))
  }
}

case class Product(left: Expr, right: Expr) extends Expr {

  override def eval: Int = left.eval * right.eval
}

case object Product {
  def apply(left: Expr, right: Int): Product = {
    Product(left, Num(right))
  }
  def apply(left: Int, right: Expr): Product = {
    Product(Num(left), right)
  }
  def apply(left: Int, right: Int): Product = {
    Product(Num(right), Num(right))
  }
}

case class Var(symbol: String) extends Expr {

  override def eval: Int = -1
}