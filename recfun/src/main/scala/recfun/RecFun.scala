package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println()
    println("Parentheses Balancing")
    printBalancedResult("(()())")
    printBalancedResult("(if (zero? x) max (/ 1 x))")
    printBalancedResult("I told him (that it’s not (yet) done). (But he wasn't listening)")
    printBalancedResult(":-)")
    printBalancedResult("())(")

    println()
    println("Counting Change")
    printCountChangeResult(4, List(1, 2))
    printCountChangeResult(5, List(1, 2, 3))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) {
      1
    } else if (r == 0) {
      0
    } else if (c > r) {
      0
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceInner(chars: List[Char], balance: Int): Int = {
      if (balance < 0) {
        balance
      } else if (chars.isEmpty) {
        balance
      } else {
        val head = chars.head
        val tail = chars.tail
        val opening = if (head == '(') 1 else 0
        val closing = if (head == ')') -1 else 0
        balanceInner(tail, balance + opening + closing)
      }
    }

    balanceInner(chars, 0) == 0
  }

  def printBalancedResult(s: String): Unit = {
    println(s"${if (balance(s.toList)) "✅" else "❌"} String '$s' is ${if (balance(s.toList)) "" else "NOT "}balanced")
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else if (money > 0 && coins.isEmpty) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }

  def printCountChangeResult(money: Int, coins: List[Int]): Unit= {
    val result = countChange(money, coins)
    println(s"Changing $money money with coins {${coins.mkString(", ")}} is possible in $result ways")
  }
}
