package recfun

import scala.annotation.tailrec


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
   def pascal(c: Int, r: Int): Int = {
      if (r == 0 && c == 0)
        1
      else if (c < 0 || c > r)
        0
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def countParenthesis(chars: List[Char], acc: Int): Int = {
      if (acc < 0 || chars.isEmpty) return acc

      var newAcc = acc

      if (chars.head == '(') newAcc += 1
      if (chars.head == ')') newAcc -= 1

      countParenthesis(chars.tail, newAcc)
    }

    countParenthesis(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

}
