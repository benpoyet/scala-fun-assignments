package recfun
import common._

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
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(remaining: List[Char], openSoFar: Int): Boolean = {
      if(remaining.isEmpty) openSoFar == 0
      else if(remaining.head == ')') (openSoFar > 0) && check(remaining.tail, openSoFar - 1)
      else if(remaining.head == '(') check(remaining.tail, openSoFar + 1)
      else check(remaining.tail, openSoFar)
    }

    check(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iter
    countChange()
  }
}
