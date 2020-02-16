package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println(balance("((asdasd)))()".toList))
    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def evalChar(c: Char): Int =
      if (c == '(') 1
      else if (c == ')') -1
      else 0
    def findBalance(chars: List[Char], balance: Int): Boolean =
      if (balance < 0 || chars.isEmpty) balance == 0
      else findBalance(chars.tail, balance + evalChar(chars.head))
    findBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money > 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
