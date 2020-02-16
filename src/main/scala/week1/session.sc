import scala.annotation.tailrec

object session {
  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    @tailrec
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) < getEpsilon()

    def getEpsilon(): Double = x / 10000

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1)
  }

  def factorial(n: Integer): Integer = if (n <= 1) 1 else n * factorial(n-1)

  @tailrec
  def tail_rec_factorial(n: Integer, carry: Integer = 1): Integer =
    if (n <= 1) carry else tail_rec_factorial(n - 1, n * carry)

  def tail_rec_factorial_lf(n: Int): Int = {
    // acc = accumulator
    def loop(n: Int, acc: Int): Int =
      if (n <= 1) acc else loop(n - 1, n * acc)
    loop(n, 1)
  }
}

session.sqrt(1.0e60)
session.sqrt(1.0e-6)

session.factorial(5)
session.tail_rec_factorial(5)