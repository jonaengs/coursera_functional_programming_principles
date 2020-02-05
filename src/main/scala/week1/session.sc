import scala.annotation.tailrec

object session {
  def abs(x: Double): Double = if (x < 0) -x else x

  @tailrec
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) < getEpsilon(x)

  /*
  problem with previous method: for small numbers the precision was trash,
  and for large enough numbers, the Double primitive would not have enough enough
  bits available to be within the required precision value.

  Set the epsilon to some fraction of x. dividing by 100 should give
  an error bound of about 1%. 1000 around 0.1%
   */
  def getEpsilon(x: Double): Double = x/10000

  def improve(guess: Double, x: Double): Double =
     (guess + x / guess) / 2

  def sqrt(x: Double): Double = sqrtIter(1, x)
}

session.sqrt(1.0e60)
session.sqrt(1.0e-6)