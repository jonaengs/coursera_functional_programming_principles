import scala.List
import scala.Vector

(1 to 4) flatMap (x => 1 to x)
"Hey" flatMap (c => List('.', c))

(1 to 10).sum
val ps = (1 to 10) zip (-1 to -5 by -1)
ps.unzip


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

// using pattern matching
def _scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case (x, y) => x * y}.sum

// not exactly efficient, but conceptually it is very simple and precise
def isPrime(n: Int): Boolean =
  (2 until n) forall (i => n % i != 0)

(2 to 20) filter isPrime