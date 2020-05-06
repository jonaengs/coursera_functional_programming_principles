import scala.List

def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

def map[T, U](xs: List[T])(f: T => U): List[U] = xs match {
  case Nil => Nil
  case y :: ys => f(y) :: map(ys)(f)
}

// the definition of map allows us to rewrite scaleList in a much simpler way:
def _scaleList(xs: List[Double], factor: Double): List[Double] = xs map (x => x * factor)


def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y*y :: squareList(ys)
}

def _squareList(xs: List[Int]): List[Int] = xs map (x => x*x)


def filter[T](xs: List[T])(p: T => Boolean): List[T] = xs match {
  case Nil => xs
  case y :: ys => if (p(y)) y :: filter(ys)(p) else filter(ys)(p)
}


def isPos(x: Int): Boolean = x > 0
val nums = List(2, -4, 7, 5, 1)

nums filter isPos
nums filterNot isPos // -4
nums partition isPos // ((2, 7, 5, 1), (-4,))
nums takeWhile isPos // 2
nums dropWhile isPos // -4, 7, 5, 1
nums span isPos // ((2,), (-4, 7, 5, 1))


// "packs" consecutive duplicates of list elements into their own sublists
// pack(List("a", "a", "b", "c", "c", "b")) => List(List("a", "a"), List("b"), List("c", "c"), List("b"))
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  // case y :: ys => (y :: ys takeWhile (x => x == y)) :: pack(ys dropWhile (x => x == y))
  // case y :: _ => (xs takeWhile (x => x == y)) :: pack(xs dropWhile (x => x == y))
  case y :: _ =>
    def p(x: T): Boolean = x == y
    val (matching, rest) = xs span p // xs span (x => x == y)
    matching :: pack(rest)
}

pack(List(1, 1, 2, 3, 2, 2))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode(List(1, 1, 2, 3, 2, 2))

