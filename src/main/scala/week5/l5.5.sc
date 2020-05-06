import scala.List

// map and length functions implemented using foldRight

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) ((x, z) => f(x) :: z)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((_, z) => z + 1)


val l = List(5, 4, 3, 2, 1)
def f(x: Int): Int = 2 * x

lengthFun(l)
mapFun(l, f)