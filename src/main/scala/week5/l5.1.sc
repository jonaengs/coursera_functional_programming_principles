import scala.annotation.tailrec
import scala.List

@tailrec
def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("Nil.last")
  case List(x: T) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("Nil.init")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

// xs ::: ys ==> yx.:::(xs) (prepend)
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ::: List(y)
}

def removeAt[T](xs: List[T], n: Int): List[T] = {
  def counter(xs: List[T], n: Int): List[T] = xs match {
    case y :: ys =>
      if (n == 0) y :: ys.tail
      else y :: counter(ys, n - 1)
  }
  if (xs.length <= n) xs
  else counter(xs, n - 1)
}

def removeAt2[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys => y match {
    case z :: zs => (z :: flatten(zs)) ::: flatten(ys)
    case _ => y :: flatten(ys)
  }
}


val l = 1 :: 2 :: 3 :: Nil
last(l)
init(l)
concat(l, l) // = xs ::: ys
reverse(l)
removeAt(l, 2)
removeAt2(l, 2)
flatten(List(List(1, 1), 2, List(3, List(5, 8))))
