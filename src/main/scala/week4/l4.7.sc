import scala.List


def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => x :: xs
  case y :: ys =>
    if (x > y) y :: insert(x, ys)
    else x :: xs
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()  // Empty list. List() ⇔ Nil
  case y :: ys => insert(y, isort(ys))
}


val l = List(3, 1, 4, 2)
isort(l)