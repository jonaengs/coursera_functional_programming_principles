import scala.List

// without using tuples -- inelegant
def _merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
  case Nil => ys
  case x :: xs1 => ys match {
    case Nil => xs
    case y :: ys1 =>
      if (x < y) x :: _merge(xs1, ys)
      else y :: _merge(xs, ys1)
  }
}


def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
}

// splitAt is a list method. This is purely for demonstration purposes
def splitAt(xs: List[Int], n: Int): (List[Int], List[Int]) = {
  (xs take n, xs drop n)
}

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

merge(List(1,2,3), List(3, 4, 5))
msort(List(7, -4, 1, 5, 2))
