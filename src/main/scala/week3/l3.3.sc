import java.util.NoSuchElementException
import scala.annotation.tailrec

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException(("Nil.tail"))
}

@tailrec
def nth[T](list: List[T], n: Int): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else nth(list.tail, n-1)


val l = new Cons(1, new Cons(2, new Cons(3, new Nil())))

nth(l, 1)
nth(l, -1)