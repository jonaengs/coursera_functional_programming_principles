import java.util.NoSuchElementException

// List implementation copied over from week 3 worksheet
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

// We know the List(1, 2) is used as function call, and we know that this would expand to an object with an apply() method:
object List {
  def apply[T]: List[T] = new Nil
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
}