package funsets


trait FunSets extends FunSetsInterface {

  override type FunSet = Int => Boolean

  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): FunSet = x => x == elem

  def union(s: FunSet, t: FunSet): FunSet = x => s(x) || t(x)

  // intersect and filter are functionally the same :mind-blown:
  def intersect(s: FunSet, t: FunSet): FunSet = x => s(x) && t(x)

  def diff(s: FunSet, t: FunSet): FunSet = x => s(x) && !t(x)

  def filter(s: FunSet, p: Int => Boolean): FunSet = x => s(x) && p(x)

  val bound = 1000
  /** Test whether predicate p holds for all elements of s */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  // double negative?? whaaat
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def map(s: FunSet, f: Int => Int): FunSet = x => exists(s, y => f(y) == x)


  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
