abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right:IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains elem
    else if (x > elem) right contains elem
    else true
  def incl(x :Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this // x must be equal to elem, so it is already in the tree
  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem  // basically, deconstruct the entire thing and add each element again?

  override def toString = "{" + left + elem + right + "}"
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other

  override def toString() = "."
}

object intSets {
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
}


val x = null
val y: String = x
val z: Int = x