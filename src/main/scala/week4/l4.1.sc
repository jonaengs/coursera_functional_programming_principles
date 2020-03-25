abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object zero extends Nat {
  def isZero: Boolean = true
  def predecessor = throw new NoSuchElementException
  def +(that: Nat): Nat = that
  def -(that: Nat) = throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {  // successor to n
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)  // recursive, since n is the predecessor
  def -(that: Nat): Nat = if (that.isZero) n else n - that.predecessor  // recursive call to n
}
