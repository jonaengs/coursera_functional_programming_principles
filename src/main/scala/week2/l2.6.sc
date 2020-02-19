class Rational(x: Int, y: Int) {
  require(y != 0, "the denominator cannot be zero")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer: Int = x / g
  def denom: Int = y / g

  def <(s: Rational): Boolean = this.numer * s.denom < s.numer * this.denom
  def max(s: Rational): Rational = if (this < s) s else this

  def makeString(): String = numer + "/" + denom
}


class UnsimplifiedRational(x: Int, y: Int) {
  require(y != 0, "the denominator cannot be zero")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def numer: Int = x
  def denom: Int = y

  def <(s: UnsimplifiedRational): Boolean = this.numer * s.denom < s.numer * this.denom
  def max(s: UnsimplifiedRational): UnsimplifiedRational = if (this < s) s else this

  def makeString(): String = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}

// testing
def a() = {
  def f() = println("abcabc")
  f
}

a()