class Rational(x: Int, y: Int) {
  def numer: Int = x
  def denom: Int = y

  def +(s: Rational): Rational =
    new Rational(
      this.numer * s.denom + s.numer * this.denom,
      this.denom * s.denom
    )
  def neg: Rational = new Rational(-numer, denom)
  def -(s: Rational): Rational =
    this + s.neg
  def *(s: Rational): Rational =
    new Rational(
      this.numer * s.numer,
      this.denom * s.denom
    )
}

def makeString(r: Rational): String =
  r.numer + "/" + r.denom

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numer
x.denom

makeString(x + y)
makeString(y - x)

makeString(x + y * z)
makeString(x - y - z)

