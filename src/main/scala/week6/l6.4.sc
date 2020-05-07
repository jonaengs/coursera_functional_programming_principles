
class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly): Poly =
    new Poly(terms ++ (other.terms map fitTerm))

  def fitTerm(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case None => exp -> coeff
      case Some(coeff1) => exp -> (coeff + coeff1)
    }
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2


class _Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0

  /*
  def + (other: _Poly): _Poly = new _Poly(terms ++ other.terms map adjust)
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
   */

  def + (other: _Poly): _Poly =
    new _Poly((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new _Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new _Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2

Map(1 -> 2, 3 -> 4) + (5 -> 6)