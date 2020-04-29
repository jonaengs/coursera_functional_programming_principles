trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr  // op as in operand, not operation
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  override def isNumber: Boolean = true
  override def isSum: Boolean = false
  override def numValue: Int = n
  override def leftOp: Expr = throw new Error("number has no leftop")
  override def rightOp: Expr = throw new Error("number has sno rightop")
}

class Sum(e1: Expr, e2: Expr) extends Expr { // Sum(e1, e2) ==> e1 + e2.
  def isNumber = false
  def isSum = true
  def numValue = throw new Error("sum has no num value")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression " + e)
}

