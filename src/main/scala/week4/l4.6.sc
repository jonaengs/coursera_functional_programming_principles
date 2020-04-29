trait Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

case class Var(x: String) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Product(e1, e2) => (e1, e2) match {
    case (Sum(_, _), Sum(_, _)) => "(" + show(e1) + ")" +  " * " + "(" + show(e2) + ")"
    case (Sum(_, _), _) => "(" + show(e1) + ")" +  " * " + show(e2)
    case (_, Sum(_, _)) => show(e1) + " * " + "(" + show(e2) + ")"
    case _ => show(e1) + " * " + show(e2)
  }
  case Var(x) => x
}



val x = Var("x")
val sum1 = Sum(x, Number(2))
val prod0 = Product(x, Number(3))
val prod1 = Product(sum1, Number(1))
val prod2 = Product(sum1, prod1)
val prod3 = Product(prod1, sum1)
val prod4 = Product(prod0, prod3)
val prod5 = Product(sum1, sum1)

show(prod0)
show(prod1) // should display (x + 2) * 1
show(prod2)
show(prod3)
show(prod4)
show(prod5)