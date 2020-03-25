package week4

// lecture 4.1
class Boolean {
  abstract class Boolean {
    def ifThenElse[T](t: => T, e: => T): T

    def && (x: Boolean): Boolean = ifThenElse(x, _false)
    def || (x: Boolean): Boolean = ifThenElse(_true, x)
    def unary_! : Boolean = ifThenElse(_false, _true)

    def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
    def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
    def < (x: Boolean): Boolean = ifThenElse(_false, x)
  }

  object _true extends Boolean {
    def ifThenElse[T](t: => T, e: => T): T = t
  }

  object _false extends Boolean {
    def ifThenElse[T](t: => T, e: => T): T = e
  }
}


