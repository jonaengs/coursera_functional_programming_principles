val tolerance = 1e-4

def isCloseEnough(x: Double, y: Double) =
  math.abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess) // define as var since used twice
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

// should be ~2
fixedPoint(x  => 1 + x/2)(1)


def sqrt(x: Double): Double =
  fixedPoint(y => x / y)(1)
// sqrt(2) <- never terminates

def improved_sqrt(x: Double): Double =
  fixedPoint(y => (y + x / y) / 2)(1.0)
improved_sqrt(2)

def averageDamp(f: Double => Double)(x: Double): Double =
  (x + f(x))/2


def final_sqrt(x: Double): Double =
  // note how this is almost the same as the first definition of sqrt() above, only with dampening of the function
  fixedPoint(averageDamp(y => x/y))(1.0)

final_sqrt(2)