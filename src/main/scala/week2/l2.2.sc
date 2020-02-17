def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def id(x: Int): Int = x
def factorial(a: Int): Int = product(id)(1, a)
// could also be written "product(x => x)(1, a)"

// called mapReduce in lecture
def reduce(f: Int => Int)(operator: (Int, Int) => Int, base: Int)(a:Int, b: Int): Int =
  if (a > b) base else operator(f(a), reduce(f)(operator, base)(a + 1, b))

def multiply(a: Int, b: Int): Int = a * b
def fact_2(a: Int): Int = reduce(id)(multiply, 1)(1, a)

def fact_3(a: Int): Int = reduce(id)(_*_, 1)(1, a)


factorial(5)
fact_2(5)
fact_3(5)
// should all eval to 120

// sum squares of numbers from 3 to 5 (= 9 + 16 + 25 = 50)
reduce(x => x * x)(_+_, 0)(3, 5)

// we can also define product() in terms of reduce:
def product2(f: Int => Int)(a: Int, b: Int): Int =
  reduce(f)(_*_, 1)(a, b)

def factorial_reduced(a: Int): Int = product2(id)(1, a)
factorial_reduced(5) // 120
