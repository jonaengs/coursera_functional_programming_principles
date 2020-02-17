def sum(f: Int => Int)(a: Int, b: Int): Int ={
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x)(1, 100)

def sumn(f: Int => Int)(n: Int)(a: Int, b:Int): Int =
  if (a > b) 0 else f(a) * n + sumn(f)(n)(a + 1, b)

sumn(x => x*x)(3)(1, 10)
