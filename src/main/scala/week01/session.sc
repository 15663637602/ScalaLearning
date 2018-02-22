object session {
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def abs(d: Double) = {
      if (d < 0) -d
      else d
    }

    def isGoodEnough(guess: Double) = {
      abs(guess * guess - x) / x < 0.001
    }

    def improve(guess: Double): Double = {
      (guess + x / guess) / 2
    }
    sqrtIter(1.0)
  }
  sqrt(2.0)
  sqrt(4.0)
  sqrt(1e-6)
  sqrt(1e60)

  @annotation.tailrec
  def gcd(a:Int, b:Int):Int = {
    if(b == 0) a
    else gcd(b,a%b)
  }
  gcd(14,21)

  def factorial(n:Int):Int = {
    @annotation.tailrec
    def loop(n:Int,acc:Int):Int = {
      if(n<=0) acc
      else loop(n-1,n*acc)
    }
    loop(n,1)
  }
  factorial(4)

}