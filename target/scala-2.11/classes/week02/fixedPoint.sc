import math.abs

object fixedPoint{
  val tolerance = 0.0001
  def isCloseEnough(x:Double,y:Double):Boolean = {
    abs((x - y) / x) / x < tolerance
  }
  def fixedPoint(f:Double => Double)(firstGuess:Double):Double = {
    def iterate(guess:Double):Double = {
      val next = f(guess)
      if(isCloseEnough(guess,next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1 + x/2)(1)

  def sqrt(x:Double) = fixedPoint(y => (y+ x / y) /2 )(1.0)

  def averageDamp(f:Double => Double)(x:Double) = (f(x) + x) / 2 //如果你传入一个f，
                                                                 // 会得到一个函数,这个函数传入x值，返回一个函数

  def sqrt2(x:Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  sqrt(2)

  sqrt2(2)
}