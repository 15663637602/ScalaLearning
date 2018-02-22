package week02

/**
  * Created by Yuqi Li on 2018/2/5.
  */
object Rational extends App{
  val x = new Rational(1,2)
  val y = new Rational(2,3)

  println(x+y)

  val a = new Rational(1,3)
  val b = new Rational(5,7)
  val c = new Rational(3,2)
  val n = a + (b) - (c)
  println(n)

  println(b + (b))
  println(a < b)
  println(x max y)
}
class Rational(x:Int,y:Int) {
  require(y != 0, "denominator must be nonzero")
  def this(x:Int) = this(x,1)
  private def gcd(a:Int,b:Int):Int = {
    if(b == 0) a
    else gcd(b,a%b) //21,14
  }
  val numer = x
  val denom = y
  def < (that:Rational):Boolean = {
    numer*that.denom < denom*that.numer
  }
  def max(that:Rational):Rational = {
    if(this < that) that
    else this
  }
  def + (that:Rational):Rational = {
    new Rational(numer*that.denom+denom*that.numer,denom*that.denom)
  }
  def unary_- : Rational = new Rational(-numer,denom)

  def - (that:Rational):Rational = {
    this + -that
  }

  override def toString ={
    val g = gcd(numer,denom)
    numer/g + "/" + denom/g
  }
}