package week04

/**
  * Created by Yuqi Li on 2018/2/12.
  */
trait Expr
case class Number(n:Int) extends Expr
case class Sum(e1:Expr,e2:Expr) extends Expr

object Exprs {
  def show(e:Expr):String = e match{
    case Number(x) => x.toString
    case Sum(e1,e2) => show(e1) + "+" + show(e2)
  }

  def main(args: Array[String]): Unit = {
    println(show(Sum(Number(1),Number(4))))
  }
}
