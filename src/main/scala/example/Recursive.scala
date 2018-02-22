package example

/**
  * Created by Yuqi Li on 2018/1/30.
  */
object Recursive extends App{
  def factorial(n:Int):Int = {
    if (n<=0) 1
    else n*factorial(n-1)
  }

  @annotation.tailrec
  def factorial2(n:Int,x:Int):Int = {
    if(n<=0) x
    else factorial2(n-1,n*x)
  }

  val x = factorial2(5,1)
  println(x)
  println("--------------------------")

  def sum(f:Int=>Int)(a:Int)(b:Int):Int = {
    @annotation.tailrec
    def loop(n:Int,acc:Int):Int ={
      if(n<a) acc
      else loop(n-1,acc+f(n))
    }
    loop(b,0)
  }
  val s = sum(x => x*x)(2)(5)
  println(s)
  println("------------------------")
  def qSort(in:List[Int]):List[Int] = {
    if(in.length<=1) in
    else qSort(in.filter(x=>x<in.head)) ++ in.filter(x=>x == in.head) ++ qSort(in.filter(x=>x < in.head))
  }

  def sumSq(in:List[Double]):(Int,Double,Double) = {
    in.foldLeft(0,0.0,0d)((t,v)=>(t._1+1, t._2+v, t._3+v*v))
  }
  println(sumSq(List(1.0,2.0,3.0)))

}
