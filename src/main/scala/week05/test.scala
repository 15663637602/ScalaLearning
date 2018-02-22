package week05

/**
  * Created by Yuqi Li on 2018/2/13.
  */
object test extends App{
  def removeAt(n:Int,xs:List[Int]):List[Int] = {
    if(n+1 > xs.length) throw new Exception("out of bound")
    else if (n+1 == xs.length) xs.init
    else removeAt(n,xs.init) ++ List(xs.last)
  }
  println("asd")
}
