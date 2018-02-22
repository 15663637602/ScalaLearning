object week05{
  def removeAt(n:Int,xs:List[Int]):List[Int] = {
    if(n+1 > xs.length) throw new Exception("out of bound")
    else if (n+1 == xs.length) xs.init
    else removeAt(n,xs.init) ++ List(xs.last)
  }
  def removeAt2(n:Int,xs:List[Int]):List[Int] = (xs take n) ::: (xs drop n+1)
  println(removeAt2(1,List(1,2,3,4,5,6)))

  val nums = List(2,-4,5,7,1)
  val fruits = List("apple","pineapple","orange","banana")

  nums filter (x=>x>0)
  nums filterNot (x=>x>0)
  nums partition (x=>x>0)

  //prefix
  nums takeWhile(x=>x>0)
  //opposite
  nums dropWhile(x=>x>0)
  //comibe the prefix and its opposite
  nums span(x=>x>0)

  val data = List("a","a","a","b","c","c","a")
  def pack[T](xs:List[T]):List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first,rest) = xs span (y => y == x)
      first :: pack(rest)
  }
  pack(data)
  def encode[T](xs:List[T]):List[(T,Int)] = {
    pack(xs) map (ys => (ys.head,ys.length))
  }
  encode(data)

  def concat[T](xs:List[T],ys:List[T]):List[T] = {
    (xs foldRight ys )(_ :: _)
  }

  def mapFun[T,U](xs:List[T],f: T => U):List[U] = {
    (xs foldRight List[U]())(f(xs.last))
  }
}