import week03.{homework, _}
object nth {
  def nth[T](n:Int,xs:List[T]):T = {
    if(xs.isEmpty) throw IndexOutOfBoundsException
    else if(n==0) xs.head
    else nth(n-1,xs.tail)
  }
  val list = new homework.Cons[Int](1, new homework.Cons[Int](2, new homework.Cons[Int](3, new Nil)))
}