package week03

import java.util.NoSuchElementException

/**
  * Created by Yuqi Li on 2018/2/6.
  */
trait List[T] {
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
}
//val evaluated at first time it is initialized, while def evaluated each time it is referenced
class Cons[T](val head:T,val tail:List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}
class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  //Nothing is subtype of any other type
  def head : Nothing= throw new NoSuchElementException("Nil.head")
  def tail : Nothing= throw new NoSuchElementException("Nil.tail")
}

object nth {
  def nth[T](n:Int,xs:List[T]):T = {
    if(n==0) xs.head
    else nth(n-1,xs.tail)
  }
  val list = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil)))
}