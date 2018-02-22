package week04

/**
  * Created by Yuqi Li on 2018/2/8.
  */
trait List[T] {
  def isEmpty:Boolean
  def head: T
  def tail: List[T]
}
class Cons[T](val head: T, val tail:List[T]) extends List[T] {
  def isEmpty: Boolean = false
}
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}
object List {
  //List(1,2)  ===  List.apply(1,2)
  def apply[T](x1:T,x2:T): List[T] = new Cons[T](x1,new Cons[T](x2,new Nil))
  def apply[T]() : List[T] = new Nil[T]
  def apply[T](x:T):List[T] = new Cons[T](x,new Nil)
}