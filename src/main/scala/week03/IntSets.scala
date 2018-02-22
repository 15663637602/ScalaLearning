package week03

/**
  * Created by Yuqi Li on 2018/2/6.
  */
object IntSets extends App{
  val t1 = new NonEmpty(3,new Empty,new Empty)
  val t2 = t1 incl 4
  println(t1)
  println(t2)
}

abstract class IntSet {
  //add
  def incl(x:Int):IntSet
  //test whether contains
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  def contains(x: Int): Boolean = {
    if(x < elem) left.contains(x)
    else if(x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if(x < elem) new NonEmpty(elem,left incl(x),right)
    else if(x > elem) new NonEmpty(elem,left,right.incl(x))
    else this
  }

  override def toString: String = "{"+left+elem+right+"}"

  // left union right:把element剔除，只留下left和right连着
  //再连上other后，把elem给include上
  override def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }
}

class Empty extends IntSet {
  def contains(x:Int):Boolean = false
  def incl(x:Int):IntSet = new NonEmpty(x,new Empty,new Empty)
  override def toString: String = "."

  override def union(other: IntSet): IntSet = other
}