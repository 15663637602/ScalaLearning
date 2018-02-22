package week04

/**
  * Created by Yuqi Li on 2018/2/7.
  */
abstract class Nat {
  def isZero:Boolean
  def predecessor:Nat
  def successor:Nat = new Succ(this)
  def +(that:Nat):Nat
  def -(that:Nat):Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")


  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) Zero else throw new Error("negative")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n


  //递归,n是前一个数值
  //n + that 会得到一个比Succ(n) + that 小一个数的值，所以要用new Succ(n + that)
  override def +(that: Nat): Nat = {
    println("n: "+n.toString)
    val x = new Succ(n + that)
    println("x: "+x)
    x
  }

  //如果用 new Succ(n - that) 那么会到throw Error("negative") 所以n要减that的前一个值，n又是Succ(n)的前一个值，所以抵消了，得到的即为正确答案
  override def -(that: Nat): Nat = if(that.isZero) n else n - that.predecessor
}
object test{
  def main(args: Array[String]): Unit = {
    val zero = Zero
    println(zero)
    println("--------------")
    val one = new Succ(zero)
    val two = new Succ(one)
    val three = new Succ(two)
    val five = two + three
    println("----------------------")
    val eight = three + five
    val seven = eight.predecessor
    val six = seven.predecessor
    println(five , six.predecessor)
    println("--------------")
    val twelve = seven + five
  }
}