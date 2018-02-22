package week04.sort

/**
  * Created by Yuqi Li on 2018/2/13.
  */
object Sort {
  // 每趟排序，取一个数为基准，先从后往前，再从前往后，使该数左边的数都小于这个数，右边的数都大于这个数
  def quickS(in: List[Int]): List[Int] = {
    if (in.length < 2) in
    else quickS(in.filter(_ < in.head)) ++ in.filter(_ == in.head) ++ quickS(in.filter(_ > in.head))
  }

  //因为排序跟第一个元素关系密切，所以用case y::ys 的形式
  def insertS(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, insertS(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: y :: ys else y :: insert(x, ys)
  }

  def msort[T](xs:List[T])(lt: (T,T) => Boolean):List[T] = {
    val n = xs.length / 2
    if(n == 0) xs
    else{
      def merge(xs:List[T],ys:List[T]):List[T] = (xs,ys) match{
        case (Nil,ys) => ys
        case (xs,Nil) => xs
        case (x::xs1,y::ys1) =>
          if(lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)
      }
      val (fst,snd) = xs.splitAt(n)
      merge(msort(fst)(lt),msort(snd)(lt))
    }
  }

  def msort1[T](xs:List[T])(implicit ord:Ordering[T]):List[T] = {
    val n = xs.length / 2
    if(n == 0) xs
    else{
      def merge(xs:List[T],ys:List[T]):List[T] = (xs,ys) match{
        case (Nil,ys) => ys
        case (xs,Nil) => xs
        case (x::xs1,y::ys1) =>
          if(ord.lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)
      }
      val (fst,snd) = xs.splitAt(n)
      merge(msort1(fst),msort1(snd))
    }
  }

  def msort2(xs:List[Int]):List[Int] = {
    val n = xs.length / 2
    if(n == 0) xs
    else{
      def merge(xs:List[Int],ys:List[Int]):List[Int] = xs match{
        case List() => ys
        case x::xs1 => ys match {
          case List() => xs
          case y::ys1 => if(x < y) x :: merge(xs1,ys) else y:: merge(xs,ys1)
        }
      }
      val (fst,snd) = xs.splitAt(n)
      merge(msort2(fst),msort2(snd))
    }
  }


  def main(args: Array[String]): Unit = {
    val list = List(3,5,2,1,6,99,65,43,23,55,12)
    val fruits = List("apple","orange","pineapple","banana")
    println(msort1(list))
    println(msort1(fruits))

  }


}
