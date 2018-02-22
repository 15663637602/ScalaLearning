
object pairs {
  def isPrime(n:Int):Boolean = {
    (2 until n) forall (n % _ != 0)
  }
  val n = 7

  for {
    i <- 1 until 10
    j <- 2 until 9
    if i >= 8
  }yield List(i,j)

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i,j))) filter (pair =>
    isPrime(pair._1 + pair._2))

  def scalarProduct(xs:List[Double],ys:List[Double]):Double = {
    (for (
      (x,y) <- xs zip ys
    ) yield x*y).sum
  }

}