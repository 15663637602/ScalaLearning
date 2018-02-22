package week06

/**
  * Created by Yuqi Li on 2018/2/22.
  */
object polynomials {
  class Poly(val terms: Map[Int,Double]){
    def + (other:Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(terms:Map[Int,Double])

    override def toString: String =
      (for {
        (exp,coeff) <- terms.toList.sorted.reverse
      }yield coeff+"x^"+exp) mkString " + "
  }

  def main(args: Array[String]): Unit = {
    val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  }

}
