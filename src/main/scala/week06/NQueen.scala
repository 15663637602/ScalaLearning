package week06

/**
  * Created by Yuqi Li on 2018/2/21.
  */
object NQueen {

  def reverse[T](in:List[T]):List[T] = {
    var result : List[T] = Nil
    var these = in
    while(in.nonEmpty){
      result = these.head :: result
      these = these.tail
    }
    result
  }

  def isSafe(col: Int, queens: List[Int]) = {
    val row = queens.length
    // List(pairs)
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def queens(n:Int):Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }

    placeQueens(n)
  }


  def show(queens:List[Int]) = {
    // for loop generates a List[Vecotr[Int]], mkString( convert collection type to String) --> List[String], separate by a defined charactor
    val lines = for (col <- queens.reverse) yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString

    // String separate by new line
    println(lines.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    queens(4) map show mkString "\n"
    (queens(8) take 3 map show) mkString "\n"
  }
}
