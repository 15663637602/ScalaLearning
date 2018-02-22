package week06

/**
  * Created by Yuqi Li on 2018/2/21.
  */
object NQueensQuestion {
  def nQueens(n:Int):Set[List[Int]] = {
    def placeQueen(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else for {
        queens <- placeQueen(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
    }
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }
    placeQueen(n)
  }

    def show(queens:List[Int]) = {
      val lines = for (col <- queens.reverse) yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
      println(lines.mkString("\n"))
    }

    def main(args: Array[String]): Unit = {
      val queens = nQueens(4)
      queens.map(show)
    }


}
