package week01

/**
  * Created by Yuqi Li on 2018/2/4.
  */
object Recursive {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance(")(asdwdasd)(".toList))
    //println(countChange100())
    println(countChange(12, List(5,2,1)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 1) 1
    else if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def verify(c: List[Char], n: Int): Int = {
      if (n < 0) -1
      else {
        if (c.isEmpty) n
        else if (c.head.toString == "(") verify(c.tail, n + 1)
        else if (c.head.toString == ")") verify(c.tail, n - 1)
        else verify(c.tail, n)
      }
    }

    if (verify(chars, 0) == 0) true
    else false
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money,coins.tail)
  }

  def countChange100()= {
    for {
      a <- (0 to 2)
      b <- (0 to 5)
      c <- (0 to 10)
      d <- (0 to 20)
      e <- (0 to 100)
      if((50 * a + 20 * b + 10 * c + 5 * d + 1 * e) == 100)
    }yield (a,b,c,d,e)
  }
}
