package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c,r) match {
     case (0, _) => 1
     case (c, r)  if (c==r) => 1
     case (c,r) => pascal(c,r-1)+pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def bal(chars: List[Char], count: Int) : Boolean = {
      if (chars.isEmpty && count == 0) true
      else if (chars.isEmpty && count != 0) false
      else if (count < 0) false
      else if (chars.head == ')') bal(chars.tail, (count-1))
      else if (chars.head == '(') bal(chars.tail, (count+1))
      else bal(chars.tail, count)
    }
    bal(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
     case (0, _) => 1
     case (m, _) if m < 0 => 0
     case (_, cs) if cs.isEmpty => 0
     case (m, cs) => countChange(m - cs.head, cs) + countChange(m, cs.tail) 
  }
}
