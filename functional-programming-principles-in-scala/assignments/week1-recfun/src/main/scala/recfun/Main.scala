package recfun

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
    def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case (c, r) if c == r => 1
      case (c, r) => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check(count: Int, chars: List[Char]): Boolean = (count, chars) match {
        case (c, _) if c < 0 => false
        case (_, ch) if ch.isEmpty => true
        case (c, ch) => {
          ch.head match {
            case '(' => check(c + 1, ch.tail)
            case ')' => check(c - 1, ch.tail)
            case _ => check(c, ch.tail)
          }
        }
      }
      check(0, chars)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (m, _) if m < 0 => 0
    case (_, c) if c.isEmpty => 0
    case (m, c) => countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
