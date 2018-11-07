package recfun

import java.security.KeyStore.TrustedCertificateEntry

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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }

  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {

        def recur(chars: List[Char], num_parentheses: Int): Int = {
          if(chars.isEmpty || num_parentheses <= -1) num_parentheses
          else if (chars.head == '('){
            recur(chars.tail, num_parentheses+1)
          }
          else if (chars.head == ')') {

            recur(chars.tail, num_parentheses-1)
          }
          else recur(chars.tail, num_parentheses)
        }
      val a = recur(chars, 0)
      a == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def recur(money_left: Int, coins_left: List[Int]): Int ={
        if(money_left == 0) 1
        else if (money_left < 0) 0
        else if (coins_left.isEmpty) 0
        else{
          recur(money_left-coins_left.head, coins_left) + recur(money_left, coins_left.tail)
        }

      }

      recur(money, coins)
    }
}

