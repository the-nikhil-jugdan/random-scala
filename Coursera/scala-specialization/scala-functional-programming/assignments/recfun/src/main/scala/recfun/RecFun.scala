package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(column: Int, row: Int): Int = if column == 0 || column == row then 1 else pascal(column, row - 1) + pascal(column -1 , row -1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 

    def isOpening(char: Char): Boolean = char == '(' || char == '[' || char == '{'

    def isClosing(char: Char): Boolean = char == ')' || char == ']' || char =='}'

    def getClosing(char:Char): Char = if char == '(' then ')' else if char == '[' then ']' else if char == '{' then '}' else ' '

    var string = chars

    def balanceWith(_match: Char): Boolean = 

      if string.isEmpty then _match == ' '
      else {
        val current = string.head
        string = string.tail

        if isClosing(current) then current == _match 
        // if isOpening then balanceWith(getClosing(current)) && balanceWith(_match)
        // else balanceWith(_match)
        //
        else (!isOpening(current) || balanceWith(getClosing(current)) ) && balanceWith(_match)
      }

    balanceWith(' ')


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if money < 0 || coins.isEmpty then 0
    else if money == 0 then 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

