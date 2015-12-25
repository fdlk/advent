object day25 {

  def index(row: Int, col: Int): Int = {
    val n = row + col - 1
    val topRight = n * (n + 1) / 2
    topRight - row
  }

  def ticket(index: Int): BigInt = 20151125 * BigInt(252533).modPow(index, 33554393) % 33554393

  ticket(index(2981, 3075))
}

