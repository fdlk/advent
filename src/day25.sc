import scala.annotation.tailrec
object day25 {
  def index(row: Int, col: Int): Int = {
    val n = row + col - 1
    val topRight = n * (n + 1) / 2
    topRight - row
  }
  val indexOfTicket = index(2981, 3075)
  def modular_pow(base: Long, exponent: Int, modulus: Long): Long = {
    @tailrec
    def modular_pow_internal(base_rec: Long, exponent_rec: Int, result: Long): Long = {
      if(exponent_rec == 0) result
      else {
        modular_pow_internal( base_rec * base_rec % modulus, exponent_rec >> 1,
          if (exponent_rec % 2 == 1) result * base_rec % modulus else result)
      }
    }
    if (modulus == 1) 0 else {
      modular_pow_internal(base, exponent, 1)
    }
  }
  def ticket(index: Int): Long = {
    (20151125 * modular_pow(252533, index, 33554393)) % 33554393
  }
  ticket(indexOfTicket)
}

