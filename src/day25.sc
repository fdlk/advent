val input = common.loadPackets(List("day25.txt"))

def index(row: Int, col: Int): Int = {
  val n = row + col - 1
  val topRight = n * (n+1) / 2
  topRight - row + 1
}

def nextValue(value: BigInt): BigInt = {
  (value * 252533) % 33554393
}

def ticket(index:Int): BigInt = {
  Stream.iterate(BigInt(20151125))(nextValue).drop(index-1).head
}

val row = 2981
val col = 3075
ticket(index(row, col))
