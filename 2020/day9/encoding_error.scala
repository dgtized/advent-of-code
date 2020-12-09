// usage: scala encoding_error.scala example

import scala.io.Source

object H {
  def checksum(preceding: List[Long], goal: Long) : Boolean = {
    for(base <- 0 to preceding.length - 2) {
      val a = preceding(base)
      for(ahead <- base + 1 to preceding.length - 1) {
        if(a + preceding(ahead) == goal) {
          return true
        }
      }
    }
    return false
  }

  def contiguous(numbers: List[Long], goal: Long, offset: Int) : Unit = {
    for(n <- 0 to offset - 2 ) {
      val base = numbers.drop(n)
      var total : Long = 0
      for(u <- n + 1 to offset - 1) {
        val chunk = base.take(u - n)
        val sum = chunk.sum
        if(sum == goal) {
          println(chunk)
          println("Weakness: " + (chunk.min + chunk.max))
        }
      }
    }
  }
}

object EncodingError extends App {
  val numbers = Source.fromFile(args(0)).getLines.map { _.toLong }.toList
  val preamble = args(1).toInt

  for(i <- preamble to numbers.length - 1) {
    val discarded = i - preamble;
    val preceding = numbers.drop(discarded).take(preamble)
    if(!H.checksum(preceding, numbers(i))) {
      println("Checksum Error: " + numbers(i))

      H.contiguous(numbers, numbers(i), i)
    }
  }
}
