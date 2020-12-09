// usage: scala encoding_error.scala example

import scala.io.Source

object H {
  def sum(preceding: List[Long], goal: Long) : Boolean = {
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
}

object EncodingError extends App {
  val numbers = Source.fromFile(args(0)).getLines.map { _.toLong }.toList
  val preamble = args(1).toInt

  for(i <- preamble to numbers.length - 1) {
    val discarded = i - preamble;
    val preceding = numbers.drop(discarded).take(preamble)
    if(!H.sum(preceding, numbers(i))) {
      println("First Star: " + numbers(i))
    }
  }
}
