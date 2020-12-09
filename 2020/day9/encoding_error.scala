// usage: scala encoding_error.scala example

import scala.io.Source

object EncodingError {
  def checksum(window: List[Long], goal: Long) : Boolean = {
    window.indices.dropRight(1).exists { base =>
      window.drop(base).exists { x =>
        window(base) + x == goal
      }
    }
  }

  def contiguous(window: List[Long], goal: Long) : List[Long] = {
    window.indices.find { n =>
      val chunk = window.drop(n).takeWhile( {
        var total = 0L;
        x => {
          total += x
          total <= goal
        }
      })

      if(chunk.sum == goal) {
        return chunk
      }
      chunk.sum == goal
    }
    return List()
  }

  def main(args: Array[String]) {
    val numbers = Source.fromFile(args(0)).getLines.map { _.toLong }.toList
    val preamble = args(1).toInt

    for(i <- preamble to numbers.length - 1) {
      val discarded = i - preamble;
      val preceding = numbers.drop(discarded).take(preamble)
      if(!checksum(preceding, numbers(i))) {
        println("Checksum Error: " + numbers(i))

        val chunk = contiguous(numbers.take(i), numbers(i))
        println("Weakness: " + (chunk.min + chunk.max))
      }
    }
  }
}
