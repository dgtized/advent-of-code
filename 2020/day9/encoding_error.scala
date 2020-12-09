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

    numbers.sliding(preamble + 1, 1).foreach { window =>
      val current = window.last
      if(!checksum(window.dropRight(1), current)) {
        println("Checksum Error: " + current)

        val chunk = contiguous(numbers.takeWhile(_ < current), current)
        println("Weakness: " + (chunk.min + chunk.max))

        return
      }
    }
  }
}
