// usage: scala encoding_error.scala example

import scala.io.Source

object EncodingError {
  def checksum(window: List[Long], goal: Long) : Boolean = window match {
    case Nil => false
    case head :: rest =>
      if(rest.exists( _ + head == goal ))
        return true
      else
        checksum(rest, goal)
  }

  def contiguous(window: List[Long], goal: Long) : List[Long] =
    window match {
      case Nil => List()
      case _ =>
        val chunk = window.takeWhile( {
          var total = 0L;
          x => {
            total += x
            total <= goal
          }
        })

        if(chunk.sum == goal)
          return chunk

        contiguous(window.drop(1), goal)
    }

  def main(args: Array[String]) {
    val numbers = Source.fromFile(args(0)).getLines.map { _.toLong }.toList
    val preamble = args(1).toInt

    val invalid = numbers.
      sliding(preamble + 1, 1).
      find ({ window => !checksum(window.dropRight(1), window.last) }).get.
      last

    println("Checksum Error: " + invalid)

    val chunk = contiguous(numbers.takeWhile(_ < invalid), invalid)
    println("Weakness: " + (chunk.min + chunk.max))
  }
}
