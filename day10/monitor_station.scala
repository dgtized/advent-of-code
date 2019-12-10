import scala.io.Source

object MonitorStation extends App {
  val lines = Source.fromFile(args(0)).getLines.toList

  lines.foreach { line =>
    println(line)
  }
}
