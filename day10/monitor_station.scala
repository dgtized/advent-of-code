import scala.io.Source

case class Point(x: Int, y: Int)

object MonitorStation extends App {
  val lines = Source.fromFile(args(0)).getLines.toList
  var points = List[Point]()

  var y : Int = 0
  lines.foreach { line =>
    println(line)

    for(x <- 0 to line.length-1) {
      if(line(x) == '#') {
        points = Point(x, y) :: points
      }
    }
    y = y + 1
  }

  points.foreach { point =>
    println(point)
  }
}
