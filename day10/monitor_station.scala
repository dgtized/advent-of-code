import scala.io.Source
import scala.math

case class Point(x: Int, y: Int) {
  def angle(point: Point): Double =
    math.atan2(
      (point.y - this.y).toDouble,
      (point.x - this.x).toDouble
    )

  def visible(points : List[Point]) : Integer =
    points.filter { point => point != this }.groupBy { point =>
      this.angle(point)
    }.size

  def distance(point: Point): Double =
    math.sqrt(math.pow(point.x - this.x, 2) +
      math.pow(point.y - this.y, 2))
}



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

  println

  val source = points.maxBy { source => source.visible(points) }
  println(source + " " + source.visible(points))

  println

  ordered = points.groupBy { x => source.angle(x) }.
    toSeq.
    map { case (k, v) => (k, v.sortBy { x => source.distance(x) }) }.
    sortBy { case (k, v) => k }
}
