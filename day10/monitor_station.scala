// execute with: scala monitor_station.scala input

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

object Sweep {
  def clockwise(points : List[List[Point]]) : List[Point] =
    if(points.isEmpty) {
      List[Point]()
    } else {
      points.map { _.head } ::: clockwise(
        points.map { _.tail }.filter { !_.isEmpty }
      )
    }
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

  val ordered = points.groupBy { x => source.angle(x) }.
    toSeq.
    map { case (k, v) => (k, v.sortBy { x => source.distance(x) }) }.
    sortBy { case (k, _) => k }

  val northIdx = ordered.indexWhere { case (k, _) => k >= -math.Pi / 2 }
  println(northIdx + " " + ordered(northIdx))

  val orderedPoints = ordered.map { case(_, v) => v }.toList

  val clockwiseLists = orderedPoints.drop(northIdx) ::: orderedPoints.take(northIdx)

  val clockwise = Sweep.clockwise(clockwiseLists)

  //clockwise.zipWithIndex.foreach { case (x, i) => println(List(i+1, x, source.angle(x), source.distance(x))) }
  val hit = clockwise(200-1)
  println(hit + " " + (hit.x * 100 + hit.y))
}
