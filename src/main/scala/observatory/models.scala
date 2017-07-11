package observatory

import java.net.URI
import java.time.LocalDate

import com.sksamuel.scrimage.Pixel

import scala.math._


case class Location(lat: Double, lon: Double) {
  lazy val point = Point(lat.toRadians, lon.toRadians)
}

case class Point(latRad: Double, lonRad: Double) {
  lazy val location = Location(latRad.toDegrees, lonRad.toDegrees)

  /**
    * Added for special case: https://www.coursera.org/learn/scala-capstone/discussions/weeks/2/threads/
    *
    * @param other Point
    * @return      distance on Earth in meters
    */
  def haversineEarthDistance(other: Point): Double =
    Visualization.earth_radius * greatCircleDistance(other) * 1000

  /**
    * Δσ = arccos( sin φ1 * sin φ2 + cos φ1 * cos φ2 * cos (Δλ))
    *
    * Source: http://www.movable-type.co.uk/scripts/latlong.html
    * a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
    *
    * @param other Point
    * @return      The distance between both points
    */
  def greatCircleDistance(other: Point): Double = {
    val latDistance = abs(other.latRad - latRad)
    val lonDistance = abs(other.lonRad - lonRad)

    val a = pow(sin(latDistance / 2), 2) + cos(latRad) * cos(other.latRad) * pow(sin(lonDistance / 2), 2)
    val fixedA = if (a > 1) 1 else if (a < -1) -1 else a

    2 * atan2(sqrt(fixedA), sqrt(1 - fixedA))
  }

}

case class Color(red: Int, green: Int, blue: Int) {
  def toPixel(alpha: Int = 255): Pixel = Pixel(red, green, blue, alpha)
}

case class Station(id: String, lat: Double, lon: Double)

case class TempReading(id: String, month: Int, day: Int, temperature: Double)

case class StnTempReading(id: String, month: Int, day: Int, temperature: Double, lat: Double, lon: Double)

case class LocatedTemperature(date: ReadingDate, location: Location, temperature: Double)

case class ReadingDate(year: Int, month: Int, day: Int) {
  def toLocalDate: LocalDate = LocalDate.of(year, month, day)
}

/**
  * Source: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Mathematics
  *
  * @param x    X coordinate
  * @param y    Y coordinate
  * @param zoom Zoom level
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  def toLocation = Location(
    atan(sinh(Pi * (1.0 - 2.0 * y / (1 << zoom)))).toDegrees,
    x / (1 << zoom) * 360 - 180
  )

  def toURI = new URI(s"http://tile.openstreetmap.org/$zoom/$x/$y.png")
}
