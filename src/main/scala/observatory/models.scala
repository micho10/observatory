package observatory

import java.net.URI
import java.time.LocalDate

import com.sksamuel.scrimage.Pixel

import scala.math._


case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int) {
  def toPixel(alpha: Int = 255): Pixel = Pixel(red, green, blue, alpha)
}

case class Station(id: String, lat: Double, lon: Double)

case class TempReading(id: String, month: Int, day: Int, temperature: Double)

case class StnTempReading(id: String, month: Int, day: Int, temperature: Double, lat: Double, lon: Double)

case class LocatedTemperature(date: ReadingDate, location: Location, temperature: Double)

case class ReadingDate(year: Int, month: Int, day: Int) {
  def toLocalDate = LocalDate.of(year, month, day)
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
