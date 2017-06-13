package observatory

import java.time.LocalDate

import com.sksamuel.scrimage.Pixel

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

