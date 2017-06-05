package observatory

import java.time.LocalDate

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: String, lat: Double, long: Double)

case class TempReading(id: String, month: Int, day: Int, temperature: Double)

case class StnTempReading(id: String, month: Int, day: Int, temperature: Double, lat: Double, long: Double)

case class LocatedTemperature(date: ReadingDate, location: Location, temperature: Double)

case class ReadingDate(year: Int, month: Int, day: Int) {
  def toLocalDate = LocalDate.of(year, month, day)
}

