package observatory

import java.time.LocalDate

import org.apache.spark.sql.Dataset
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  val year = 2013
  val stationsFile = "/stations.csv"
  val temperaturesFile = s"/$year.csv"

  lazy val stations: Dataset[Station] = Extraction.stations(stationsFile).persist
  lazy val temperatures: Dataset[TempReading] = Extraction.tempReadings(temperaturesFile).persist
  lazy val locatedTemperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
  lazy val yearlyAverage: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(locatedTemperatures)


  test("Extract the stations information from the stations file") {
//    stations.show
    stations.filter((station: Station) => station.id == "035963").show
    assertResult(15)(stations.count())
    assertResult(0)(stations.filter((station: Station) => station.id == "012345").count())
    assertResult(1)(stations.filter((station: Station) => station.id == "035963").count())
    assertResult(1)(stations.filter((station: Station) => station.id == "035963-35048").count())
    assertResult(1)(stations.filter((station: Station) => station.id == "-00004").count())
  }

  test("Extract the temperature information from the temperatures file") {
//    temperatures.show(40)
    temperatures.filter((reading: TempReading) => reading.id == "049999").show
    assertResult(31)(temperatures.count())
    assertResult(0)(temperatures.filter((reading: TempReading) => reading.id == "012345").count())
    assertResult(0)(temperatures.filter((reading: TempReading) => reading.id == "049999").count())
    assertResult(3)(temperatures.filter((reading: TempReading) => reading.id == "049999-00003").count())
    assertResult(1)(temperatures.filter((reading: TempReading) => reading.id == "-00008").count())
    assertResult(1)(temperatures.filter((reading: TempReading) => reading.id == "049999-00003"
      && reading.month == 10
      && reading.day == 3
      && reading.temperature == (32.5 - 32) / 1.8).count())
  }

  test("Located temperatures") {
//    locatedTemperatures.take(30) foreach println
    assertResult(28)(locatedTemperatures.size)
  }

  test("Calculate yearly average for every location") {
//    yearlyAverage foreach println
    assertResult(13)(yearlyAverage.size)
    assertResult(0.5555555555555556)(yearlyAverage.filter(_._1 == Location(72.58, -38.46)).head._2)
  }

}
