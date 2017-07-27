package observatory

import observatory.Interaction._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  // Test values
  private val year = 1975
  private val stationsFile = "/stations50.csv"
  private val temperaturesFile = s"/$year.csv"
  private lazy val temperatures = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
  private lazy val yearlyAverage = Extraction.locationYearlyAverageRecords(temperatures)


  test("Tile location 1") {
    assertResult(Location(90, -180))(tileLocation(0, 0, 0))
  }

  test("Tile location 2") {
    assertResult(Location(84.7383871209534, -176.484375))(tileLocation(10, 10, 10))
  }

  test("Tile location 3") {
    assertResult(Location(-89.99999212633796, 145))(tileLocation(5, 100, 100))
  }


}
