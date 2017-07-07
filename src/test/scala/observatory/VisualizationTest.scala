package observatory


import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import observatory.Visualization._
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  // Test values
  private val year = 1975
  private val stationsFile = "/stations50.csv"
  private val temperaturesFile = s"/$year.csv"
  private lazy val temperatures = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
  private lazy val yearlyAverage = Extraction.locationYearlyAverageRecords(temperatures)

  private val scale = Seq(
    (60.0,  Color(255, 255, 255)),
    (32.0,  Color(255,   0,   0)),
    (12.0,  Color(255, 255,   0)),
    (0.0,   Color(  0, 255, 255)),
    (-15.0, Color(  0,   0, 255)),
    (-27.0, Color(255,   0, 255)),
    (-50.0, Color( 33,   0, 107)),
    (-60.0, Color(  0,   0,   0))
  )

  test("location yearly average records") {
    yearlyAverage.take(50) foreach println
    assertResult(49)(yearlyAverage.size)
    assertResult(1)(yearlyAverage.count(_._1 == Location(69.976,23.372)))
  }

  test("Predict temperature") {
    val epsilon = 1e-4

    val temperature = predictTemperature(yearlyAverage, Location(18.433,-66.011))
    temperature should be (26.53967 +- epsilon)
//    assertResult(26.539573820395745)(predictTemperature(yearlyAverage, Location(18.433,-66.011)))
  }

  test("greatCircleDistance test zero distance") {
    val distance = greatCircleDistance(Location(-12, 85), Location(-12, 85))
    println(s"distance: $distance")
    println("=========================================")
    assert(distance === 0)
  }

  test("greatCircleDistance test (extreme case 1)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(90,-180), Location(12, -95))
    println(s"distance: $distance")
    println("=========================================")
    distance should be (10010.0 +- epsilon)
  }

  test("color interpolation sorted") {
    val scale = List((0.0, Color(0, 0, 255)))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, -0.5))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, 0.5))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, 0.0))
  }

  test("Color interpolation unsorted") {
    assertResult(Color(255, 255,   0))(interpolateColor(scale,  12))
    assertResult(Color(255, 255, 255))(interpolateColor(scale,  62))
    assertResult(Color(  0,   0,   0))(interpolateColor(scale, -62))
    assertResult(Color(128, 255, 128))(interpolateColor(scale,   6))
  }

  test("Visualize image") {

    val img = visualize(yearlyAverage, scale)
    img.output(new File(s"target/$year-sample.png"))

    assertResult(360 * 180)(img.pixels.length)
  }

  // TODO: Test
//  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double
//  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image

//  def toCoords(x: Int, y: Int): (Int, Int)

  test("Conversion from pos(x, y) to coords(x, y)") {
    assertResult((-180, 90))(toCoords(0, 0))
    assertResult((-90, 40))(toCoords(90, 50))
    assertResult((0, 0))(toCoords(180, 90))
    assertResult((90, -50))(toCoords(270, 140))
    assertResult((179, -89))(toCoords(359, 179))
    assertResult((-180, 90))(toCoords(360, 180))
    assertResult((-179, 89))(toCoords(361, 181))
  }

}
