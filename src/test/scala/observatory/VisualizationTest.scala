package observatory


import observatory.Visualization._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

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
//    yearlyAverage.take(50) foreach println
    assertResult(49)(yearlyAverage.size)
    assertResult(1)(yearlyAverage.count(_._1 == Location(69.976,23.372)))
  }

  test("greatCircleDistance test zero distance") {
    val distance = greatCircleDistance(Location(-12, 85), Location(-12, 85))
//    println(s"distance: $distance")
//    println("=========================================")
    assertResult(0)(distance)
  }

  test("greatCircleDistance test lat unit distance") {
    val epsilon = 1e-4
    val distance = greatCircleDistance(Location(0, 0), Location(1, 0))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (111.19492664455873 +- epsilon)
  }

  test("greatCircleDistance test lon unit distance") {
    val epsilon = 1e-4
    val distance = greatCircleDistance(Location(0, 0), Location(0, 1))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (111.19492664455873 +- epsilon)
  }

  test("greatCircleDistance test unit distance") {
    val epsilon = 1e-4
    val distance = greatCircleDistance(Location(0, 0), Location(1, 1))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (157.24938127194397 +- epsilon)
  }

  test("greatCircleDistance test (45, 0) -> (0, 0)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(45, 0), Location(0, 0))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (5003.771699005144 +- epsilon)
  }

  test("greatCircleDistance test (90, -180) -> (0, 0)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(90, -180), Location(0, 0))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (10007.543398010286 +- epsilon)
  }

  test("greatCircleDistance test (-89, -180) -> (0, 0)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(-89, -180), Location(0, 0))
//    println(s"distance: $distance")
//    println("=========================================")
    distance should be (10118.738324654845 +- epsilon)
  }

  test("greatCircleDistance test (37.358, -78.438) -> (37.957, -78.439)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(37.358, -78.438), Location(0, 0))
    println(s"distance: $distance")
    println("=========================================")
    distance should be (66.77 +- epsilon)
  }

  test("greatCircleDistance test (38.35, -78.433) -> (37.957, -78.439)") {
    val epsilon = 1e-4

    val distance = greatCircleDistance(Location(-89, -180), Location(0, 0))
    println(s"distance: $distance")
    println("=========================================")
    distance should be (44.27 +- epsilon)
  }



  test("Predict temperature small data set 1 at (0, -45)") {
    assertResult(15)(predictTemperature(List((Location(45, -90), 10), (Location(-45, 0), 20)), Location(0, -45)))
  }

  test("Predict temperature small data set 2 at (0, 0)") {
    assertResult(10)(predictTemperature(List((Location(0, 0), 10)), Location(0, 0)))
  }

  test("Predict temperature small data set 3 at (0, 0)") {
    assertResult(52)(predictTemperature(List((Location(45, -90), 0), (Location(-45, 0), 59.028308521858634)), Location(0, 0)))
  }

//  test("Predict temperature small data set 4 at (37.957, -78.439)") {
////    assertResult(35.759237)(predictTemperature(List((Location(37.358, -78.438), 10), (Location(38.35, -78.433), 20)), Location(37.957, -78.439)))
//  }

  test("predictTemperature: some point closer") {
    val location1 = Location(1, 1)
    val temperature1 = 10d
    val location2 = Location(-10, -10)
    val temperature2 = 50d
    val list = List(
      (location1, temperature1),
      (location2, temperature2)
    )
    val result = Visualization.predictTemperature(list, Location(0, 0))
    println(s"predictTemperature: some point closer - result: $result")
    assert(temperature1 - result < temperature2 - result)
  }

//  test("Predict temperature yearly average") {
//    val epsilon = 1e-4
//
//    val temperature = predictTemperature(yearlyAverage, Location(18.433, -66.011))
//    temperature should be (26.53967 +- epsilon)
//    //    assertResult(26.539573820395745)(predictTemperature(yearlyAverage, Location(18.433,-66.011)))
//  }

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

//  test("Visualize image") {
//
//    val img = visualize(yearlyAverage, scale)
//    img.output(new File(s"target/$year-sample.png"))
//
//    assertResult(360 * 180)(img.pixels.length)
//  }

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



//  import LocationGens._
//
//  test("predicted 3 temperature at location z should be closer to known temperature at location x than to known temperature at " +
//    "location y, if z is closer (in distance) to x than y, and vice versa") {
//    val size = 100000
//    val locs = listOfN(size, locationTemp(35 to 37, 37 to 39, 0.495 to 0.5 by 0.001)).sample.get
//    val locs2 = listOfN(size, locationTemp(32 to 34, 34 to 36, 0.395 to 0.4 by 0.001)).sample.get
//    val points = listOfN(100, locations(32 to 37, 34 to 39)).sample.get
//
//
//    val avgLoc = (locs.map(l=>l._2).sum / size)
//    val avgLoc2 = (locs2.map(l=>l._2).sum / size)
//    locs.size should be >= size
//    locs2.size should be >= size
//    val avg = (avgLoc + avgLoc2) / 2
//
//    val predTemps = points.map(p=>(p,
//      Visualization.predictTemperature(locs ::: locs2, p),
//      Visualization.calculateDistance(p, Location(36, 38)),
//      Visualization.calculateDistance(p, Location(33, 35))
//    ))
//
//    predTemps.foreach{ predTemp=>
//      println(s"loc: ${predTemp._1}, temp: ${predTemp._2}, dist to 1: ${predTemp._3}, dist to 2: ${predTemp._4}, diff: ${predTemp._3 - predTemp._4} avg $avg")
//      if(predTemp._3 < predTemp._4) predTemp._2 should be > (avg - 1)
//      else predTemp._2 should be < avg + 1
//    }
//  }

}


//object LocationGens {
//  def locations(lat: Range, lonRange: Range): Gen[Location] = for {
//    lat <- chooseNum[Double](lat.start, lat.end)
//    lon <- chooseNum[Double](lonRange.start, lonRange.end)
//  } yield Location(lat, lon * lonRange.step)
//
//  def temps(l: Location, tempMod: NumericRange[Double]): Gen[Double] = {
//    val loc = math.abs(l.lon) + math.abs(l.lat)
//    for {
//      temp <- chooseNum(loc * tempMod.start, loc * tempMod.end)
//    } yield temp
//  }
//
//  def locationTemp(lat: Range, lon: Range, tempMod: NumericRange[Double]): Gen[(Location, Double)] = for {
//    loc <- locations(lat, lon)
//    temp <- temps(loc, tempMod)
//  } yield (loc, temp)
//
//}

