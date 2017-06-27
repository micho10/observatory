package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import observatory.Visualization._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("greatCircleDistance test zero distance") {
    val distance = greatCircleDistance(Location(-12, 85), Location(-12, 85))
//    println(s"distance: $distance")
//    println("=========================================")
    assert(distance === 0)
  }

  test("greatCircleDistance test (extreme case 1)") {
    val distance = greatCircleDistance(Location(90,-180), Location(12, -95))
//    println(s"distance: $distance")
//    println("=========================================")
    assert(distance === 10010)
  }

//  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color

  test("color interpolation sorted") {
    val scale = List((0.0, Color(0, 0, 255)))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, -0.5))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, 0.5))
    assertResult(Color(0, 0, 255))(interpolateColor(scale, 0.0))
  }

  test("Color interpolation unsorted") {
    // UNSORTED SCALE!!!
    val scale =
      List(
        ( 60.0, Color(255, 255, 255)), ( 32.0, Color(255,   0,   0)),
        ( 12.0, Color(255, 255,   0)), (  0.0, Color(  0, 255, 255)),
        (-15.0, Color(  0,   0, 255)), (-27.0, Color(255,   0, 255)),
        (-50.0, Color( 33,   0, 107)), (-60.0, Color(  0,   0,   0)))
    assertResult(Color(255, 255,   0))(interpolateColor(scale, 12.0))
    assertResult(Color(255, 255, 255))(interpolateColor(scale, 62))
    assertResult(Color(128, 255, 128))(interpolateColor(scale,  6))
  }

}
