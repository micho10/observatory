package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("greatCircleDistance test (extreme case 1)") {
    val distance = Visualization.greatCircleDistance(Location(-12, 85), Location(12, -95))
    println(s"distance: $distance")
    assert(distance === math.Pi)
  }


}
