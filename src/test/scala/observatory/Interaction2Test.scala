package observatory

import observatory.LayerName.Temperatures
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Interaction2Test extends FunSuite with Checkers {

  test("slider must be within bounds range") {
    val layer = Layer(Temperatures, Seq.empty, 1975 to 2015)
    assertResult(2000)(Interaction2.yearSelection(Signal(layer), Signal(2000))())
    assertResult(1975)(Interaction2.yearSelection(Signal(layer), Signal(1975))())
    assertResult(1975)(Interaction2.yearSelection(Signal(layer), Signal(1914))())
    assertResult(2015)(Interaction2.yearSelection(Signal(layer), Signal(2015))())
    assertResult(2015)(Interaction2.yearSelection(Signal(layer), Signal(2045))())
  }


}
