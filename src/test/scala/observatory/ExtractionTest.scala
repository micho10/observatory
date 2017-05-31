package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

//  test("With invalid inputs, the output sequence must be empty") {
//
//  }

  test("It can find the Temperatures file and extract the data") {
    val stations = Extraction.openStations("/stations.csv")

  }

}