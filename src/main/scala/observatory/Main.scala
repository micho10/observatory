package observatory

import java.io.File
import java.time.LocalDate

object Main extends App {

  val stationsFile = "/stations.csv"


  def extractData(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    println(s"Extracting located temperatures of $year")
    val temperatures = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
    println(s"Located [$year]: ${temperatures.size}")
    writeResults(year, temperatures)
    temperatures
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def writeResults(year: Int, temps: Iterable[(LocalDate, Location, Double)]): Unit =
    printToFile(new File(s"output/located-$year.csv")) {p => temps.foreach(p.println)}


  println("######## Observatory started ########")

  // Extract localized temperatures
//  val locatedTemps = (1975 to 1976).map(year => extractData(year, stationsFile, s"/$year.csv")).head
  (1975 to 1985).par.map(year => extractData(year, stationsFile, s"/$year.csv"))

//  // Calculate yearly average
//  println("Calculating the yearly average temperatures")
//  val yearlyAverageTemps = Extraction.locationYearlyAverageRecords(temperatures)
//  println(s"Averages: ${yearlyAverageTemps.size}")


  println("######## Observatory finished ########")

}
