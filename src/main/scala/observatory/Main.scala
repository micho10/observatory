package observatory

import java.io.File
import java.time.LocalDate

object Main extends App {

  val year = 1975
  val stationsFile = "/stations.csv"


  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def extractData(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    def writeResults(year: Int, temps: Iterable[(LocalDate, Location, Double)]): Unit =
      printToFile(new File(s"output/located-$year.csv")) {p => temps.foreach(p.println)}

    println(s"Extracting located temperatures of $year")
    val temperatures = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
    println(s"Located [$year]: ${temperatures.size}")
    writeResults(year, temperatures)
    temperatures
  }

  def yearlyAverage(year: Int, records: Iterable[(LocalDate, Location, Double)]): Unit = {
    def writeResults(year: Int, temps: Iterable[(Location, Double)]): Unit =
      printToFile(new File(s"output/average-$year.csv")) {p => temps.foreach(p.println)}

    println("Calculating the yearly average temperatures")
    val yearlyAverageTemps = Extraction.locationYearlyAverageRecords(records)
    println(s"Averages [$year]: ${yearlyAverageTemps.size}")
    writeResults(year, yearlyAverageTemps)
  }


  println("######## Observatory started ########")

  // Extract localized temperatures
//  val locatedTemps = (1975 to 1976).map(year => extractData(year, stationsFile, s"/$year.csv")).head
//  (1982 to 1984).par.map(year => extractData(year, stationsFile, s"/$year.csv"))

  val temperatures = extractData(year, stationsFile, s"/$year.csv")

  // Calculate yearly average
  yearlyAverage(year, temperatures)

  println("######## Observatory finished ########")

}
