package observatory

import java.io.File
import java.time.LocalDate


object Main extends App {

  val year = 1975
  val stationsFile = "/stations.csv"

  private val colors = Seq(
    (60.0,  Color(255, 255, 255)),
    (32.0,  Color(255,   0,   0)),
    (12.0,  Color(255, 255,   0)),
    (0.0,   Color(  0, 255, 255)),
    (-15.0, Color(  0,   0, 255)),
    (-27.0, Color(255,   0, 255)),
    (-50.0, Color( 33,   0, 107)),
    (-60.0, Color(  0,   0,   0))
  )


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

  def yearlyAverage(year: Int, records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    def writeResults(year: Int, temps: Iterable[(Location, Double)]): Unit =
      printToFile(new File(s"output/average-$year.csv")) {p => temps.foreach(p.println)}

    println("Calculating the yearly average temperatures")
    val yearlyAverageTemps = Extraction.locationYearlyAverageRecords(records)
    println(s"Averages [$year]: ${yearlyAverageTemps.size}")
    writeResults(year, yearlyAverageTemps)
    yearlyAverageTemps
  }

//  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Unit = {
//    def writeResults(year: Int, temps: Iterable[(Location, Double)]): Unit =
//      printToFile(new File(s"output/prediction-$year.csv")) {p => temps.foreach(p.println)}
//
//    println("Predicting temperatures at (0, 0)")
//    val prediction =
//
//  }

  def visualization(year: Int, temperatures: Iterable[(Location, Double)]): Unit = {
    def writeResults(year: Int, temps: Iterable[(Location, Double)]): Unit =
      printToFile(new File(s"output/average-$year.csv")) {p => temps.foreach(p.println)}

    println("Visualizing temperature predictions")
    val image = Visualization.visualize(temperatures, colors)
    image.output(new File("output/some-image.png"))
    println("Created temperatures image")
  }


  println("######## Observatory started ########")

  // Extract localized temperatures
//  val locatedTemps = (1975 to 1976).map(year => extractData(year, stationsFile, s"/$year.csv")).head
//  (1982 to 1984).par.map(year => extractData(year, stationsFile, s"/$year.csv"))

  // Extract temperature data
  val temperatures = extractData(year, stationsFile, s"/$year.csv")

  // Calculate yearly average
  val averageTemps = yearlyAverage(year, temperatures)

  // Create image of temperatures
//  val averageTemps = yearlyAverage(year, temperatures)
  visualization(year, averageTemps)

  println("######## Observatory finished ########")

}
