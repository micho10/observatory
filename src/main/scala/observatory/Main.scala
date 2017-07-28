package observatory

import java.io.{File, FileOutputStream}
import java.time.LocalDate

import scala.io.StdIn._


object Main extends App {

  val year = 1976
  val stationsFile = "/stations.csv"

  private val palette = Seq(
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

//  def writeToFile(f: java.io.File)(op: java.io.FileOutputStream => Unit): Unit = {
//    val p = new FileOutputStream(f)
//    try { op(p) } finally { p.close() }
//  }

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
    val image = Visualization.visualize(temperatures, palette)
    image.output(new File(s"output/visual-$year.png"))
    println("Created temperatures image")
  }

  def saveImage(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]): Unit = {
    println(s"creating => target/temperatures/$year/$zoom/$x-$y.png")
    val image = Interaction.tile(data, palette, zoom, x, y)
    image.output(new java.io.File(s"target/temperatures/$year/$zoom/$x-$y.png"))
    println("Created tile image")
  }

//  val data: Set[(Int, Iterable[(Location, Double)])] = Set((1975, locateAverage))
//  Interaction.generateTiles(data, saveImage)



println("######## Observatory started ########")

//  // readLine: lets you prompt the user and also read their command line input
//  val name = readLine("Year? ")
//
//  // readInt: read a simple Int
//  print("Year (1975 - 2015)? ")
//  val year = readInt()

//  // readInt: read a simple Int
//  print("Zoom (0 - 3)? ")
//  val zoom = readInt()


  // Extract localized temperatures
//  val locatedTemps = (1975 to 1976).map(year => extractData(year, stationsFile, s"/$year.csv")).head
//  (1982 to 1984).par.map(year => extractData(year, stationsFile, s"/$year.csv"))

  println("### Extracting data ...")

  // Extract temperature data
  val temperatures = extractData(year, stationsFile, s"/$year.csv")

  println("### Calculating yearly average ...")

  // Calculate yearly average
  val averageTemps = yearlyAverage(year, temperatures)

  println("### Visualizing ...")

  // Create image of temperatures
  visualization(year, averageTemps)

  println("### Generating tiles ...")

  Interaction.generateTiles(Seq((year, averageTemps)), saveImage)

  println("######## Observatory finished ########")

}

