package observatory

import java.io.InputStream
import java.time.LocalDate

import org.apache.spark.sql.SparkSession

import scala.io.Source


/**
  * 1st milestone: data extraction
  */
object Extraction {

  val spark: SparkSession = SparkSession
    .builder()
    .appName("Observatory")
    .config("spark.master", "local")
    .getOrCreate()

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    /* Transform F temperatures to C */
    def tempFtoC(temperature: Double): Double = (temperature - 32) / 1.8

    val stations = spark.read.csv(stationsFile)
//    stations.select("_c0", "_c1", "_c2", "_c3").where($"_c2".isNotNull && $"_c3".isNotNull).distinct.orderBy($"_c0", $"_c1").withColumnRenamed("_c0", "stnId").withColumnRenamed("_c1", "wbanId").withColumnRenamed("_c2", "lat").withColumnRenamed("_c3", "long").show

//    val stationStream: InputStream = getClass.getResourceAsStream(stationsFile)
//    val stations = Source.fromInputStream(stationStream).getLines()

    val tempReading = spark.read.csv(temperaturesFile)
//    val temperatureStream: InputStream = getClass.getResourceAsStream(temperaturesFile)
//    val temperatures = Source.fromInputStream(temperatureStream).getLines()

    List.empty
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

  def openStations(stationsFile: String): Iterable[(String)] = {
    val stationStream: InputStream = getClass.getResourceAsStream(stationsFile)
    val stations = Source.fromInputStream(stationStream).getLines()
    stations.toList
  }

  private case class Station(stnId: Int, wbanId: Int, lat: Double, lon: Double)

  private case class TempReading(stnId: Int, wbanId: Int, month: Int, day: Int, temperature: Double)

}
