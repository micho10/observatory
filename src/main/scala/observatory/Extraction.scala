package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{DoubleType, IntegerType}


/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession = SparkSession
    .builder()
    .appName("Observatory")
    .config("spark.master", "local")
    .getOrCreate()

  import spark.implicits._

  def stations(stationsFile: String): Dataset[Station] =
    spark
      .read
      .csv(resourcePath(stationsFile))
      .select(
        concat_ws("-", '_c0, '_c1).as("id"),
        '_c2.as("lat").cast(DoubleType),
        '_c3.as("long").cast(DoubleType)
      )
      .where('_c2.isNotNull && '_c3.isNotNull)
      .as[Station]

  def tempReadings(temperaturesFile: String): Dataset[TempReading] =
    spark
      .read
      .csv(resourcePath(temperaturesFile))
      .select(
        concat_ws("-", '_c0, '_c1).as("id"),
        '_c2.as("month").cast(IntegerType),
        '_c3.as("day").cast(IntegerType),
        (('_c4 - 32 ) / 1.8).as("temperature").cast(DoubleType)
      )
      .as[TempReading]

  def stnTempReadings(stations: Dataset[Station], tempReadings: Dataset[TempReading]): Dataset[StnTempReading] =
    tempReadings
      .join(stations, "id")
      .as[StnTempReading]


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val joined =
      stations(stationsFile)
        .join(tempReadings(temperaturesFile), "id")
//        .as[Joined]
        .map(j => (LocalDate.of(year, j.month, j.day), Location(j.lat, j.long), j.temperature))
        .toDF("date", "location", "temperature")
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

  def resourcePath(resource: String): String = Paths.get(resource).toUri.toString

}
