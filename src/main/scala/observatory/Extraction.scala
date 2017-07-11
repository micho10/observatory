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

  Logger.getLogger("org.apache.spark").setLevel(Level.ERROR)

  val spark: SparkSession = SparkSession
    .builder
    .appName("Observatory")
    .master("local[8]")
    .config("spark.master", "[*]")
    .config("spark.executor.memory", "1G")
    .getOrCreate()

  import spark.implicits._

  def stations(stationsFile: String): Dataset[Station] =
    spark
      .read
      .csv(resourcePath(stationsFile))    // csv() only works if the file contains a header??
      .select(
        concat_ws("-", coalesce('_c0, lit("")), '_c1).as("id"),
        '_c2.as("lat").cast(DoubleType),
        '_c3.as("lon").cast(DoubleType)
      )
      .where('_c2.isNotNull && '_c3.isNotNull)
      .as[Station]

  def tempReadings(temperaturesFile: String): Dataset[TempReading] =
    spark
      .read
      .csv(resourcePath(temperaturesFile))
      .select(
        concat_ws("-", coalesce('_c0, lit("")), '_c1).as("id"),
        '_c2.as("month").cast(IntegerType),
        '_c3.as("day").cast(IntegerType),
        (('_c4 - 32 ) / 1.8).as("temperature").cast(DoubleType)
      )
      .as[TempReading]

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    def stnTempReadings(year: Int, stations: Dataset[Station], tempReadings: Dataset[TempReading]): Dataset[LocatedTemperature] =
      tempReadings
        .join(stations, "id")
        .as[StnTempReading]
        .map(reading => (ReadingDate(year, reading.month, reading.day), Location(reading.lat, reading.lon), reading.temperature))
        .toDF("date", "location", "temperature")
        .as[LocatedTemperature]

    stnTempReadings(year, stations(stationsFile), tempReadings(temperaturesFile))
      .collect()
      .par
      .map(ld => (ld.date.toLocalDate, ld.location, ld.temperature))
      .seq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] =
    records
      .par
      .groupBy(_._2)
      .mapValues(record => record.foldLeft(0.0)(
        (a, r) => a + r._3 / record.size
      ))
      .seq


  private def resourcePath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

}
