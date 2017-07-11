package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math.{pow, round}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  // Mean Earth radius in Km
  val earth_radius = 6372.8

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val predictions: Iterable[(Double, Double)] = distanceTemperatureCombi(temperatures, location)

    predictions.find(_._1 == 0) match {
      case Some((_, temp)) => temp
      case _               => idw(predictions, power = 3)
    }
  }

  def distanceTemperatureCombi(temperatures: Iterable[(Location, Double)], location: Location): Iterable[(Double, Double)] =
    temperatures.map {
      case (otherLocation, temperature) => (location.point haversineEarthDistance otherLocation.point, temperature)
    }


  /**
    * Spatial interpolation using the Inverse Distance Weighting algorithm
    *
    * @param temperatures tupla of distances and temperatures
    * @param power
    * @return             temperature interpolation
    */
  def idw(temperatures: Iterable[(Double, Double)], power: Int): Double = {
    val (weightedSum, inverseWeightedSum) = temperatures.aggregate((0.0, 0.0))(
      {
        case ((ws, iws), (distance, temperature)) =>
          val w = 1 / pow(distance, power)
          (w * temperature + ws, w + iws)
      }, {
        case ((wsA, iwsA), (wsB, iwsB)) => (wsA + wsB, iwsA + iwsB)
      }
    )

    weightedSum / inverseWeightedSum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color =
    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None             =>
        val (smaller, greater) = points.toList.sortBy(_._1).partition(_._1 < value)
        linearInterpolation(smaller.takeRight(1).headOption, greater.headOption, value)
    }

  /**
    * Approximates a color using linear interpolation between the colors of 2 known values
    *
    * @param p First known point
    * @param q Second known point
    * @return  The color inferred for the new value
    */
  private def linearInterpolation(p: Option[(Double, Color)], q: Option[(Double, Color)], value: Double): Color = {
    def lerp(min: Double, max: Double, x: Double): Double = (x - min) / (max - min)
    def color(minColor: Color, maxColor: Color, variation: Double): Color =
      Color(
        round(minColor.red   + (maxColor.red   - minColor.red)   * variation).toInt,
        round(minColor.green + (maxColor.green - minColor.green) * variation).toInt,
        round(minColor.blue  + (maxColor.blue  - minColor.blue)  * variation).toInt
      )

    (p, q) match {
      case (Some(p), Some(q))             =>
        val gradient = lerp(p._1, q._1, value)
        color(p._2, q._2, gradient)
      case (Some((pValue, pColor)), None) => pColor
      case (None, Some((qValue, qColor))) => qColor
      case _                              => Color(0, 0, 0)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val image_height = 180
    val image_width  = 360

    /*
          0 -> (-180, 90)  ...   180 -> (0, 90)  ...   359 -> (179, 90)
      ...
      32040 -> (-180, 0)   ... 32220 -> (0, 0)   ... 32399 -> (179, 0)
      ...
      64440 -> (-180, -89) ... 64620 -> (0, -89) ... 64799 -> (179, -89)
     */
    def project(position: Int): Location = {
      val x = position % image_width
      val y = position / image_width

      val (lon, lat) = toCoords(x, y)
      Location(lat, lon)
    }

    def createPixelMap(width: Int, height: Int): Seq[Pixel] =
      (0 until height * width).par.map{ position =>
        position -> interpolateColor(
          colors,
          predictTemperature(
            temperatures,
            project(position)
          )
        ).toPixel()
      }
        .seq
        .sortBy(_._1)
        .map(_._2)

    val pixels = createPixelMap(image_width, image_height)

    Image(image_width, image_height, pixels.toArray)
  }

  /************************************************
    * pos(x*y) => coords(x,y) => location(lat,lon)
    * *********************************************
    */
  def toCoords(x: Int, y: Int): (Int, Int) = (x % 360 - 180, 90 - y % 180)

}
