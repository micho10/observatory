package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double =
    temperatures.find(_._1 == location) match {

      //TODO Consider the special case of small distance
      //  - calculate collection distances between the known points and the new location
      //  - case Some (dist <= 1 km) => use temperature of the closest point
      //  - case None                => idw()

      case Some(loc) => loc._2
      case None =>
        val weightedLocations = weightKnownLocations(temperatures, location, power = 3)(greatCircleDistance)
        idw(weightedLocations)
    }

//  {
//    val distanceThreshold = 1
//    val power = 2.5
//
//    temperatures.find(_._1 == location) match {
//      case Some(loc) => loc._2
//      case None =>
//        val weights = temperatures.par.map {
//          case (loc, _) =>
//            val distance = greatCircleDistance(loc, location)
//            if (distance < distanceThreshold) 1 else 1 / pow(distance, power)
//        }.toList
//
//        idw(temperatures, weights)
//    }
//
//  }

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
//      Location(image_height / 2 - y, x - image_width / 2)
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

  /**
    * Δσ = arccos( sin φ1 * sin φ2 + cos φ1 * cos φ2 * cos (Δλ))
    *
    * Source: http://www.movable-type.co.uk/scripts/latlong.html
    * a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
    *
    * @param p Point
    * @param q Point
    * @return  The distance between both points
    */
  def greatCircleDistance(p: Location, q: Location): Double = {
    val earth_radius = 6371

//    val lat1 = toRadians(p.lat)
//    val lat2 = toRadians(q.lat)
//    println(s"lat1: $lat1 | lat2: $lat2")
//
//    val lonDistance = toRadians(p.lon - q.lon)
//    println(s"lonDistance: $lonDistance")
//
//    val arg = sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDistance)
//    println(s"arg: $arg")
//
//    val centralAngle = acos(if (arg > 1) 1 else if (arg < -1) -1 else arg)
//    println(s"centralAngle: $centralAngle")
//
//    earth_radius * centralAngle
    val lat1 = p.lat.toRadians
    val lat2 = q.lat.toRadians
//    println(s"lat1: $lat1 | lat2: $lat2")

    val latDistance = abs(p.lat - q.lat).toRadians
    val lonDistance = abs(p.lon - q.lon).toRadians
//    println(s"lonDistance: $lonDistance | latDistance: $latDistance")

    val a = pow(sin(latDistance / 2), 2) + cos(lat1) * cos(lat2) * pow(sin(lonDistance / 2), 2)
    val fixedA = if (a > 1) 1 else if (a < -1) -1 else a
//    println(s"a: $a")

    val c = 2 * atan2(sqrt(fixedA), sqrt(1 - fixedA))
//    println(s"c: $c")

    earth_radius * c
  }

//  /**
//    * Spatial interpolation using the Inverse Distance Weighting algorithm
//    *
//    * @param weightedLocations  Tupla of known temperatures + weight
//    * @return                   The interpolated value
//    */
//  private def idw(temperatures: Iterable[(Location, Double)], weights: Iterable[Double]): Double = {
  private def idw(weightedLocations: Iterable[(Location, Double, Double)]): Double = {
    var (numerator, denominator) = (0.0, 0.0)

    for {
      wloc <- weightedLocations
    } yield (numerator += wloc._2 * wloc._3, denominator += wloc._2)

    numerator / denominator

//    val weightsSum = weights.sum
//
//    val weightedTemperatures = temperatures.par.unzip._2.zip(weights).map { case (a, b) => a * b }.sum
//
//    weightsSum / weightedTemperatures
  }

  /**
    * Calculate the weights of the known locations
    *
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @param power        Power parameter, a positive real number
    * @param distance     Function to calculate the distance between 2 points
    * @return             Tupla of known temperatures + weight
    */
  private def weightKnownLocations(temperatures: Iterable[(Location, Double)], location: Location, power: Double)
                                  (distance: (Location, Location) => Double): Iterable[(Location, Double, Double)] = {
    def weight(newLocation: Location, known: Location): Double = 1 / pow(distance(known, location), power)
    assert(power > 0)

    temperatures.map(temp => (temp._1, temp._2, weight(location, temp._1)))
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

  /************************************************
    * pos(x*y) => coords(x,y) => location(lat,lon)
    * *********************************************
    */
  def toCoords(x: Int, y: Int): (Int, Int) = (x % 360 - 180, 90 - y % 180)

}

