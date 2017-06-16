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
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = temperatures.find(_._1 == location) match {
//TODO Consider the special case of small distance
//  - calculate collection distances between the known points and the new location
//  - case Some (dist <= 1 km) => use temperature of the closest point
//  - case None                => idw()

    case Some(loc) => loc._2
    case None =>
      val weightedLocations = weightKnownLocations(temperatures, location, power = 3)(distance)
      idw(weightedLocations)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = points.find(_._1 == value) match {
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
      64440 -> (-180, -90) ... 64620 -> (0, -90) ... 64799 -> (179, -90)
     */
    def project(position: Int): Location = {
      val x = position % image_width
      val y = position / image_width

      Location(image_height / 2 - y, x - image_width / 2)
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
    * @param p Point
    * @param q Point
    * @return  The distance between both points
    */
  private def distance(p: Location, q: Location): Double = {
    val earth_radius = 6371

    val lat1 = toRadians(p.lat)
    val lat2 = toRadians(q.lat)
    val lonDistance = toRadians(p.lon - q.lon)

    val centralAngle = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDistance))

    earth_radius * centralAngle
  }

  /**
    * Spatial interpolation using the Inverse distance weighting algorithm
    *
    * @param weightedLocations  Tupla of known temperatures + weight
    * @return                   The interpolated value
    */
  private def idw(weightedLocations: Iterable[(Location, Double, Double)]): Double = {
    var (numerator, denominator) = (0.0, 0.0)

    for {
      wloc <- weightedLocations
    } yield (numerator += wloc._2 * wloc._3, denominator += wloc._2)

    numerator / denominator
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
    def lerp(p: Double, q: Double, x: Double): Double = (1 - x) * p + x * q
    def color(baseColor: Color, value: Double): Color =
      Color(
        round(baseColor.blue * value).toInt,
        round(baseColor.green * value).toInt,
        round(baseColor.red * value).toInt
      )

    (p, q) match {
      case (Some(p), Some(q))             =>
        val gradient = lerp(p._1, q._1, value)
        color(p._2, gradient)
      case (Some((pValue, pColor)), None) => pColor
      case (None, Some((qValue, qColor))) => qColor
      case _                              => Color(0, 0, 0)
    }
  }

}

//https://stackoverflow.com/questions/4353525/floating-point-linear-interpolation
//http://www.scala-notes.org/2010/08/a-generic-interpolate-method-using-type-classes/
//http://zuqqhi2.com/en/linear-interpolation

//private def interpolate(index: Int, x: T): T = {
//  /* Interpolate or extrapolate linearly between point number index-1 and index. */
//  assert(index > 0)
//
//  val x1 = X(index-1)
//  val x2 = X(index)
//  val y1 = Y(index-1)
//  val y2 = Y(index)
//  val f = implicitly[Field[T]]
//
//  // w = (x - x1) / (x2 - x1)
//  val w = f./(f.-(x, x1),
//  f.-(x2, x1))
//  // u = 1 - w
//  val u = f.-(f.one, w)
//
//  // result = y1 * u + y2 * w
//  f.+(f.*(y1, u),
//  f.*(y2, w))
//}
//}

