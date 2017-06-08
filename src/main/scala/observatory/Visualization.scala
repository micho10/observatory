package observatory

import com.sksamuel.scrimage.Image

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
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val weightedLocations = weightKnownLocations(temperatures, location, power = 2)(distance)
    idw(weightedLocations)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }


  /**
    * Δσ = arccos( sin φ1 * sin φ2 + cos φ1 * cos φ2 * cos (Δλ))
    *
    * @param p points
    * @param q point
    * @return  the distance between both points
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
    * Inverse distance weighting
    *
    * @return         the interpolated value
    */
  private def idw(weightedLocations: Iterable[(Location, Double, Double)]): Double = {
    var numerator = 0.0
    var denominator = 0.0


//    def calculateIDW(temps: Iterable[(Location, Double)], numerator: Double, denominator: Double): Double = {
//      if (temps.isEmpty) numerator / denominator
//      else {
//        val known = temps.head
//        val dist = distance(known._1, location)
//        dist match {
//          case 0 => calculateIDW(temps.drop(1), numerator + )
//          case _ => ???
//        }
//      }
//    }

//    calculateIDW(temperatures, 0.0, 0.0)
    numerator / denominator
  }

  /**
    *
    * @param temperatures
    * @param location
    * @param power        power parameter, a positive real number
    * @param distance     function to calculate the distance between 2 points
    * @return
    */
  private def weightKnownLocations(temperatures: Iterable[(Location, Double)], location: Location, power: Double)
                                  (distance: (Location, Location) => Double): Iterable[(Location, Double, Double)] = {
    def weight(newLocation: Location, known: Location): Double = {
      val dist = distance(known, location)
      dist match {
        case 0 => 0
        case _ => 1 / pow(dist, power)
      }
    }

    assert(power > 0)
    temperatures.map(temp => (temp._1, temp._2, weight(location, temp._1)))
  }

}

