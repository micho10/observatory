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
    ???
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

    val latDistance = toRadians(p.lat - q.lat)
    val lonDistance = toRadians(p.lon - q.lon)

    earth_radius
  }


  private def idw = ???

}

