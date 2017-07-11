package observatory

import com.sksamuel.scrimage.Image
import observatory.Visualization.interpolateColor

import scala.math._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int
                   ): Image = {
    // Image precision
    val image_height = 256
    val image_width = 256

    def splitTile: IndexedSeq[(Int, Tile)] =
      for {
        i <- Range(0, image_width)
        j <- Range(0, image_height)
      } yield i + j * image_width -> Tile(x + i, y + j, zoom)

    val pixels = splitTile.par.map {
      case (position, tile) =>
        val location = tile.toLocation

        val d00 = grid(floor(location.lat).toInt, floor(location.lon).toInt)
        val d01 = grid(floor(location.lat).toInt, ceil(location.lon).toInt)
        val d10 = grid(ceil(location.lat).toInt, floor(location.lon).toInt)
        val d11 = grid(ceil(location.lat).toInt, ceil(location.lon).toInt)

        val xCoord = location.lon - floor(location.lat).toInt
        val yCoord = ceil(location.lat).toInt - location.lat

        position -> interpolateColor(
          colors,
          bilinearInterpolation(xCoord, yCoord, d00, d01, d10, d11)
        ).toPixel(127)
    }
      .seq
      .sortBy(_._1)
      .map(_._2)

    Image(image_width, image_height, pixels.toArray)
  }

}
