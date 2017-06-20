package observatory

import scala.math.round
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
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
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
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

    // Transforms the tiles' coordinates into (lat, lon) location coordinates
    def toLocationsMap: Map[Int, Location] = (
      for {
        i <- 0 until image_width
        j <- 0 until image_height
      } yield i + j * image_width -> Tile(x + i, y + j, zoom).toLocation
      ).toMap

    def makeCornersGrid(width: Int, height: Int, temperatures: Map[Int, Double]): (Int, Int) => Double = {
      val corners: Map[(Int, Int), Double] = Map(
        (0, 0) -> temperatures.head._2,
        (0, 1) -> temperatures(width),
        (1, 0) -> temperatures(height * y),
        (1, 1) -> temperatures.last._2
      )

      (row, col) => corners(row, col)
    }

    def toPixelMap(height: Int, width: Int): IndexedSeq[Pixel] = ???
//      for {
//      i <- 0 until width
//      j <- 0 until height
//    } yield {
//      val location = Tile(x + i, y + j, zoom).toLocation
//      val temperature = grid(location.lat, location.lon)
//      Visualization.interpolateColor(
//        colors,
//        bilinearInterpolation(x + i, y + i, x, x + width, y, y + width)
//      ).toPixel(alpha = 127)
//    }


    val locations = toLocationsMap

    // Transforms the locations map into a temperature map
    val temperatures = locations.mapValues(location => grid(round(location.lat).toInt, round(location.lon).toInt))

    val d = makeCornersGrid(image_width, image_height, temperatures)

    val pixels = toPixelMap(image_height, image_width)

    Image(image_width, image_height, pixels.toArray)
  }

//    def createPixelMap(width: Int, height: Int, x: Int, y: Int): Seq[Pixel] = {
//      val location = Tile(x, y, zoom).toLocation
//      val temperature = grid(location.lat, location.lon)
//    }

//      (0 until height * width).par.map{ position =>
//        position -> interpolateColor(
//          colors,
//          predictTemperature(
//            temperatures,
//            project(position)
//          )
//        ).toPixel()
//      }
//        .seq
//        .sortBy(_._1)
//        .map(_._2)

//    for {
//      i <- 0 until image_width
//      j <- 0 until image_height
//    } yield {
//      val temp = Tile(x + i, y + j, zoom).toLocation
//    }

//    def interpolateColor(colors: Iterable[(Double, Color)], x: Int, y: Int): Color = ???

    /*
     val temp = grid(tile(x, y, zoom).toLocation)
     */

    /*
    create temperature map
    temperature => color map (pixels)
    Image(w, h, color)
     */
//    val pixels = createPixelMap(image_width, image_height, x, y)
//
//    Image(image_width, image_height, pixels.toArray)

}


/*
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
 */
