
import java.util.IllegalFormatWidthException

import common._

import scala.collection.immutable.Seq

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def pointOf(width: Int)(index: Int): (Int, Int) = {
    val x = index % width
    val y = index / width
    (x, y)
  }

  def indexOf(width: Int)(point: (Int, Int)): Int = {
    val (x, y) = point
    width * y + x
  }

  def pointInRadius(point0: (Int, Int), radius: Int)(point: (Int, Int)): Boolean = {
    val (x0, y0) = point0
    val (x, y) = point
    if (x <= x0 + radius && x >= x0 - radius && y <= y0 + radius && y >= y0 - radius) true
    else false
  }

  def pointsAroundOf(width: Int, height: Int)(point0: (Int, Int))(radius: Int): Seq[(Int, Int)] = {
    val (x0, y0) = point0
    val min = 0
    val max = width * height - 1
    val start = indexOf(width)(x0 - radius, y0 - radius)
    val end = indexOf(width)(x0 + radius, y0 + radius)
    val indexesInRadius = for {
      i <- start to end
      if (pointInRadius(point0, radius)(pointOf(width)(i)))
    } yield clamp(i, min, max)
    indexesInRadius.distinct.map(pointOf(width)(_))
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val pointsAroundPixel = pointsAroundOf(src.width, src.height)((x, y))(radius)
    val total = pointsAroundPixel.size
    val (r, g ,b ,a) = pointsAroundPixel.map(p => src(p._1, p._2)).map(c => (red(c), green(c), blue(c), alpha(c))).fold((0,0,0,0))((p, acc) => {
      val (r0, g0, b0, a0) = acc
      val (r, g, b, a) = p
      (r0 + r, g0 + g, b0 + b, a0 + a)
    })
    rgba(r / total, g / total, b / total, a / total)
  }

}
