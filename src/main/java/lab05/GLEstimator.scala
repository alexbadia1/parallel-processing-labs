package lab05

import scala.math.pow

object GLEstimator extends App {
  /** Nano seconds in 1 seconds */
  val NANO_SECONDS_MULTIPLIER: Long = 1000000000L

  /** Gregory Leibniz series limit */
  val N: Long = 100000000L

  /**
   * Calculates pi using the Gregory-Leibniz series
   *
   * @param n - Gregory Leibniz series limit
   */
  def glEstimator(n: Long): Unit = {
    val start = System.nanoTime()
    val pi: BigDecimal = ((0L to n).foldLeft(0.0d)((sum, n) => {
      sum + (pow(-1, n) / (2 * n + 1))
    }) * 4).toFloat
    val end = System.nanoTime()
    val diff: Float = (end - start) * NANO_SECONDS_MULTIPLIER
    println(String.format("pi = %s\ndt = %s s", pi, formatSeconds(diff, 2)))
  }

  /**
   * Seems like Double and Float are immutable, so format methods don't
   * work as intended. Enforce precision via string parsing.
   */
  def formatSeconds(num: Float, precision: Int): String = {
    val tmp = num.toString
    var pos: Int = 0
    var found: Boolean = false
    while (pos < tmp.length && !found) {
      if (tmp.charAt(pos).equals('.')) {
        found = true
      }
      pos += 1
    }

    // Enforce precision
    if (pos + precision < tmp.length) {
      tmp.substring(0, pos + precision)
    }
    // Add extra 0's for precision need be
    else {
      tmp.substring(0, pos) + "0".repeat(precision)
    }
  }


  glEstimator(N)
}
