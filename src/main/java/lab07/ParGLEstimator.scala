package lab07

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.math.pow

object ParGLEstimator extends App {
  /** Nano seconds in 1 seconds */
  val NANO_SECONDS_MULTIPLIER: Long = 1000000000L

  /**
   * Calculates pi using the Gregory-Leibniz series
   *
   * @param seriesSize - Gregory Leibniz series limit
   * @param range      - subset range of Gregory Leibniz series
   */
  def parGLEstimator(seriesSize: Long, range: Int): (Double, Long, Long) = {

    // Calculate partitions of GL series to run in parallel
    val numPartitions: Int = Math.ceil(seriesSize / range).toInt
    val ranges = for (k <- 0 to numPartitions) yield {
      val lower: Int = k * range + 1
      val upper: Int = Int.MaxValue min (k + 1) * range
      (lower, upper)
    }

    // Run partitions of GL series in parallel
    val parallelStart = System.nanoTime()
    val partials = ranges.par.map { partial =>
      // Sum over lower and upper per Eq. 1
      val (lower, upper) = partial
      val s = System.nanoTime()
      val partialPi = (lower to upper).foldLeft(0.0d)((sum, n) => {
        sum + (pow(-1, n) / (2 * n + 1))
      }) * 4
      val partialTime = System.nanoTime() - s

      // Return 2-tuple of partial pi sum & partial elapsed time
      (partialPi, partialTime)
    }

    // Summation of partitions of GL series for PI
    val pi = partials.foldLeft(4.0d)((sum, pair) => {
      val (pi, time) = pair
      sum + pi
    })
    val parallelEnd = System.nanoTime()

    // To find the serial runtime, sum all parallel times
    val serialTime = partials.foldLeft(0L)((sum, pair) => {
      var (pi, time) = pair
      sum + time
    })

    (pi, serialTime, parallelEnd - parallelStart)
  } // parGLEstimator

  /**
   * Amdahl Calculations
   *
   * @param t1 - time on one processor
   * @param tn - time on n processors
   */
  def amdahlCalc(pi: Double, t1: Long, tn: Long): Unit = {
    val numProcessors = Runtime.getRuntime.availableProcessors
    val t1Seconds: Float = t1 / NANO_SECONDS_MULTIPLIER
    val tnSeconds: Float = tn / NANO_SECONDS_MULTIPLIER
    val r: Float = t1 / tn
    val e: Float = r / numProcessors * 100
    println("pi = " + pi)
    println("T1 = " + format(t1Seconds, 2))
    println("TN = " + format(tnSeconds, 2))
    println("N = " + numProcessors)
    println("R (T1 / TN) = " + format(r, 2))
    println("E (R / N) = " + format(e, 2) + "%")
  }

  /**
   * Seems like Double and Float are immutable, so format methods don't
   * work as intended. Enforce precision via string parsing.
   *
   * @param num       - number to format
   * @param precision - number of decimal places to enforce
   * @return formatted number as string
   */
  def format(num: Float, precision: Int): String = {
    val tmp: String = num.toString
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

  val (pi, t1, tn) = parGLEstimator(100000000L, 100)
  amdahlCalc(pi, t1, tn)
}
