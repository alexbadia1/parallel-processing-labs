package lab06

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.math.pow

object FuturesGLEstimator extends App {
  /** Nano seconds in 1 seconds */
  val NANO_SECONDS_MULTIPLIER: Long = 1000000000L

  /**
   * Calculates pi using the Gregory-Leibniz series
   *
   * @param max - Gregory Leibniz series limit
   */
  def futuresGLEstimator(max: Long, range: Int): Unit = {
    val numPartitions: Int = Math.ceil(max / range).toInt

    // Parallelize this object using futures
    val start: Long = System.nanoTime()
    val futures = for (k <- 0 to numPartitions) yield Future {
      val lower: Int = k * range + 1
      val upper: Int = Int.MaxValue min (k + 1) * range
      glEstimator(lower, upper)
    }

    // Wait for all the futures using foldLeft over futures as follows
    val pi: Double = futures.foldLeft(4.0d) { (sum, future) =>
      import scala.concurrent.duration._
      val partial = Await.result(future, Duration.Inf)
      sum + partial
    }
    val end: Long = System.nanoTime()
    amdahlCalc(pi, UniProcessorGLEstimator(max), end - start)
  }

  /**
   * Calculates a subset sum of the Gregory-Leibniz series
   *
   * @param lower - base of subset Gregory Leibniz series
   * @param upper - limit of Gregory Leibniz series limit
   */
  def glEstimator(lower: Long, upper: Long): Float = {
    ((lower to upper).foldLeft(0.0d)((sum, n) => {
      sum + (pow(-1, n) / (2 * n + 1))
    }) * 4).toFloat
  }

  /**
   * Calculates pi using the Gregory-Leibniz series
   *
   * @param n - Gregory Leibniz series limit
   */
  def UniProcessorGLEstimator(n: Long): Long = {
    val start = System.nanoTime()
    val pi: BigDecimal = ((0L to n).foldLeft(4.0d)((sum, n) => {
      sum + (pow(-1, n) / (2 * n + 1))
    }) * 4).toFloat
    System.nanoTime() - start
  }

  /**
   * Amdahl Calculations
   *
   * @param t1 - time on one processor
   * @param tn - time on n processors
   */
  def amdahlCalc(pi: Double, t1: Long, tn: Long): Unit = {
    val t1Seconds = t1 * NANO_SECONDS_MULTIPLIER
    val tnSeconds = tn * NANO_SECONDS_MULTIPLIER
    val n: Int = Runtime.getRuntime.availableProcessors
    val r: Float = t1.toFloat / tn.toFloat
    val e: Float = r / n * 100
    println("pi = " + pi)
    println("T1 = " + format(t1Seconds, 2))
    println("TN = " + format(tnSeconds, 2))
    println("N = " + n)
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

  futuresGLEstimator(100000000L, 100)
}
