package esc.sim.bicubic
import scala.util.Random

/**
 * Utilities for verifying Q3.12 bicubic calculations.
 */
object BicubicUtil {
  def randomSamples(): Array[Int] = {
    val samples = new Array[Int](4)
    for (i <- 0 to 3) {
      samples(i) = Random.nextInt(0xFFF + 0xFF) - 0x7F
    }
    samples
  }

  def randomWeights(): Array[Int] = {
    val weights = new Array[Int](4)
    for (i <- 0 to 3) {
      weights(i) = Random.nextInt(0xFFF << 1) - 0xFFF
    }
    weights
  }

  def randomDelta(): Int = Random.nextInt(0xFFF)

  def cubicWeights(samples: Array[Int]): Array[Int] = {
    val s = samples
    val weights = new Array[Int](4)
    weights(0) = (s(1) - s(2)) * 3 + s(3) - s(0)
    weights(1) = 2 * s(0) - 5 * s(1) + 4 * s(2) - s(3)
    weights(2) = s(2) - s(0)
    weights(3) = s(1)
    weights
  }

  def cubicInterpolate(weights: Array[Int], delta: Int): Int = {
    val w = weights
    var result = ((w(0) * delta) >> 12) + w(1)
    result = ((result * delta) >> 12) + w(2)
    result >>= 1
    result = ((result * delta) >> 12) + w(3)
    result
  }

  def calculateCubic(samples: Array[Int], delta: Int): Int = {
    cubicInterpolate(cubicWeights(samples), delta)
  }
}
